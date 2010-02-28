;;; mentor.el --- My Emacs kNows TORrents!  Control rtorrent from emacs

;; Copyright (C) 2010, Stefan Kangas

;; Maintainer: Stefan Kangas
;; Version: 0.0.1
;; Keywords: bittorrent, rtorrent

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code plugs the bittorrent client rtorrent into Emacs.  It is
;; accomplished using XML-RPC, and a Python script plugging the requests over
;; SCGI in order not to need a webserver locally.  The current goal is to
;; provide enough features in order to never have to touch the standard ncurses
;; interface ever again.

;; TODO:
;; Implement SCGI in Emacs Lisp
;; Support for XML-RPC over HTTP
;; Support for categories

;; Bug reports, comments, and suggestions are welcome!

;;; Change Log:

;; 0.1.0 First public release

;;; Code:

(require 'xml-rpc)


;;; configuration

(defgroup mentor nil
  "Controlling rtorrent from Emacs."
  :prefix "mentor-"
  :group 'tools)

(defcustom mentor-scgi-url "scgi://localhost:5000"
  "The scgi URL, as specified in rtorrent config var scgi_port"
  :group 'mentor
  :type 'string)


;;; major mode

(defvar mentor-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "g") 'mentor-update)
    (define-key map (kbd "TAB") 'mentor-toggle-object)
    (define-key map (kbd "d") 'mentor-stop-torrent)
    (define-key map (kbd "k") 'mentor-kill-torrent)
    (define-key map (kbd "K") 'mentor-kill-torrent-and-remove-data)
    (define-key map (kbd "n") 'mentor-next)
    (define-key map (kbd "p") 'mentor-prev)
    (define-key map (kbd "s") 'mentor-sort)
    (define-key map (kbd "Q") 'mentor-shutdown-rtorrent)
    map))

(defun mentor-mode ()
  "Major mode for controlling rtorrent from emacs

\\{mentor-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'mentor-mode
        mode-name "mentor"
        truncate-lines t)
  (use-local-map mentor-mode-map)
  (run-mode-hooks 'mentor-mode-hook))

(defun mentor-init ()
  (interactive)
  (if (mentor-not-connectable-p)
      (message "Unable to connect")
    (progn (switch-to-buffer (get-buffer-create "*mentor*"))
           (mentor-mode)
           (mentor-update))))

(defun mentor-not-connectable-p ()
  ;; TODO
  nil)



;;; Run XML-RPC calls

(defun mentor-command (&rest args)
  "Run command as an XML-RPC call via SCGI."
  ;; (when (not (listp args))
  ;;   (setq args (list args)))
  (xml-rpc-xml-to-response
   (with-temp-buffer
     (apply 'call-process
            "/home/skangas/.emacs.d/lisp-personal/mentor/bin/xmlrpc2scgi.py"
            nil t nil (apply 'append `(,mentor-scgi-url) args))
     ;; (xml-rpc-value-to-xml-list
     (xml-rpc-request-process-buffer (current-buffer)))))

(defun mentor-command-multi (&rest args)
  (mentor-command (apply 'append '("d.multicall" "default") args)))

;; Needed to work around buggy expressions in rtorrent
(defvar mentor-method-exclusions-regexp "d\\.get_\\(mode\\|custom.*\\|bitfield\\)")

(defun mentor-rpc-system-listmethods (&optional regexp)
  "system.listMethods \
Returns a list of all available commands.  First argument is \
interpreted as a regexp, and if specified only returns matching \
functions"
  (let ((methods (mentor-command '("system.listMethods")))
        (retval '()))
    (when regexp
      (mapc (lambda (cur)
              (when (and (string-match regexp cur)
                         (not (string-match mentor-method-exclusions-regexp cur)))
                (setq retval (cons cur retval))))
            methods))
    retval))


;; Main view

(defun mentor-sort ()
  ;; TODO sort lines according to various criteria
  (interactive)
  (sort-lines nil (point-min) (point-max)))

(defun mentor-update ()
  "Update mentor view"
  (interactive)
  (message "Updating torrent list...")
  (mentor-update-torrent-list)
  (erase-buffer)
  (mentor-insert-torrents)
  (message "Updating torrent list... DONE"))

(defun mentor-insert-torrents ()
  (maphash
   (lambda (id torrent)
     (mentor-insert-torrent id torrent))
   mentor-torrent-hash))

(defvar mentor-format-collapsed-torrent '("%s %s" "state" "name"))
(setq mentor-format-collapsed-torrent '("%s %-70s [%4s]"
                                        mentor-torrent-status
                                        mentor-torrent-name
                                        mentor-torrent-progress))
;;   "The format of a collapsed torrent, as a list.

;; The first string in the listhas the same syntax as format.

;; The rest of the list is interpreted literally unless they are strings padded with % \
;; on either side.  If so, they are taken as info fields to be processed for every torrent.")

(defun mentor-insert-torrent (id torrent)
  (insert
   (concat
   (propertize (mentor-process-format-string
                mentor-format-collapsed-torrent
                torrent)
               'torrent-id id
               'collapsed t)
   "\n")))

(defun mentor-process-format-string (format-list torrent)
  (let ((re mentor-regexp-information-fields))
    (apply 'format
           (car format-list)
           (mapcar (lambda (cur)
                     (cond ((functionp cur)
                            (funcall cur torrent))
                           ((stringp cur)
                            (if (string-match re cur)
                                (or (mentor-get-field
                                     (substring cur
                                                (+ (match-beginning 0) 0)
                                                (- (match-end 0) 0))
                                     torrent)
                                    "")
                              cur))
                           (t "")))
                   (cdr format-list)))))

(defun mentor-torrent-at-point ()
  (get-text-property (point) 'torrent-id))

(defun mentor-next ()
  (interactive)
  (let ((from (mentor-torrent-at-point)))
    (while (and (equal from (mentor-torrent-at-point))
                (not (save-excursion (end-of-line)
                                     (= (point) (point-max)))))
      (next-line))))

(defun mentor-prev ()
  (interactive)
  (let ((from (mentor-torrent-at-point)))
    (while (and (equal from (mentor-torrent-at-point))
                (not (save-excursion (end-of-line)
                                     (= (point) (point-max)))))
      (previous-line))))

(defun mentor-toggle-object ()
  (interactive)
  (let ((id (get-text-property (point) 'torrent-id)))
    (when id
      (let ((torrent (mentor-get-torrent id)))
        (message (mentor-get-field "connection_current" torrent))))))


;; Torrent actions

(defun mentor-kill-torrent ()
  (interactive)
  (message "TODO"))

(defun mentor-stop-torrent ()
  (interactive)
  (message "TODO"))


;;; Torrents

(defvar mentor-torrent-hash nil)
(make-variable-buffer-local 'mentor-torrent-hash)

(defun mentor-update-torrent-list ()
  "Update torrent information list"
  (when (not mentor-torrent-hash)
    (setq mentor-torrent-hash (make-hash-table :test 'equal)))
  (let* ((methods (mentor-rpc-system-listmethods "^d\\.\\(get\\|is\\)"))
         (tor-list (mentor-command-multi (mapcar
                                          (lambda (x) (concat x "="))
                                          methods)))
         (attributes (mapcar
                      (lambda (name)
                        (replace-regexp-in-string "^d\\.\\(get_\\)?" "" name))
                      methods))
         (torrents (mapcar
                    (lambda (torrent)
                      (mentor-join-lists attributes torrent))
                    tor-list)))
    (mapc
     (lambda (torrent)
       (let ((id (mentor-get-field "local_id" torrent)))
         (setq torrent (assq-delete-all id torrent))
         (puthash id torrent mentor-torrent-hash)))
     torrents)
    (when (not mentor-regexp-information-fields)
      (setq mentor-regexp-information-fields
            (regexp-opt attributes 'words)))))

(defun mentor-get-torrent (id)
  (gethash id mentor-torrent-hash))

(defun mentor-get-field (field torrent)
  (cdr (assoc field torrent)))

(defun mentor-torrent-name (torrent)
  (mentor-get-field "name" torrent))

(defun mentor-torrent-progress (torrent)
  (let* ((done (abs (mentor-get-field "bytes_done" torrent)))
         (total (abs (mentor-get-field "size_bytes" torrent)))
         (percent (* 100 (/ done total))))
    (if (>= percent 100)
        "DONE"
      (format "%2d%s" percent "%"))))

(defun mentor-torrent-status (torrent)
  (let* ((active (mentor-get-field "is_active" torrent))
         (closed (mentor-get-field "is_closed" torrent))
         (open (mentor-get-field "is_open" torrent)))
    (cond ((and active (= active 1)) " ")
          ((and closed (= closed 1)) "C")
          ((and open (= open 1)) "O")
          (t "?"))))


;;; Utility functions

(defun mentor-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
	(substring str 0 (- (length str) 1))
      str)))

(defun mentor-join-lists (list1 list2)
  (let ((result '()))
    (while list1
      (setq result (cons `(,(car list1) . ,(car list2)) result))
      (setq list1 (cdr list1))
      (setq list2 (cdr list2)))
    result))
(provide 'mentor)

;;; mentor.el ends here
