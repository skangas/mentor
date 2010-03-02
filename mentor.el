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
;; Only update incomplete torrents
;; Highlight current torrent

;; Bug reports, comments, and suggestions are welcome!

;;; Change Log:

;; 0.1.0 First public release

;;; Code:

(require 'xml-rpc)

(defvar mentor-version "0.0.1")


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
    (define-key map (kbd "M-g") 'mentor-update-at-point)
    (define-key map (kbd "g") 'mentor-update)
    (define-key map (kbd "G") 'mentor-reload)
    (define-key map (kbd "DEL") 'mentor-toggle-object) ;; what to do?
    (define-key map (kbd "RET") 'mentor-toggle-object)
    (define-key map (kbd "TAB") 'mentor-toggle-object)
    (define-key map (kbd "d") 'mentor-stop-torrent)
    (define-key map (kbd "s") 'mentor-start-torrent)
    (define-key map (kbd "k") 'mentor-kill-torrent)
    (define-key map (kbd "K") 'mentor-kill-torrent-and-remove-data)
    (define-key map (kbd "n") 'mentor-next)
    (define-key map (kbd "p") 'mentor-prev)
    (define-key map (kbd "S") 'mentor-sort)
    (define-key map (kbd "Q") 'mentor-shutdown-rtorrent)
    map))

(defvar mentor-mode-hook nil)

(defun mentor-mode ()
  "Major mode for controlling rtorrent from emacs

\\{mentor-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'mentor-mode
        mode-name "mentor"
        buffer-read-only t
        truncate-lines t)
  (set (make-local-variable 'line-move-visual) nil)
  (use-local-map mentor-mode-map)
  (run-mode-hooks 'mentor-mode-hook))

(defun mentor-init ()
  (interactive)
  (if (mentor-not-connectable-p)
      (message "Unable to connect")
    (progn (switch-to-buffer (get-buffer-create "*mentor*"))
           (mentor-mode)
           (mentor-update-all))))

(defun mentor-not-connectable-p ()
  ;; TODO
  nil)



;;; Run XML-RPC calls

(defun mentor-command (&rest args)
  "Run command as an XML-RPC call via SCGI."
  (when (not (listp args))
    (setq args (list args)))
  (xml-rpc-xml-to-response
   (with-temp-buffer
     (apply 'call-process
            "/home/skangas/.emacs.d/lisp-personal/mentor/bin/xmlrpc2scgi.py"
            nil t nil (append `(,mentor-scgi-url) args))
     ;; (xml-rpc-value-to-xml-list
     (xml-rpc-request-process-buffer (current-buffer)))))

(defun mentor-command-multi (&rest args)
  (apply 'mentor-command (apply 'append '("d.multicall" "default") args)))

;; Needed to work around buggy expressions in rtorrent
(defvar mentor-method-exclusions-regexp "d\\.get_\\(mode\\|custom.*\\|bitfield\\)")

(defun mentor-rpc-system-listmethods (&optional regexp)
  "system.listMethods \
Returns a list of all available commands.  First argument is \
interpreted as a regexp, and if specified only returns matching \
functions"
  (let ((methods (mentor-command "system.listMethods"))
        (retval '()))
    (when regexp
      (mapc (lambda (cur)
              (when (and (string-match regexp cur)
                         (not (string-match mentor-method-exclusions-regexp cur)))
                (setq retval (cons cur retval))))
            methods))
    retval))


;;; Main view

;; (defun mentor-update-torrent-at-point ()
;;   (let ((id (mentor-id-at-point)))

(defun mentor-update ()
  "Update torrents"
  (interactive)
  (beginning-of-buffer)
  (mentor-update-torrent-list)
  (while (mentor-next)
    (when (not (mentor-torrent-is-done-p))
      (mentor-update-torrent-at-point))))

(defun mentor-update-all ()
  "Update torrent list mentor view completely"
  (interactive)
  (save-excursion
    (mentor-update-torrent-list)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (concat "mentor-" mentor-version " - rTorrent "
                      (mentor-command "system.client_version") "/"
                      (mentor-command "system.library_version")
                      " (" (mentor-command "get_name") ")\n\n"))
      (mentor-insert-torrents))))

(defun mentor-insert-torrents ()
  (maphash
   (lambda (id torrent)
     (mentor-insert-torrent id torrent))
   mentor-torrent-hash))

(defvar mentor-format-collapsed-torrent '("%s %s" "state" "name"))
(setq mentor-format-collapsed-torrent '("%.3s | %s | %-70s"
                                        mentor-torrent-progress
                                        mentor-torrent-status
                                        mentor-torrent-name))
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

(defun mentor-sort ()
  ;; TODO sort lines according to various criteria
  (interactive)
  (goto-char (point-min))
  (mentor-next)
  (save-excursion
    (let ((sort-fold-case t)
          (inhibit-read-only t))
      (sort-subr nil
                 (lambda () (ignore-errors (mentor-next)))
                 (lambda () (ignore-errors (mentor-goto-torrent-end)))
                 (lambda () (mentor-get-field-at-point "name"))))))

(defun mentor-id-at-point ()
  (get-text-property (point) 'torrent-id))

(defun mentor-torrent-at-point ()
  (mentor-get-torrent (mentor-id-at-point)))

(defun mentor-get-field-at-point (field)
  (mentor-get-field field (mentor-torrent-at-point)))

(defun mentor-get-hash-at-point ()
  (mentor-get-field-at-point "hash"))

(defun mentor-command-at-point (command)
  (mentor-command command (mentor-get-field-at-point "hash")))

(defmacro while-same-torrent (skip-blanks &rest body)
  `(let ((id (mentor-id-at-point)))
     (while (and (or (and ,skip-blanks
                          (not (mentor-id-at-point)))
                     (equal id (mentor-id-at-point))))
       ,@body)))

(defun mentor-next ()
  (interactive)
  (while-same-torrent t (forward-char))
  (beginning-of-line))

(defun mentor-prev ()
  (interactive)
  (while-same-torrent t (backward-char))
  (beginning-of-line))

(defun mentor-goto-torrent-beginning ()
  (interactive)
  (while-same-torrent nil
                      (backward-char)))

(defun mentor-goto-torrent-end ()
  (interactive)
  (while-same-torrent nil
                      (forward-char)))

(defun mentor-toggle-object ()
  (interactive)
  (let ((id (get-text-property (point) 'torrent-id)))
    (when id
      (let ((torrent (mentor-get-torrent id)))
        (message (mentor-get-field "connection_current" torrent))))))


;; Torrent actions

(defmacro mentor-use-torrent (&rest body)
  `(let ((torrent (or torrent
                      (mentor-torrent-at-point))))
     ,@body))

(defun mentor-kill-torrent ()
  (interactive)
  (let ((torrent (mentor-torrent-at-point))
        (files (mentor-torrent-file-list torrent)))
    ;; (mentor-stop-torrent)
    ;; (mentor-kill-torrent)
    (mapc
     (lambda (file)
       (message file))
     files)))

(defun mentor-kill-torrent-and-remove-data ()
  (interactive)
  (let ((id (mentor-id-at-point)))
    (mentor-stop-torrent)
    (message "TODO")))

(defmacro mentor-command-at-torrent-or-point (torrent command)
  `(if ,torrent
       (mentor-command ,command (mentor-get-field "hash" torrent))
     (mentor-command-at-point command)))

(defun mentor-stop-torrent (&optional torrent)
  (interactive)
  (montor-command-at-torrent-or-point torrent "d.stop"))

(defun mentor-start-torrent ()
  (interactive)
  (montor-command-at-torrent-or-point torrent "d.start"))

(defun mentor-pause-torrent ()
  (interactive)
   (montor-command-at-torrent-or-point torrent "d.pause"))


;;; Torrents

(defvar mentor-torrent-hash nil)
(make-variable-buffer-local 'mentor-torrent-hash)

(defun mentor-update-torrent-list ()
  "Update torrent information list"
  (message "Updating torrent list...")
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
            (regexp-opt attributes 'words))))
    (message "Updating torrent list... DONE"))

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
        "    "
      (format "%2d%s" percent "%"))))

(defun mentor-torrent-status (torrent)
  (let* ((active (mentor-get-field "is_active" torrent))
         (closed (mentor-get-field "is_closed" torrent))
         (open (mentor-get-field "is_open" torrent)))
    (cond ((and active (= active 1)) " ")
          ((and closed (= closed 1)) "C")
          ((and open (= open 1)) "O")
          (t "?"))))

(defun mentor-torrent-get-file-list (torrent)
  (mentor-command "f.multicall" (mentor-get-field "hash" torrent) "" "f.get_path="))

(defun mentor-torrent-get-target-directory (&optional torrent)
  (mentor-use-torrent
   (mentor-get-field "directory" torrent)))

(defun mentor-torrent-is-done-p (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (= (mentor-get-field "bytes_done" torrent)
      (mentor-get-field "size_bytes" torrent))))

(defun mentor-torrent-is-multi-file-p (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (= 1 (mentor-get-field "is_multi_file" torrent))))

(defun mentor-torrent-is-open-p (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (= 1 (mentor-get-field "is_open" torrent))))


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
