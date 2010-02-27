;;; mentor.el --- My Emacs kNows TORrents!  Control rtorrent from emacs

;; Copyright (C) 2010, Stefan Kangas

;; Maintainer: Stefan Kangas
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

;;; Change Log:

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
    (define-key map (kbd "n") 'mentor-next)
    (define-key map (kbd "p") 'mentor-prev)
    (define-key map (kbd "s") 'mentor-sort)
    (define-key map (kbd "Q") 'mentor-shutdown-rtorrent)
    map))

(defun mentor-mode ()
  "Major mode for controlling rtorrent from emacs"
  (kill-all-local-variables)
  (setq major-mode 'rtorrent-control-mode
        mode-name "rtorrent"
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
  (sort-lines))

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

(defun mentor-insert-torrent (id torrent)
  ;; TODO allow for different format lists
  (insert
   (propertize (concat (mentor-get-field "name" torrent)  "\n")
               'torrent-id id
               'collapsed t)))

(defun mentor-torrent-at-point ()
  (get-text-property 'torrent-id))

(defun mentor-next ()
  ;; TODO move to next torrent
  (interactive)
  (next-line))

(defun mentor-prev ()
  ;; TODO move to next torrent
  (interactive)
  (next-line))

(defun mentor-toggle-object ()
  (interactive)
  (message "TODO"))
  ;;(get-text-property 'torrent-id))


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
    (setq mentor-torrent-hash (make-hash-table)))
  (let* ((methods (mentor-rpc-system-listmethods "^d\\.\\(get\\|is\\)"))
         (tor-list (mentor-command-multi (mapcar
                                          (lambda (x) (concat x "="))
                                          methods)))
         (attributes (mapcar
                      (lambda (name)
                        (replace-regexp-in-string "^d\\.\\(get_\\)" "" name))
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
     torrents)))

(defun mentor-get-field (field torrent)
  (cdr (assoc field torrent)))


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
