;;; mentor.el --- My Emacs kNows TORrents!  Control rtorrent from GNU Emacs

;; Copyright (C) 2010, 2011, Stefan Kangas

;; Author: Stefan Kangas
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
;; Implement SCGI in Emacs Lisp (later)
;; Support for categories
;; Optimization: Update info only for incomplete torrents
;; Highlight current torrent
;; Filters
;; Marking torrents

;; Known issues:

;; Large files (> 4GB) will cause overflows when not running rtorrent compiled
;; against xmlrpc-c >1.07 with -DXMLRPC_HAVE_I8 and add relevant setting to your
;; rtorrent configuration (see below).
;;
;; Work-around installed for now. For more details, see:
;;
;; http://code.google.com/p/transdroid/issues/detail?id=31
;; http://libtorrent.rakshasa.no/ticket/1538
;; http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=575560
;; http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=563075
;;
;; We will not get a working version in Debian stable any time soon, so you
;; might want to install a backported rtorrent. (see package libxmlrpc-c3)
;;
;; Also, you must add this following to your .rtorrent.rc:
;;
;; xmlrpc_dialect=i8

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

(defcustom mentor-auto-update-default nil
  "If non-nil, enable auto update for all newly created mentor buffers."
  :type 'boolean
  :group 'mentor)

(defcustom mentor-auto-update-interval 5
  "Time interval in seconds for auto updating mentor buffers."
  :type 'integer
  :group 'mentor)

(defcustom mentor-rtorrent-url "scgi://localhost:5000"
  "The URL to the rtorrent client. Can either be on the form
scgi://HOST:PORT or http://HOST[:PORT]/PATH depending on if you are
connecting through scgi or http."
  :group 'mentor
  :type 'string)


;;; major mode

(defvar mentor-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    ;; keys mimicking the default rtorrent UI
    (define-key map (kbd "M-s") 'mentor-start-torrent) ;; Start download.
    (define-key map (kbd "M-d") 'mentor-stop-torrent) ;; Stop an active download, or remove a stopped download.
    (define-key map (kbd "M-k") 'mentor-close-torrent) ;; Close a torrent and its files.
    (define-key map (kbd "M-e") 'mentor-recreate-files) ;; Set  the 'create/resize queued' flags on all files in a torrent.
    (define-key map (kbd "M-r") 'mentor-hash-check-torrent) ;; Initiate hash check of torrent.
    (define-key map (kbd "M-o") 'mentor-change-directory) ;; Change  the  destination  directory of the download. The torrent
    (define-key map (kbd "M-c") 'mentor-call-command) ;; Call commands or change settings.
    (define-key map (kbd "M-b") 'mentor-set-inital-seeding) ;; Set download to perform initial seeding.

    (define-key map (kbd "+") 'mentor-increase-priority) ;; Change the priority of the download.
    (define-key map (kbd "-") 'mentor-decrease-priority) ;; Change the priority of the download.

    (define-key map (kbd "DEL") 'mentor-add-torrent)

    (define-key map (kbd "M-g") 'mentor-update-torrent-and-redisplay)
    (define-key map (kbd "g") 'mentor-update)
    (define-key map (kbd "G") 'mentor-reload)
    (define-key map (kbd "RET") 'mentor-toggle-object)
    (define-key map (kbd "TAB") 'mentor-toggle-object)
    (define-key map (kbd "k") 'mentor-kill-torrent)
    (define-key map (kbd "K") 'mentor-kill-torrent-and-remove-data)
    (define-key map (kbd "n") 'mentor-next)
    (define-key map (kbd "p") 'mentor-prev)
    (define-key map (kbd "M") 'mentor-move-torrent)
    (define-key map (kbd "m") 'mentor-mark-torrent)
    (define-key map (kbd "s c") 'mentor-sort-by-state)
    (define-key map (kbd "s d") 'mentor-sort-by-download-speed)
    (define-key map (kbd "s n") 'mentor-sort-by-name)
    (define-key map (kbd "s p") 'mentor-sort-by-property-prompt)
    (define-key map (kbd "s s") 'mentor-sort-by-size)
    (define-key map (kbd "s t") 'mentor-sort-by-tied-file-name)
    (define-key map (kbd "s u") 'mentor-sort-by-upload-speed)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "Q") 'mentor-shutdown-rtorrent)
    map))

(defvar mentor-mode-hook nil)

(defvar mentor-auto-update-buffers nil)

(defvar mentor-auto-update-timer nil)

(defvar mentor-sort-property nil)
(make-variable-buffer-local 'mentor-sort-property)

(defvar mentor-sort-reverse nil)
(make-variable-buffer-local 'mentor-sort-reverse)

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
  (run-mode-hooks 'mentor-mode-hook)
  (if (and (not mentor-auto-update-timer) mentor-auto-update-interval)
      (setq mentor-auto-update-timer
            (run-at-time t mentor-auto-update-interval
                         'mentor-auto-update-timer))))

(defun mentor-init ()
  (interactive)
  (if (mentor-not-connectable-p)
      (message "Unable to connect")
    (progn (switch-to-buffer (get-buffer-create "*mentor*"))
           (mentor-mode)
           (mentor-reload))))

(defun mentor-not-connectable-p ()
  ;; TODO
  nil)

(defun mentor-auto-update-timer ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (eq major-mode 'mentor-mode)
               mentor-auto-update-flag)
          (mentor-update t t)))))

(defun mentor-toggle-auto-update (arg)
  "Change whether this Mentor buffer is updated automatically.
With prefix ARG, update this buffer automatically if ARG is positive,
otherwise do not update.  Sets the variable `mentor-auto-update-flag'.
The time interval for updates is specified via `mentor-auto-update-interval'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq mentor-auto-update-flag
        (cond ((eq arg 'toggle) (not mentor-auto-update-flag))
              (arg (> (prefix-numeric-value arg) 0))
              (t (not mentor-auto-update-flag))))
  (message "Mentor auto update %s"
           (if mentor-auto-update-flag "enabled" "disabled")))


;;; Run XML-RPC calls

(defun mentor-rpc-command (&rest args)
  "Run command as an XML-RPC call via SCGI or http."
  (when (not (listp args))
    (setq args (list args)))
  (if (string= (subseq mentor-rtorrent-url 0 4) "http")
      (apply 'xml-rpc-method-call mentor-rtorrent-url args)
    (xml-rpc-xml-to-response
     (with-temp-buffer
       (apply 'call-process
	      "/home/skangas/.emacs.d/lisp-personal/mentor/bin/xmlrpc2scgi.py"
            nil t nil (append `(,mentor-rtorrent-url) args))
       ;; (xml-rpc-value-to-xml-list
       (xml-rpc-request-process-buffer (current-buffer))))))

(defun mentor-rpc-command-multi (&rest args)
  (apply 'mentor-rpc-command (apply 'append '("d.multicall" "default") args)))

;; Needed to work around buggy expressions in rtorrent
(defvar mentor-method-exclusions-regexp "d\\.get_\\(mode\\|custom.*\\|bitfield\\)")

(defvar mentor-all-rpc-methods-list nil)

(defun mentor-rpc-list-methods (&optional regexp)
  "system.listMethods \
Returns a list of all available commands.  First argument is \
interpreted as a regexp, and if specified only returns matching \
functions"
  (when (not mentor-all-rpc-methods-list)
    (setq mentor-all-rpc-methods-list (mentor-rpc-command "system.listMethods")))
  (let ((methods mentor-all-rpc-methods-list)
        (retval '()))
    (when regexp
      (mapc (lambda (cur)
              (when (and (string-match regexp cur)
                         (not (string-match mentor-method-exclusions-regexp cur)))
                (setq retval (cons cur retval))))
            methods))
    retval))


;;; Main view

;; (defvar *mentor-update-time* (current-time))

(defun mentor-update ()
  "Update torrents"
  (interactive)
  (beginning-of-buffer)
  (mentor-update-torrent-list)
  (while (mentor-next)
    (when (not (mentor-torrent-is-done-p))
      (mentor-update-torrent))))

(defun mentor-reload ()
  "Completely reload the mentor torrent view buffer."
  (interactive)
  (when (equal major-mode 'mentor-mode)
    (save-excursion
      (mentor-update-torrent-list)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (concat "mentor-" mentor-version " - rTorrent "
                        (mentor-rpc-command "system.client_version") "/"
                        (mentor-rpc-command "system.library_version")
                        " (" (mentor-rpc-command "get_name") ")\n\n"))
        (mentor-insert-torrents)
        (mentor-sort-current)))))

(defun mentor-insert-torrents ()
  (maphash
   (lambda (id torrent)
     (mentor-insert-torrent id torrent))
   mentor-torrents))

(defvar mentor-format-collapsed-torrent '("%.1s U:%-5s D:%-5s %-80s %.4s  %10s / %6s  %-70s"
                                        mentor-torrent-state
                                        mentor-torrent-get-speed-up
                                        mentor-torrent-get-speed-down
                                        (mentor-torrent-get-name . 80)
                                        mentor-torrent-get-progress
                                        mentor-torrent-get-size-done
                                        mentor-torrent-get-size-total
                                        mentor-torrent-tied-file-name))

(defun mentor-redisplay-torrent (torrent)
  (let ((buffer-read-only nil)
        (id (mentor-id-at-point)))
    (delete-region (mentor-get-torrent-beginning) (mentor-get-torrent-end))
    (forward-char) ;; avoid oboe
    (mentor-insert-torrent id torrent)
    (mentor-prev)))

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
  "Process the torrent format string"
  (let ((re mentor-regexp-information-properties))
    (apply
     'format (car format-list)
     (mapcar
      (lambda (fmt)
        (let* ((len (if (listp fmt) (cdr fmt) nil))
               (fmt (if (listp fmt) (car fmt) fmt)))
          (format (if len (concat "%." (number-to-string len) "s") "%s")
           (cond ((functionp fmt)
                  (funcall fmt torrent))
                 ((stringp fmt)
                  (if nil ;;(string-match re fmt)
                      (or (mentor-get-property
                           (substring fmt
                                      (+ (match-beginning 0) 0)
                                      (- (match-end 0) 0))
                           torrent)
                          "")
                    fmt))
                 (t "")))))
      (cdr format-list)))))


;;; Sorting

(defun mentor-sort-by-property (property &optional reverse)
  (setq mentor-sort-property property)
  (setq mentor-sort-reverse reverse)
  (goto-char (point-min))
  (mentor-next)
  (save-excursion
    (let ((sort-fold-case t)
          (inhibit-read-only t))
      (sort-subr reverse
                 (lambda () (ignore-errors (mentor-next)))
                 (lambda () (ignore-errors (mentor-goto-torrent-end)))
                 (lambda () (mentor-get-property property))))))

(defun mentor-sort-current ()
  "Sort buffer according to `mentor-sort-property' and `mentor-sort-reverse'."
  (when mentor-sort-property
    (mentor-sort-by-property mentor-sort-property mentor-sort-reverse)))

(defun mentor-sort-by-download-speed ()
  (interactive)
  (mentor-sort-by-property "down_rate" t))

(defun mentor-sort-by-name ()
  (interactive)
  (mentor-sort-by-property "name"))

(defun mentor-sort-by-state ()
  (interactive)
  (mentor-sort-by-property "state"))

(defun mentor-sort-by-tied-file-name ()
  (interactive)
  (mentor-sort-by-property "tied_to_file"))

(defun mentor-sort-by-size ()
  (interactive)
  (mentor-sort-by-property "size_bytes" t))

(defun mentor-sort-by-upload-speed ()
  (interactive)
  (mentor-sort-by-property "up_rate" t))


;;; Get torrent

(defun mentor-id-at-point ()
  (get-text-property (point) 'torrent-id))

(defun mentor-torrent-at-point ()
  (mentor-get-torrent (mentor-id-at-point)))

(defmacro mentor-use-torrent (&rest body)
  "Convenience macro to use either the defined `torrent' value or
the torrent at point."
  `(let ((torrent (or torrent
                      (mentor-torrent-at-point)
                      (error "no torrent"))))
     ,@body))

(defmacro while-same-torrent (skip-blanks &rest body)
  `(let ((id (mentor-id-at-point)))
     (while (and (or (and ,skip-blanks
                          (not (mentor-id-at-point)))
                     (equal id (mentor-id-at-point))))
       ,@body)))


;;; Navigation

(defun mentor-next ()
  (interactive)
  (while-same-torrent t (forward-char))
  (beginning-of-line))

(defun mentor-prev ()
  (interactive)
  (while-same-torrent t (backward-char))
  (beginning-of-line))

(defun mentor-get-torrent-beginning ()
  (save-excursion
    (mentor-goto-torrent-beginning)
    (point)))

(defun mentor-get-torrent-end ()
  (save-excursion
    (mentor-goto-torrent-end)
    (point)))

(defun mentor-goto-torrent-beginning ()
  (interactive)
  (while-same-torrent nil (backward-char)))

(defun mentor-goto-torrent-end ()
  (interactive)
  (while-same-torrent nil (forward-char)))

(defun mentor-toggle-object ()
  (interactive)
  (let ((id (get-text-property (point) 'torrent-id)))
    (when id
      (let ((torrent (mentor-get-torrent id)))
        (message (number-to-string (mentor-get-property 'bytes_done torrent)))))))


;;; Torrent actions

(defun mentor-kill-torrent (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (let ((files (mentor-torrent-get-file-list torrent)))
     ;; (mentor-stop-torrent)
     ;; (mentor-kill-torrent)
     (mapc
      (lambda (file)
        (message file))
      files))))

(defun mentor-kill-torrent-and-remove-data ()
  (interactive)
  (let ((id (mentor-id-at-point)))
    (mentor-stop-torrent)
    (message "TODO")))

(defun mentor-call-command (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-change-directory (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-close-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.close" (mentor-get-property 'hash torrent)))

(defun mentor-hash-check-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.check_hash" (mentor-get-property 'hash torrent)))

(defun mentor-pause-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.pause" (mentor-get-property 'hash torrent)))

(defun mentor-recreate-files (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-set-inital-seeding (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-start-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.start" (mentor-get-property 'hash torrent)))

(defun mentor-stop-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.stop" (mentor-get-property 'hash torrent)))

(defun mentor-increase-priority (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-decrease-priority (&optional torrent)
  (interactive)
  (message "TODO"))


;;; Torrents

(defvar mentor-torrents nil
  "Hash table containing all torrents")
(make-variable-buffer-local 'mentor-torrents)

(defvar mentor-regexp-information-properties nil)

;; (defun mentor-conv-method-names-to-attributes (&rest methods)
;;   (mapcar (lambda (method)
;;             (replace-regexp-in-string "^d\\.\\(get_\\)?" "" method))
;;           methods))

;; (defun mentor-list-update (torrent)
;;   "Update the torrent list, using the results from the XML-RPC
;; d.multicall which are passed in as the first argument"
  
(defvar mentor-important-methods
  '(
    "d.get_bytes_done"
    "d.get_down_rate"
    "d.get_priority"
    "d.get_up_rate"
    "d.get_up_total"
    "d.is_active"
    "d.is_active"
    "d.is_hash_checked"
    "d.is_hash_checking"
    "d.is_open"
    "d.is_pex_active"))
    ;; "hash"

(defun mentor-rpc-method-to-property (name)
  (intern
   (replace-regexp-in-string "^d\\.\\(get_\\)?" "" name)))

(defun mentor-property-to-rpc-method ()
  nil)

(defun mentor-update-torrent-and-redisplay (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (mentor-update-torrent torrent)
   (mentor-redisplay-torrent torrent)))

(defun mentor-update-torrent (torrent)
  (let* ((hash (mentor-get-property 'hash torrent))
         (id (mentor-get-property 'local_id torrent)))
    (dolist (method mentor-important-methods)
      (let ((property (mentor-rpc-method-to-property method))
            (new-value (mentor-rpc-command method hash)))
        (setcdr (assq property torrent) new-value)))))

(defun mentor-update-torrent-list ()
  "Synchronize torrent information with rtorrent.

By default, all information will be received anew, making this a
potentially expensive operation.

Optionally a torrent ID may be specified as the second argument.
If so, only the torrent with this ID will be updated."
  (message "Updating torrent list...")
  (when (not mentor-torrents)
    (setq mentor-torrents (make-hash-table :test 'equal)))
  (let* ((methods (mentor-rpc-list-methods "^d\\.\\(get\\|is\\)"))
         (tor-list (mentor-rpc-command-multi
                    (mapcar (lambda (x) (concat x "=")) methods)))
         (attributes (mapcar 'mentor-rpc-method-to-property methods))
         (torrents (mapcar (lambda (torrent)
                             (mentor-join-lists attributes torrent))
                           tor-list)))
    (dolist (torrent torrents)
      (let ((id (mentor-get-property 'local_id torrent)))
        (setq torrent (assq-delete-all id torrent))
        (puthash id torrent mentor-torrents)))
  ;; (when (not mentor-regexp-information-properties)
  ;;   (setq mentor-regexp-information-properties
  ;;         (regexp-opt attributes 'words))))
    (message "Updating torrent list... DONE")))

(defun mentor-get-torrent (id)
  (gethash id mentor-torrents))

(defun mentor-get-property (property &optional torrent)
  "Get property for a torrent.
If `torrent' is nil, use torrent at point."
  ;; (when (not torrent)
  ;;     (setq torrent (mentor-torrent-at-point)))
  ;; (when (stringp property)
  ;;     (setq property (make-symbol property)))
  (cdr (assoc property torrent)))

(defun mentor-torrent-get-name (&optional torrent)
  (mentor-get-property 'name torrent))

(defun mentor-torrent-get-progress (torrent)
  (let* ((done (abs (or (mentor-get-property 'bytes_done torrent) 0)))
         (total (abs (or (mentor-get-property 'size_bytes torrent) 1)))
         (percent (* 100 (/ done total))))
    (format "%3d%s" percent "%")))

(defun mentor-torrent-get-state (&optional torrent)
  (if (= (mentor-get-property 'state torrent) 1)
      " "
    "S"))

(defun mentor-torrent-get-speed-down (torrent)
  (mentor-bytes-to-kilobytes
   (mentor-get-property 'down_rate torrent)))

(defun mentor-torrent-get-speed-up (torrent)
  (mentor-bytes-to-kilobytes
   (mentor-get-property 'up_rate torrent)))

(defun mentor-torrent-get-size-done (torrent)
  (mentor-bytes-to-human
   (mentor-get-property 'bytes_done torrent)))

(defun mentor-torrent-get-size-total (torrent)
  (mentor-bytes-to-human
   (mentor-get-property 'size_bytes torrent)))

(defun mentor-torrent-status (&optional torrent)
  (let* ((active (mentor-get-property 'is_active torrent))
         (open (mentor-get-property 'is_open torrent)))
    (if (or (and open (= open 1))
            (and active (= active 1)))
        " "
      "I")))

(defun mentor-torrent-tied-file-name (torrent)
  (mentor-get-property 'tied_to_file torrent))

(defun mentor-torrent-get-file-list (torrent)
  (mentor-rpc-command "f.multicall" (mentor-get-property 'hash torrent) "" "f.get_path="))

(defun mentor-get-target-directory (&optional torrent)
  (mentor-use-torrent
   (mentor-get-property 'directory torrent)))

(defun mentor-torrent-is-done-p (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (= (mentor-get-property 'bytes_done torrent)
      (mentor-get-property 'size_bytes torrent))))

(defun mentor-torrent-is-multi-file-p (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (= 1 (mentor-get-property 'is_multi_file torrent))))

(defun mentor-torrent-is-open-p (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (= 1 (mentor-get-property 'is_open torrent))))


;;; Utility functions

(defun mentor-bytes-to-human (bytes)
  (if bytes
      (let* ((bytes (if (stringp bytes) (string-to-number bytes) bytes))
             (kb 1024.0)
             (mb (* kb 1024.0))
             (gb (* mb 1024.0))
             (tb (* gb 1024.0)))
        (cond ((< bytes kb) bytes)
              ((< bytes mb) (concat (format "%.1f" (/ bytes kb)) "K"))
              ((< bytes gb) (concat (format "%.1f" (/ bytes mb)) "M"))
              ((< bytes tb) (concat (format "%.1f" (/ bytes gb)) "G"))
              (t "1TB+")))
    ""))

(defun mentor-bytes-to-kilobytes (bytes)
  (if bytes
      (number-to-string (/ bytes 1024))
    ""))

(defun mentor-join-lists (list1 list2)
  (let ((result '()))
    (while list1
      (setq result (cons `(,(car list1) . ,(car list2)) result))
      (setq list1 (cdr list1))
      (setq list2 (cdr list2)))
    result))

(defun mentor-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
	(substring str 0 (- (length str) 1))
      str)))

(provide 'mentor)

;;; mentor.el ends here
