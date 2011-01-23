;;; mentor.el --- Control rtorrent from GNU Emacs

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

;; Large files (> 2GB) will cause overflows when not running rtorrent compiled
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

(defcustom mentor-current-view "main"
  "The current view to use when browsing torrents. This is also
  the default view when starting mentor."
  :group 'mentor
  :type 'string)

(defcustom mentor-custom-views 
  '((1 . "main") (2 . "name") (3 . "started")
    (4 . "stopped") (5 . "complete") (6 . "incomplete")
    (7 . "hashing") (8 . "seeding") (9 . "active"))
  "A list of mappings \"(BINDING . VIEWNAME)\" where BINDING is
the key to which the specified view will be bound to."
  :group 'mentor
  :type '(cons :integer :string))

(defcustom mentor-rtorrent-url "scgi://localhost:5000"
  "The URL to the rtorrent client. Can either be on the form
scgi://HOST:PORT or http://HOST[:PORT]/PATH depending on if you are
connecting through scgi or http."
  :group 'mentor
  :type 'string)

(defcustom mentor-view-columns
  '((mentor-torrent-get-state -3 "ST")
    (mentor-torrent-get-speed-up -6 "Up")
    (mentor-torrent-get-speed-down -6 "Down")
    (mentor-torrent-get-name -80 "Name")
    (mentor-torrent-get-progress -5 "Progress")
    (mentor-torrent-get-size -15 "     Size")
    (mentor-torrent-get-tied-file-name -80 "Tied file name"))
  "A list of all columns to show in mentor view."
  :group 'mentor
  :type '(cons :symbol :string))


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

    ;; torrent list actions
    (define-key map (kbd "g") 'mentor-update)
    (define-key map (kbd "G") 'mentor-reload)
    (define-key map (kbd "M-g") 'mentor-update-torrent-and-redisplay)

    ;; navigation
    (define-key map (kbd "n") 'mentor-next)
    (define-key map (kbd "p") 'mentor-prev)
    (define-key map (kbd "M") 'mentor-move-torrent)
    (define-key map (kbd "m") 'mentor-mark-torrent)

    ;; single torrent actions
    (define-key map (kbd "d") 'mentor-stop-torrent)
    (define-key map (kbd "D") 'mentor-stop-all-torrents)
    (define-key map (kbd "k") 'mentor-erase-torrent)
    (define-key map (kbd "K") 'mentor-erase-torrent-and-data)
    (define-key map (kbd "r") 'mentor-hash-check-torrent)
    (define-key map (kbd "s") 'mentor-start-torrent)
    (define-key map (kbd "S") 'mentor-start-all-torrents)

    ;; misc actions
    (define-key map (kbd "RET") 'mentor-torrent-detail-screen)
    (define-key map (kbd "TAB") 'mentor-toggle-object)
    (define-key map (kbd "v") 'mentor-view-in-dired)

    ;; sort functions
    (define-key map (kbd "t c") 'mentor-sort-by-state)
    (define-key map (kbd "t d") 'mentor-sort-by-download-speed)
    (define-key map (kbd "t n") 'mentor-sort-by-name)
    (define-key map (kbd "t p") 'mentor-sort-by-property-prompt)
    (define-key map (kbd "t s") 'mentor-sort-by-size)
    (define-key map (kbd "t t") 'mentor-sort-by-tied-file-name)
    (define-key map (kbd "t u") 'mentor-sort-by-upload-speed)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "Q") 'mentor-shutdown-rtorrent)
    
    ;; view bindings
    (define-key map (kbd "v") 'mentor-set-view)
    (define-key map (kbd "1") (lambda () (interactive) (mentor-set-view 1)))
    (define-key map (kbd "2") (lambda () (interactive) (mentor-set-view 2)))
    (define-key map (kbd "3") (lambda () (interactive) (mentor-set-view 3)))
    (define-key map (kbd "4") (lambda () (interactive) (mentor-set-view 4)))
    (define-key map (kbd "5") (lambda () (interactive) (mentor-set-view 5)))
    (define-key map (kbd "6") (lambda () (interactive) (mentor-set-view 6)))
    (define-key map (kbd "7") (lambda () (interactive) (mentor-set-view 7)))
    (define-key map (kbd "8") (lambda () (interactive) (mentor-set-view 8)))
    (define-key map (kbd "9") (lambda () (interactive) (mentor-set-view 9)))
    (define-key map (kbd "M-v") 'mentor-torrent-add-view-prompt)
    map))

(defvar mentor-mode-hook nil)

(defvar mentor-auto-update-buffers nil)

(defvar mentor-auto-update-timer nil)

(defvar mentor-header-line nil)

(defvar mentor-sort-property nil)
(make-variable-buffer-local 'mentor-sort-property)

(defvar mentor-sort-reverse nil)
(make-variable-buffer-local 'mentor-sort-reverse)

(defvar mentor-torrent-views nil)
;;(make-variable-buffer-local 'mentor-torrent-views)

(defun mentor-mode ()
  "Major mode for controlling rtorrent from emacs

\\{mentor-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'mentor-mode
        mode-name "mentor"
        buffer-read-only t
        truncate-lines t)
  (setq mentor-torrents (make-hash-table :test 'equal))
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
           (mentor-init-header-line)
           (mentor-init-torrent-list)
	   (mentor-get-and-update-views)
           (mentor-redisplay))))

(defun mentor-init-header-line ()
  (setq header-line-format
        '(:eval (concat
                 (propertize " " 'display '((space :align-to 0)))
                 (substring mentor-header-line
                            (min (length mentor-header-line)
                                 (window-hscroll)))))))

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
  "Update all torrents and redisplay."
  (interactive)
  (mentor-update-torrents)
  (mentor-redisplay))

(defun mentor-reload ()
  "Re-initialize all torrents and redisplay."
  (interactive)
  (mentor-init-torrent-list)
  (mentor-reload-header-line)
  (mentor-get-and-update-views)
  (setq mode-line-buffer-identification (concat "*mentor " mentor-current-view "*"))
  (mentor-redisplay))

(defun mentor-redisplay ()
  "Completely reload the mentor torrent view buffer."
  (interactive)
  (when (equal major-mode 'mentor-mode)
    (save-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mentor-insert-torrents)
        (insert (concat "\nmentor-" mentor-version " - rTorrent "
                        (mentor-rpc-command "system.client_version") "/"
                        (mentor-rpc-command "system.library_version")
                        " (" (mentor-rpc-command "get_name") ")\n"))
        (mentor-sort-current)))))

(defun mentor-insert-torrents ()
  (maphash
   (lambda (id torrent)
     (mentor-insert-torrent id torrent))
   mentor-torrents))

(defun mentor-redisplay-torrent (torrent)
  (let ((buffer-read-only nil)
        (id (mentor-id-at-point)))
    (mentor-remove-torrent-from-view torrent)
    (mentor-insert-torrent id torrent)
    (mentor-prev)))

(defun mentor-remove-torrent-from-view (torrent)
  (let ((buffer-read-only nil))
    (delete-region (mentor-get-torrent-beginning) (mentor-get-torrent-end))
    (forward-char)))

(defun mentor-insert-torrent (id torrent)
  (let ((text (mentor-process-view-columns torrent)))
    (insert (propertize (concat text "\n")
                        'torrent-id id 'collapsed t))))

(defun mentor-process-view-columns (torrent)
  (apply 'concat
         (mapcar (lambda (col)
                   (let* ((str (funcall (car col) torrent))
                          (len (cadr col)))
                     (concat (mentor-enforce-length str len)
                             " ")))
                 mentor-view-columns)))

(defun mentor-reload-header-line ()
  (setq mentor-header-line
        (apply 'concat
               (mapcar (lambda (col)
                         (let* ((str (caddr col))
                                (len (or (cadddr col)
                                         (cadr col))))
                           (concat (mentor-enforce-length str len)
                                   " ")))
                       mentor-view-columns))))


;;; Sorting

(defun mentor-sort-by-property (property &optional reverse)
  (setq mentor-sort-property property)
  (setq mentor-sort-reverse reverse)
  (goto-char (point-min))
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
  (mentor-sort-by-property 'down_rate t))

(defun mentor-sort-by-name ()
  (interactive)
  (mentor-sort-by-property 'name))

(defun mentor-sort-by-state ()
  (interactive)
  (mentor-sort-by-property 'state))

(defun mentor-sort-by-tied-file-name ()
  (interactive)
  (mentor-sort-by-property 'tied_to_file))

(defun mentor-sort-by-size ()
  (interactive)
  (mentor-sort-by-property 'size_bytes t))

(defun mentor-sort-by-upload-speed ()
  (interactive)
  (mentor-sort-by-property 'up_rate t))


;;; Get torrent

(defun mentor-id-at-point ()
  (get-text-property (point) 'torrent-id))

(defun mentor-torrent-at-point ()
  (mentor-get-torrent (mentor-id-at-point)))

(defmacro mentor-use-torrent (&rest body)
  "Convenience macro to use either the defined `torrent' value or
the torrent at point."
  `(let ((torrent (or (when (boundp 'torrent) torrent)
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

(defun mentor-erase-data (torrent)
  (dired-delete-file (mentor-get-property 'base_path torrent) 'top))


;;; Interactive torrent commands

(defun mentor-erase-torrent (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (when (yes-or-no-p (concat "Remove " (mentor-get-property 'name torrent) " "))
     (mentor-rpc-command "d.erase" (mentor-get-property 'hash torrent))
     (mentor-remove-torrent-from-view torrent)
     (remhash (mentor-get-property 'local_id torrent) mentor-torrents))))

(defun mentor-erase-torrent-and-data ()
  (interactive)
  (mentor-use-torrent
   (mentor-erase-torrent torrent)
   (mentor-erase-data torrent)))

(defun mentor-call-command (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-change-directory (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-close-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.close" (mentor-get-property 'hash torrent))
  (mentor-update-torrent-and-redisplay))

(defun mentor-hash-check-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.check_hash" (mentor-get-property 'hash torrent))
  (mentor-update-torrent-and-redisplay))

(defun mentor-pause-torrent (&optional torrent)
  "Pause torrent. This is probably not what you want, use
`mentor-stop-torrent' instead."
  (interactive)
  (mentor-use-torrent
   (mentor-rpc-command "d.pause" (mentor-get-property 'hash torrent))
   (mentor-update-torrent-and-redisplay)))

(defun mentor-resume-torrent (&optional torrent)
  "Resume torrent. This is probably not what you want, use
`mentor-start-torrent' instead."
  (interactive)
  (mentor-use-torrent
   (mentor-rpc-command "d.resume" (mentor-get-property 'hash torrent))
   (mentor-update-torrent-and-redisplay)))

(defun mentor-recreate-files (&optional torrent)
  (interactive)
  (message "TODO"))

(defun mentor-set-inital-seeding (&optional torrent)
  (interactive)
  (message "TODO"))

;; TODO: go directly to file
(defun mentor-view-in-dired (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (let ((path (mentor-get-property 'base_path torrent))
         (is-multi-file (mentor-get-property 'is_multi_file torrent)))
     (find-file (if is-multi-file
                    path
                  (file-name-directory path))))))

(defun mentor-start-torrent (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (mentor-rpc-command "d.start" (mentor-get-property 'hash torrent))
   (mentor-update-torrent-and-redisplay)))

(defun mentor-stop-torrent (&optional torrent)
  (interactive)
  (mentor-rpc-command "d.stop" (mentor-get-property 'hash torrent))
  (mentor-update-torrent-and-redisplay))

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

(defvar mentor-d-interesting-methods
  '("d.get_bytes_done"
    "d.get_down_rate"
    "d.get_hashing"
    "d.get_hashing_failed"
    "d.get_priority"
    "d.get_up_rate"
    "d.get_up_total"
    "d.get_state"
    "d.views"
    "d.is_active"
    "d.is_hash_checked"
    "d.is_hash_checking"
    "d.is_open"
    "d.is_pex_active"))

(defun mentor-rpc-method-to-property (name)
  (intern
   (replace-regexp-in-string "^d\\.\\(get_\\)?\\|=$" "" name)))

;; (defun mentor-property-to-rpc-method () nil)

(defun mentor-update-torrent (torrent)
  (let* ((hash (mentor-get-property 'hash torrent))
         (id (mentor-get-property 'local_id torrent)))
    (dolist (method mentor-d-interesting-methods)
      (let ((property (mentor-rpc-method-to-property method))
            (new-value (mentor-rpc-command method hash)))
        (setcdr (assq property torrent) new-value)))))

(defun mentor-update-torrent-and-redisplay (&optional torrent)
  (interactive)
  (mentor-use-torrent
   (mentor-update-torrent torrent)
   (mentor-redisplay-torrent torrent)))

(defun mentor-rpc-d.multicall (methods)
  (let* ((methods= (mapcar (lambda (m) (concat m "=")) methods))
         (tor-list (apply 'mentor-rpc-command "d.multicall" mentor-current-view methods=))
         (attributes (mapcar 'mentor-rpc-method-to-property methods)))
    (mapcar (lambda (torrent)
              (mapcar* (lambda (a b) (cons a b))
                       attributes torrent))
            tor-list)))

(defun mentor-update-torrents ()
  (interactive)
  (message "Updating torrent list...")
  (let* ((methods (cons "d.get_local_id" mentor-d-interesting-methods))
         (torrents (mentor-rpc-d.multicall methods)))
    (dolist (tor torrents)
      (let* ((id (mentor-get-property 'local_id tor))
             (tor^ (mentor-get-torrent id)))
        (dolist (p tor)
          (setcdr (assq (car p) tor^) (cdr p))))))
  (message "Updating torrent list...DONE"))

(defun mentor-init-torrent-list ()
  "Initialize torrent list from rtorrent.

All torrent information will be re-fetched, making this an
expensive operation."
  (message "Initializing torrent list...")
  (setq mentor-torrents (make-hash-table :test 'equal))
  (let* ((methods (mentor-rpc-list-methods "^d\\.\\(get\\|is\\|views$\\)"))
         (torrents (mentor-rpc-d.multicall methods)))
    (dolist (tor torrents)
      (let ((id (mentor-get-property 'local_id tor)))
        (puthash id tor mentor-torrents))))
  (setq mentor-torrent-views (mentor-get-view-list))
  (message "Initializing torrent list... DONE"))
  ;; (when (not mentor-regexp-information-properties)
  ;;   (setq mentor-regexp-information-properties
  ;;         (regexp-opt attributes 'words))))

(defun mentor-get-torrent (id)
  (gethash id mentor-torrents))

(defun mentor-get-property (property &optional torrent)
  "Get property for a torrent.
If `torrent' is nil, use torrent at point."
  (when (not torrent)
      (setq torrent (mentor-torrent-at-point)))
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

;; TODO show an "I" for incomplete torrents
(defun mentor-torrent-get-state (&optional torrent)
  (concat
   (or (when (> (mentor-get-property 'hashing torrent) 0)
         "H")
       (if (not (= (mentor-get-property 'is_active torrent) 1))
           "S" " ")) ;; 'is_stopped
   (if (not (= (mentor-get-property 'is_open torrent) 1))
       "C" " "))) ;; 'is_closed

(defun mentor-torrent-get-speed-down (torrent)
  (mentor-bytes-to-kilobytes
   (mentor-get-property 'down_rate torrent)))

(defun mentor-torrent-get-speed-up (torrent)
  (mentor-bytes-to-kilobytes
   (mentor-get-property 'up_rate torrent)))

(defun mentor-torrent-get-size (torrent)
  (let ((done (mentor-get-property 'bytes_done torrent))
        (total (mentor-get-property 'size_bytes torrent)))
    (if (= done total)
        (format "        %-6s" (mentor-bytes-to-human total))
      (format "%6s / %6s"
              (mentor-bytes-to-human done)
              (mentor-bytes-to-human total)))))

(defun mentor-torrent-get-size-done (torrent)
  (mentor-bytes-to-human
   (mentor-get-property 'bytes_done torrent)))

(defun mentor-torrent-get-size-total (torrent)
  (mentor-bytes-to-human
   (mentor-get-property 'size_bytes torrent)))

(defun mentor-torrent-get-tied-file-name (torrent)
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

(defun mentor-torrent-get-views (torrent)
  (mentor-get-property 'views torrent))

(defun mentor-torrent-add-view-prompt (view)
  (interactive "sAdd torrent to view: ")
  (mentor-torrent-add-view (mentor-torrent-at-point) view))

(defun mentor-torrent-add-view (torrent view)
  (if (not (mentor-valid-view-name view))
      (message "Not a valid name for a view!")
    (if (not (mentor-is-view-defined view))
	(if (y-or-n-p (concat "View " view " was not found. Create it? "))
	    (progn (list (mentor-rpc-command 
			  "d.views.push_back_unique" 
			  (mentor-get-property 'hash torrent) view)
			 (mentor-add-view-and-filter view)))
	  (message "Nothing done"))
      (progn (list (mentor-rpc-command 
		    "d.views.push_back_unique" 
		    (mentor-get-property 'hash torrent) view)
		    (mentor-update-view-filter view))))))


;;; View functions

;; TODO find out what a valid name is in rtorrent
(defun mentor-valid-view-name (name)
  t)

(defun mentor-set-view (new)
  (interactive "sSwitch to view: ")
  (when (numberp new)
    (setq new (cdr (assoc new mentor-custom-views))))
  (when (not (equal new mentor-current-view))
    (setq mentor-current-view new)
    (mentor-reload)))

(defun mentor-add-view-and-filter (view)
  "Adds the specified view to rtorrents \"view_list\" and sets
the new views view_filter. SHOULD BE USED WITH CARE! Atleast in
rtorrent 0.8.6, rtorrent crashes if you try to add the same view
twice!"
  (mentor-rpc-command "view_add" view)
  (setq mentor-torrent-views (cons view mentor-torrent-views))
  (mentor-update-view-filter view))

(defun mentor-update-view-filter (view)
  "Updates the view_filter for the specified view. You need to do
this everytime you add/remove a torrent to a view since
rtorrent (atleast as of 0.8.6) does not add/remove new torrents
to a view unless the filter is updated."
  (mentor-rpc-command "view_filter" view
		      (concat "d.views.has=" view)))

(defun mentor-get-and-update-views ()
  "Gets all unique views from torrents, adds all views not
already in view_list and sets all new view_filters."
  (interactive)
  (maphash 
   (lambda (id torrent)
     (mapcar (lambda (view) 
	       (if (not (member view mentor-torrent-views))
		   (mentor-add-view-and-filter view)))
	     (cdr (assoc 'views torrent))))
   mentor-torrents))

(defun mentor-get-view-list ()
  (mentor-rpc-command "view_list"))

(defun mentor-is-view-defined (view)
  (member view mentor-torrent-views))


;;; Utility functions

(defun mentor-bytes-to-human (bytes)
  (if bytes
      (let* ((bytes (if (stringp bytes) (string-to-number bytes) bytes))
             (kb 1024.0)
             (mb (* kb 1024.0))
             (gb (* mb 1024.0))
             (tb (* gb 1024.0)))
        (cond ((< bytes 0) "???") ;; workaround for old xmlrpc-c
              ((< bytes kb) bytes)
              ((< bytes mb) (concat (format "%.1f" (/ bytes kb)) "K"))
              ((< bytes gb) (concat (format "%.1f" (/ bytes mb)) "M"))
              ((< bytes tb) (concat (format "%.1f" (/ bytes gb)) "G"))
              (t "1TB+")))
    ""))

(defun mentor-bytes-to-kilobytes (bytes)
  (if (numberp bytes)
      (if (< bytes 0)
          "???" ;; workaround for old xmlrpc-c
        (number-to-string (/ bytes 1024)))
    ""))

(defun mentor-enforce-length (str len)
  (substring (format (concat "%" (when (< len 0) "-")
                             (number-to-string (abs len)) "s")
                     str)
             0 (abs len)))

(defun mentor-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
	(substring str 0 (- (length str) 1))
      str)))

(provide 'mentor)

;;; mentor.el ends here
