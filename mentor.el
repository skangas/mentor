;;; mentor.el --- Control rtorrent from GNU Emacs

;; Copyright (C) 2010, 2011 Stefan Kangas.
;; Copyright (C) 2011 David Spångberg.

;; Author: Stefan Kangas <skangas@skangas.se>
;; Version: 0.1
;; Keywords: bittorrent, rtorrent

(defconst mentor-version "0.1"
  "The version of Mentor that you're using.")

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a GNU Emacs interface for the `rtorrent' bittorrent client.  It uses
;; XML-RPC to communicate with rtorrent, and needs rtorrent to be configured
;; accordingly.  This project aims to provide a feature complete interface.

;; TODO:
;; Filters
;; Sort according to column, changable with < and >
;; Customizable fonts
;; Save cache to disk

;; Bug reports, comments, and suggestions are welcome!

;;; Code:
(eval-when-compile (require 'cl))
(require 'url-scgi)
(require 'xml-rpc)


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

(defcustom mentor-custom-views 
  '((1 . "main") (2 . "main") (3 . "started")
    (4 . "stopped") (5 . "complete") (6 . "incomplete")
    (7 . "hashing") (8 . "seeding") (9 . "active"))
  "A list of mappings \"(BINDING . VIEWNAME)\" where BINDING is
the key to which the specified view will be bound to."
  :group 'mentor
  :type '(alist :key-type integer :value-type string))

(defcustom mentor-default-view "main"
  "The default view to use when browsing torrents."
  :group 'mentor
  :type 'string)

(defcustom mentor-directory-prefix ""
  "Prefix to use before all directories. (Hint: If your rtorrent
process is running on a remote host, you could set this to
something like `/ssh:user@example.com:'.)"
  :group 'mentor
  :type 'string)

(defcustom mentor-highlight-enable nil
  "If non-nil, highlight the line of the current torrent."
  :group 'mentor
  :type 'boolean)

(defcustom mentor-rtorrent-url "scgi://localhost:5000"
  "The URL to the rtorrent client. Can either be on the form
scgi://HOST:PORT or http://HOST[:PORT]/PATH depending on if you are
connecting through scgi or http."
  :group 'mentor
  :type 'string)

(defcustom mentor-view-columns
  '(((mentor-torrent-get-prio) -5 "Pri")
    ((mentor-torrent-get-progress) -5 "Cmp")
    ((mentor-torrent-get-state) -3 "State")
    (name -80 "Name")
    ((mentor-torrent-get-speed-up) -6 "Up")
    ((mentor-torrent-get-speed-down) -6 "Down")
    ((mentor-torrent-get-size) -15 "     Size")
    (message -40 "Message")
    (directory -100 "Directory")
    (tied_to_file -80 "Tied file name"))
  "A list of all columns to show in mentor view."
  :group 'mentor
  :type '(repeat (list symbol integer string)))

(defface mentor-highlight-face
  '((((class color) (background light))
     :background "gray13")
    (((class color) (background dark))
     :background "dark goldenrod"))
  "Face for highlighting the current torrent."
  :group 'mentor)

(defface mentor-marked-item
  '((t :inherit font-lock-warning-face))
  "Face used for marked items."
  :group 'mentor)
(defvar mentor-marked-item-face 'mentor-marked-item)

(defface mentor-directory-face
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting directories."
  :group 'mentor)

(defvar mentor-default-item-faces
  '((torrent . nil) (file . nil) (dir . mentor-directory-face))
  "An alist with the default face for item types.")


;;; major mode

(defvar mentor-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)

    ;; torrent list actions
    (define-key map (kbd "DEL") 'mentor-add-torrent)
    (define-key map (kbd "g") 'mentor-update)
    (define-key map (kbd "G") 'mentor-reload)
    (define-key map (kbd "M-g") 'mentor-update-torrent-data-and-redisplay)

    ;; navigation
    (define-key map (kbd "<up>") 'mentor-previous-item)
    (define-key map (kbd "<down>") 'mentor-next-item)
    (define-key map (kbd "p") 'mentor-previous-item)
    (define-key map (kbd "n") 'mentor-next-item)

    ;; single torrent actions
    (define-key map (kbd "+") 'mentor-increase-priority)
    (define-key map (kbd "-") 'mentor-decrease-priority)
    ;; (define-key map (kbd "C") 'mentor-call-command)
    (define-key map (kbd "C") 'mentor-copy-torrent-data)
    (define-key map (kbd "K") 'mentor-erase-torrent-and-data)
    (define-key map (kbd "R") 'mentor-move-torrent-data)
    (define-key map (kbd "b") 'mentor-set-inital-seeding)
    (define-key map (kbd "e") 'mentor-recreate-files) ;; Set the 'create/resize queued' flags on all files in a torrent.
    (define-key map (kbd "o") 'mentor-change-target-directory)
    (define-key map (kbd "d") 'mentor-stop-torrent)
    (define-key map (kbd "k") 'mentor-erase-torrent)
    (define-key map (kbd "r") 'mentor-hash-check-torrent)
    (define-key map (kbd "s") 'mentor-start-torrent)

    ;; misc actions
    (define-key map (kbd "RET") 'mentor-torrent-detail-screen)
    (define-key map (kbd "TAB") 'mentor-toggle-object)
    (define-key map (kbd "m") 'mentor-mark-item)
    (define-key map (kbd "u") 'mentor-unmark-item)
    (define-key map (kbd "M") 'mentor-mark-all)
    (define-key map (kbd "U") 'mentor-unmark-all)
    (define-key map (kbd "v") 'mentor-view-in-dired)

    ;; sort functions
    (define-key map (kbd "t c") 'mentor-sort-by-state)
    (define-key map (kbd "t D") 'mentor-sort-by-directory)
    (define-key map (kbd "t d") 'mentor-sort-by-download-speed)
    (define-key map (kbd "t n") 'mentor-sort-by-name)
    (define-key map (kbd "t p") 'mentor-sort-by-property-prompt)
    (define-key map (kbd "t s") 'mentor-sort-by-size)
    (define-key map (kbd "t t") 'mentor-sort-by-tied-file-name)
    (define-key map (kbd "t u") 'mentor-sort-by-upload-speed)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "Q") 'mentor-shutdown-rtorrent)
    
    ;; view bindings
    (define-key map (kbd "a") 'mentor-add-torrent-to-view)
    (define-key map (kbd "w") 'mentor-switch-to-view)
    (define-key map (kbd "1") (lambda () (interactive) (mentor-switch-to-view 1)))
    (define-key map (kbd "2") (lambda () (interactive) (mentor-switch-to-view 2)))
    (define-key map (kbd "3") (lambda () (interactive) (mentor-switch-to-view 3)))
    (define-key map (kbd "4") (lambda () (interactive) (mentor-switch-to-view 4)))
    (define-key map (kbd "5") (lambda () (interactive) (mentor-switch-to-view 5)))
    (define-key map (kbd "6") (lambda () (interactive) (mentor-switch-to-view 6)))
    (define-key map (kbd "7") (lambda () (interactive) (mentor-switch-to-view 7)))
    (define-key map (kbd "8") (lambda () (interactive) (mentor-switch-to-view 8)))
    (define-key map (kbd "9") (lambda () (interactive) (mentor-switch-to-view 9)))
    map))

(defvar mentor-mode-hook)

(defvar mentor-auto-update-buffers)

(defvar mentor-auto-update-timer nil)

(defvar mentor-current-view)

(defvar mentor-header-line)
(make-variable-buffer-local 'mentor-header-line)

(defvar mentor-rtorrent-client-version)
(make-variable-buffer-local 'mentor-rtorrent-client-version)

(defvar mentor-rtorrent-library-version)
(make-variable-buffer-local 'mentor-rtorrent-library-version)

(defvar mentor-rtorrent-name)
(make-variable-buffer-local 'mentor-rtorrent-name)

(defvar mentor-sort-list '((name) (up_rate . t)))
(make-variable-buffer-local 'mentor-sort-list)

(defvar mentor-view-torrent-list nil
  "alist of torrents in given views")



;; Variables that should be changed by sub-modes

(defvar mentor-sub-mode nil
  "The submode which is currently active")
(make-variable-buffer-local 'mentor-sub-mode)
(put 'mentor-sub-mode 'permanent-local t)

(defvar mentor-priority-fun)
(make-variable-buffer-local 'mentor-priority-fun)



;; Mentor major-mode

(define-derived-mode mentor-mode special-mode "mentor"
  "Major mode for controlling rtorrent from emacs

Type \\[mentor] to start Mentor.

\\{mentor-mode-map}"
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (kill-all-local-variables)
  (setq major-mode 'mentor-mode
        mode-name "mentor"
        buffer-read-only t
        truncate-lines t)
  (set (make-local-variable 'line-move-visual) t)
  (setq mentor-current-view mentor-default-view
        mentor-torrents (make-hash-table :test 'equal))
  (add-hook 'post-command-hook 'mentor-post-command-hook t t)
  ;;(set (make-local-variable 'revert-buffer-function) 'mentor-revert)
  (use-local-map mentor-mode-map)
  (run-mode-hooks 'mentor-mode-hook)
  (if (and (not mentor-auto-update-timer) mentor-auto-update-interval)
      (setq mentor-auto-update-timer
            (run-at-time t mentor-auto-update-interval
                         'mentor-auto-update-timer))))

;;;###autoload
(defun mentor ()
  (interactive)
  (progn (switch-to-buffer (get-buffer-create "*mentor*"))
         (mentor-mode)
         (mentor-init-header-line)
         (setq mentor-priority-fun 'mentor-torrent-priority-fun)
         (setq mentor-rtorrent-client-version (mentor-rpc-command "system.client_version")
               mentor-rtorrent-library-version (mentor-rpc-command "system.library_version")
               mentor-rtorrent-name (mentor-rpc-command "get_name"))
         (mentor-set-view mentor-default-view)
         (when (equal mentor-current-view mentor-last-used-view)
           (setq mentor-last-used-view (mentor-get-custom-view-name 2)))
         (mentor-init-torrent-list)
         (mentor-views-init)
         (mentor-redisplay)
         (beginning-of-buffer)))

(defun mentor-post-command-hook ()
  (when mentor-highlight-enable
    (mentor-highlight-torrent)))

(defun mentor-init-header-line ()
  (setq header-line-format
        '(:eval (concat
                 (propertize " " 'display '((space :align-to 2)))
                 (substring mentor-header-line
                            (min (length mentor-header-line)
                                 (window-hscroll)))))))

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



;;; XML-RPC calls

(defun mentor-rpc-method-to-property (name)
  (intern
   (replace-regexp-in-string "^[df]\\.\\(get_\\)?\\|=$" "" name)))

;; Do not try methods that makes rtorrent crash
(defvar mentor-method-exclusions-regexp "d\\.get_\\(mode\\|custom.*\\|bitfield\\)")

(defvar mentor-rtorrent-rpc-methods nil)

(defun mentor-rpc-list-methods (&optional regexp)
  "system.listMethods
Returns a list of all available commands.  First argument is
interpreted as a regexp, and if specified only returns matching
functions"
  (when (not mentor-rtorrent-rpc-methods)
    (let ((methods (mentor-rpc-command "system.listMethods")))
      (setq mentor-rtorrent-rpc-methods
            (delq nil
                  (mapcar (lambda (m)
                            (when (not (string-match mentor-method-exclusions-regexp m))
                              m))
                          methods)))))
  (if regexp
      (delq nil (mapcar (lambda (m)
                          (when (string-match regexp m)
                            m))
                        mentor-rtorrent-rpc-methods))
    mentor-rtorrent-rpc-methods))

(defun mentor-rpc-command (&rest args)
  "Run command as an XML-RPC call via SCGI or http."
  (let* ((url-http-response-status 200)
         (response (apply 'xml-rpc-method-call mentor-rtorrent-url args)))
    (if (equal response '((nil . "URL/HTTP Error: 200")))
        (error "mentor, unable to connect: %s" mentor-rtorrent-url)
      response)))

(defun mentor-multicall-string (method &rest args)
  (list (cons "methodName" method) (cons "params" args)))

(defun mentor-sys-multicall (&rest calls)
  "Perform a system.multicall with `calls'.  Every call should be
a list where the first element is the method name and all
consecutive elements is its arguments."
  (mentor-rpc-command
   "system.multicall"
   (mapcar (lambda (c)
             (apply 'mentor-multicall-string 
                    (car c) (cdr c))) calls)))


;;; Main view

;; (defvar *mentor-update-time* (current-time))

(defmacro mentor-keep-position (&rest body)
  "Keep the current position."
  `(let ((kept-torrent-id (mentor-item-id-at-point))
         (kept-point (point)))
     ,@body
     (if kept-torrent-id
         (condition-case err
             (mentor-goto-torrent kept-torrent-id)
           (mentor-missing-torrent
            (goto-char kept-point)))
       (goto-char kept-point))))

(defun mentor-update ()
  "Update all torrents and redisplay."
  (interactive)
  (cond ((eq mentor-sub-mode 'file-details) (mentor-details-files-update))
        ((not mentor-sub-mode)
         (mentor-keep-position
          (when (mentor-views-is-custom-view mentor-current-view)
            (mentor-views-update-filter mentor-current-view))
          (mentor-update-interesting-torrent-data)
          (mentor-redisplay)))))

(defun mentor-reload ()
  "Re-initialize all torrents and redisplay."
  (interactive)
  (cond ((eq mentor-sub-mode 'file-details) (mentor-details-files-update t))
        ((not mentor-sub-mode)
         (mentor-keep-position
          (when (mentor-views-is-custom-view mentor-current-view)
            (mentor-views-update-filter mentor-current-view))
          (setq mentor-torrents (make-hash-table :test 'equal))
          (mentor-init-torrent-list)
          (mentor-redisplay)))))

(defun mentor-redisplay ()
  "Redisplay the mentor torrent view buffer."
  (interactive)
  (mentor-reload-header-line)
  (when (equal major-mode 'mentor-mode)
    (save-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mentor-insert-torrents)
        (end-of-buffer)
        (insert "\nmentor-" mentor-version " - rTorrent "
                mentor-rtorrent-client-version "/"
                mentor-rtorrent-library-version
                " (" mentor-rtorrent-name ")\n")))))

(defun mentor-insert-torrent (id)
  (let* ((torrent (mentor-get-torrent id))
         (text (mentor-process-view-columns torrent mentor-view-columns))
         (marked (mentor-property 'marked torrent)))
    (insert (propertize text
                        'marked marked
                        'field id
                        'collapsed t
                        'type 'torrent) "\n")
    (when marked
      (save-excursion
        (mentor-previous-item)
        (mentor-mark-item)))))

(defun mentor-insert-torrents ()
  (let ((tor-ids (cdr (assoc (intern mentor-current-view)
                             mentor-view-torrent-list))))
    (dolist (id tor-ids)
      (mentor-insert-torrent id))
    (when (> (length tor-ids) 0)
      (mentor-sort))))

(defun mentor-redisplay-torrent ()
  (let ((inhibit-read-only t)
        (id (mentor-item-id-at-point)))
    (mentor-remove-item-from-view)
    (mentor-insert-torrent id)
    (mentor-previous-item)))

(defun mentor-process-columns-helper (cols lenfun strfun)
  (mapcar (lambda (column)
            (let* ((len (funcall lenfun column))
                   (str (funcall strfun column)))
              (concat (mentor-enforce-length str len) " ")))
          cols))

(defun mentor-process-view-header-columns (cols)
  (apply 'concat
         (mentor-process-columns-helper
          cols
          (lambda (col) (or (cadddr col)
                            (cadr col)))
          (lambda (col) (caddr column)))))

(defun mentor-process-view-columns (item cols)
  (apply 'concat " "
         (mentor-process-columns-helper
          cols
          (lambda (col) (cadr col))
          (lambda (col)
            (let ((prop (car col)))
              (if (not prop)
                  ""
                (if (listp prop)
                    (apply (car prop) item (cdr prop))
                  (mentor-property prop item))))))))

(defun mentor-reload-header-line ()
  (cond ((eq mentor-sub-mode 'file-details)
         (setq mentor-header-line
               (mentor-process-view-header-columns mentor-file-detail-columns)))
        ((not mentor-sub-mode)
         (setq mentor-header-line
               (mentor-process-view-header-columns mentor-view-columns)))))

(defvar mentor-highlight-overlay nil)
(make-variable-buffer-local 'mentor-highlight-overlay)

(defvar mentor-highlighted-torrent nil)
(make-variable-buffer-local 'mentor-highlighted-torrent)

(defun mentor-highlight-torrent ()
  (let ((cur (mentor-item-id-at-point)))
    (when (not mentor-highlight-overlay)
      (setq mentor-highlight-overlay (make-overlay 1 10))
      (overlay-put mentor-highlight-overlay 
                   'face 'mentor-highlight-face))
    (if cur
        (when (not (equal cur mentor-highlighted-torrent))
          (setq mentor-highlighted-torrent cur)
          (move-overlay mentor-highlight-overlay
                        (mentor-get-item-beginning)
                        (mentor-get-item-end)
                        (current-buffer)))
      (delete-overlay mentor-highlight-overlay)
      (setq mentor-highlighted-torrent nil))))



;;; Sorting

(defun mentor-do-sort (property &optional reverse)
  (mentor-keep-position
   (goto-char (point-min))
   (save-excursion
     (let ((sort-fold-case t)
           (inhibit-read-only t))
       (sort-subr reverse
                  (lambda () (ignore-errors (mentor-next-item t)))
                  (lambda () (ignore-errors (mentor-end-of-item)))
                  (lambda () (mentor-property property)))))))

(defun mentor-sort (&optional property reverse append)
  "Sort the mentor torrent buffer.
Defaults to sorting according to `mentor-sort-list'.

PROPERTY gives according to which property the torrents should be
sorted.

If REVERSE is non-nil, the result of the sort is reversed.

When APPEND is non-nil, instead of sorting directly, add the
result to the end of `mentor-sort-list'.  This means we can sort
according to several criteria."
  (when property
    (let ((elem (cons property reverse)))
      (if append
          (add-to-list 'mentor-sort-list elem t)
        (setq mentor-sort-list (list elem)))))
  (dolist (property mentor-sort-list)
    (let ((prop (car property))
          (rev (cdr property)))
      (mentor-do-sort prop rev))))

(defun mentor-sort-by-directory (append)
  (interactive "P")
  (mentor-sort 'directory nil append))

(defun mentor-sort-by-download-speed (append)
  (interactive "P")
  (mentor-sort 'down_rate t append))

(defun mentor-sort-by-name (append)
  (interactive "P")
  (mentor-sort 'name nil append))

(defun mentor-sort-by-state (append)
  (interactive "P")
  (mentor-sort 'state nil append))

(defun mentor-sort-by-tied-file-name (append)
  (interactive "P")
  (mentor-sort 'tied_to_file nil append))

(defun mentor-sort-by-size (append)
  (interactive "P")
  (mentor-sort 'size_bytes t append))

(defun mentor-sort-by-upload-speed (append)
  (interactive "P")
  (mentor-sort 'up_rate t append))


;;; Get torrent

(defmacro mentor-use-tor (&rest body)
  "Convenience macro to use either the defined `torrent' value or
the torrent at point."
  `(let ((tor (or (when (boundp 'tor) tor)
                  (mentor-get-torrent (mentor-item-id-at-point))
                  (error "no torrent"))))
     ,@body))


;;; Navigation

(defun mentor-item-id-at-point ()
  (get-text-property (point) 'field))

(defmacro mentor-while-same-item (skip-blanks condition &rest body)
  `(let* ((item (mentor-item-id-at-point)))
     (while (and ,condition
                 (or (and ,skip-blanks
                          (not (mentor-item-id-at-point)))
                     (equal item (mentor-item-id-at-point))))
       ,@body)))

(defun mentor-beginning-of-item (&optional real-start)
  "Goto the beginning of the item at point. If the item at point
has an item-start property defined and real-start is nil goto
that point. Otherwise goto the real start point."
  (interactive)
  (let ((start (or (get-text-property (point) 'item-start)
                   (field-beginning nil nil (point-at-bol)))))
    (when start
      (goto-char start))))

(defun mentor-end-of-item ()
  "Goto the end of the item at point."
  (interactive)
  (mentor-while-same-item nil (< (point) (point-max)) (forward-char)))

(defun mentor-get-item-beginning (&optional real-start)
  "If real-start is nil and the item at point has a item-start
property defined return that point. Otherwise return the real
start point."
  (save-excursion
    (mentor-beginning-of-item real-start)
    (point)))

(defun mentor-get-item-end ()
  (save-excursion
    (mentor-end-of-item)
    (point)))

(defun mentor-next-item (&optional no-wrap)
  (interactive)
  (condition-case err
      (mentor-while-same-item t t (forward-char))
    (end-of-buffer
     (when (not no-wrap)
       (goto-char (point-min))
       (when (not (mentor-item-type))
         (mentor-next-item t)))))
  (mentor-beginning-of-item))

(defun mentor-previous-item (&optional no-wrap)
  (interactive)
  (condition-case err
      (mentor-while-same-item t t (backward-char))
    (beginning-of-buffer
     (when (not no-wrap)
       (goto-char (point-max))
       (mentor-previous-item t))))
  (mentor-beginning-of-item t))

(put 'mentor-missing-torrent
     'error-conditions
     '(error mentor-error mentor-missing-torrent))

(defun mentor-remove-item-from-view ()
  (let ((inhibit-read-only t))
    (delete-region (mentor-get-item-beginning t)
                   (+ 1 (mentor-get-item-end)))))

(defun mentor-goto-torrent (id)
  (let ((pos (save-excursion
               (beginning-of-buffer)
               (while (and (not (equal id (mentor-item-id-at-point)))
                           (not (= (point) (point-max))))
                 (mentor-next-item t))
               (point))))
    (if (not (= pos (point-max)))
        (goto-char pos)
      (signal 'mentor-missing-torrent `("No such torrent" ,id)))))

;; ??? what to do
(defun mentor-toggle-object ()
  (interactive)
  (let ((type (get-text-property (point) 'type))
        (props (text-properties-at (point))))
    (cond ((eq type 'dir) 
           (mentor-toggle-file (get-text-property (point) 'file))))))


;;; Torrent actions and helper functions for interactive commands

(defun mentor-delete-file (file)
  (let ((dired-recursive-deletes nil))
    (message "Deleting %s" file)
    (condition-case err
        (dired-delete-file file)
      (file-error nil))))

(defun mentor-do-erase-data (tor)
  (let* ((hash (mentor-property 'hash tor))
           (base-path (mentor-property 'base_path tor))
           (files (mentor-torrent-get-file-list tor))
           (dirs nil))
      (if (= 0 (mentor-property 'is_multi_file tor))
          (mentor-delete-file base-path)
        (progn
          (dolist (file files)
            (let* ((file (mapconcat 'identity (apply 'list base-path (car file)) "/"))
                   (dir (file-name-directory file)))
              (mentor-delete-file file)
              (setq dirs (adjoin dir dirs :test 'equal))))
          (dolist (dir dirs)
            (mentor-delete-file dir))))))

(defun mentor-do-erase-torrent (tor)
  (mentor-rpc-command "d.erase" (mentor-property 'hash tor))
  (remhash (mentor-property 'local_id tor) mentor-torrents)
  (mentor-view-torrent-list-delete-all tor))

(defun mentor-do-start-torrent (tor)
  (mentor-rpc-command "d.start" (mentor-property 'hash tor)))

(defun mentor-do-stop-torrent (tor)
  (mentor-rpc-command "d.stop" (mentor-property 'hash tor)))

(defun mentor-get-old-torrent-path (tor)
  (let ((path (mentor-property 'base_path tor)))
    (when (not path)
      (error "Unable to get path for closed torrent"))
    (substring (directory-file-name path)
               0 (- (length (file-name-nondirectory path))))))

(defun mentor-get-new-torrent-path (tor)
  "Helper function for `mentor-copy-torrent-data' and
`mentor-move-torrent-data'"
  (let* ((old (mentor-get-old-torrent-path tor))
         (old-prefixed (concat mentor-directory-prefix old))
         (new (read-file-name "New path: " old-prefixed nil t)))
    (when (string-equal old new)
      (error "Source and destination are the same"))
    (when (not (condition-case err
                   (mentor-rpc-command "execute" "ls" "-d" new)
                 (error nil)))
      (error "No such file or directory: " new))
    new))

(defun mentor-set-priority (val)
  (setq val (or val 1))
  (apply 'mentor-rpc-command (funcall mentor-priority-fun val)))


;;; Interactive torrent commands

(defun mentor-add-torrent ()
  (interactive)
  (message "TODO: mentor-add-torrent"))

(defun mentor-call-command (&optional tor)
  (interactive)
  (message "TODO"))

(defun mentor-change-target-directory (&optional tor)
  "Change torrents target directory without moving data.
See also `mentor-move-torrent-data'."
  (interactive)
  (mentor-keep-position
   (mentor-use-tor
    (let* ((new (mentor-get-new-torrent-path tor)))
      (mentor-do-stop-torrent tor)
      (mentor-rpc-command "d.set_directory" (mentor-property 'hash) new)
      (mentor-update-torrent-data-and-redisplay)
      (message (concat "Changed target directory to " new))))))

(defun mentor-decrease-priority (&optional tor)
  (interactive)
  (mentor-do-marked
   (mentor-set-priority -1)
   (mentor-update-torrent-data-and-redisplay)))

(defun mentor-increase-priority (&optional tor)
  (interactive)
  (mentor-do-marked
   (mentor-set-priority 1)
   (mentor-update-torrent-data-and-redisplay)))

(defun mentor-erase-torrent (&optional tor)
  (interactive)
  (mentor-use-tor
   (when (yes-or-no-p (concat "Remove torrent " (mentor-property 'name tor) " "))
     (mentor-do-erase-torrent tor)
     (mentor-remove-item-from-view))))

(defun mentor-erase-torrent-and-data (&optional tor)
  (interactive)
  (mentor-use-tor
   (mentor-torrent-get-file-list) ;; populate it before erasing torrent
   (let* ((name (mentor-property 'name tor))
          (confirm-tor
           (yes-or-no-p (concat "Remove torrent " name " ")))
          (confirm-data
           (and confirm-tor
                (yes-or-no-p (concat "Remove data for " name " ")))))
     (when confirm-tor
       (mentor-do-erase-torrent tor)
       (mentor-remove-item-from-view))
     (when confirm-data
       (mentor-do-erase-data tor)))))

(defun mentor-hash-check-torrent (&optional tor)
  (interactive)
  (mentor-keep-position
   (mentor-use-tor
    (mentor-rpc-command "d.check_hash" (mentor-property 'hash tor))
    (mentor-set-property 'hashing 1)
    (mentor-set-property 'is_open 1)
    (mentor-update-torrent-data-and-redisplay))))

(defun mentor-copy-torrent-data (&optional tor)
  (interactive)
  (mentor-keep-position
   (mentor-use-tor
    (let* ((old (mentor-property 'base_path))
           (new (mentor-get-new-torrent-path tor)))
      (when (file-exists-p old)
        (mentor-rpc-command "execute" "cp" "-Rn" old new))
      (message (concat "Copied torrent data to " new))))))

(defun mentor-move-torrent-data (&optional tor)
  (interactive)
  (mentor-keep-position
   (mentor-use-tor
    (let* ((old (mentor-property 'base_path))
           (new (mentor-get-new-torrent-path tor))
           (was-started (= 1 (mentor-property 'is_active))))
      (when was-started
        (mentor-do-stop-torrent tor))
      (when (file-exists-p old)
        (mentor-rpc-command "execute" "mv" "-n" old new))
      (mentor-rpc-command "d.set_directory" (mentor-property 'hash) new)
      (when was-started
        (mentor-do-start-torrent tor))
      ;;; FIXME: needs to update the data for this torrent from rtorrent
      (mentor-set-property 'directory new)
      (mentor-update-torrent-data-and-redisplay)
      (message (concat "Moved torrent data to " new))))))

(defun mentor-pause-torrent (&optional tor)
  "Pause torrent. This is probably not what you want, use
`mentor-stop-torrent' instead."
  (interactive)
  (mentor-use-tor
   (mentor-rpc-command "d.pause" (mentor-property 'hash tor))
   (mentor-update-torrent-data-and-redisplay)))

(defun mentor-resume-torrent (&optional tor)
  "Resume torrent. This is probably not what you want, use
`mentor-start-torrent' instead."
  (interactive)
  (mentor-use-tor
   (mentor-rpc-command "d.resume" (mentor-property 'hash tor))
   (mentor-update-torrent-data-and-redisplay)))

(defun mentor-start-torrent (&optional tor)
  (interactive)
  (mentor-use-tor
   (mentor-keep-position
    (mentor-do-start-torrent tor)
    (mentor-update-torrent-data-and-redisplay))))

(defun mentor-stop-torrent (&optional tor)
  (interactive)
  (mentor-use-tor
   (mentor-do-stop-torrent tor)
   (mentor-update-torrent-data-and-redisplay)))

(defun mentor-open-torrent (&optional tor)
  (interactive)
  (mentor-use-tor
   (mentor-rpc-command "d.open" (mentor-property 'hash tor))
   (mentor-update-torrent-data-and-redisplay)))

(defun mentor-close-torrent (&optional tor)
  (interactive)
  (mentor-use-tor
   (mentor-rpc-command "d.close" (mentor-property 'hash tor))
   (mentor-update-torrent-data-and-redisplay)))

(defun mentor-recreate-files (&optional tor)
  (interactive)
  (message "TODO: mentor-recreate-files"))

(defun mentor-set-inital-seeding (&optional tor)
  (interactive)
  (message "TODO: set-inital-seeding"))

(defun mentor-update-torrent-data-and-redisplay (&optional tor)
  (interactive)
  (mentor-use-tor
   (mentor-update-torrent-data tor))
  (mentor-use-tor
   (mentor-redisplay-torrent)))

(defun mentor-view-in-dired (&optional tor)
  (interactive)
  (mentor-use-tor
   (let* ((path (mentor-property 'base_path))
          (is-multi-file (mentor-property 'is_multi_file))
          (loc (if (= 1 is-multi-file)
                   path
                 (file-name-directory path))))
     (if loc
         (progn
           (find-file loc)
           (when (= is-multi-file 0)
             (dired-goto-file path)))
       (message "Torrent has no data: %s" (mentor-property 'name))))))


;;; Get torrent data from rtorrent

(defvar mentor-torrents nil
  "Hash table containing all torrents")
(make-variable-buffer-local 'mentor-torrents)

(defvar mentor-d-interesting-methods
  '("d.get_base_path"
    "d.get_bytes_done"
    "d.get_directory"
    "d.get_down_rate"
    "d.get_hashing"
    "d.get_hashing_failed"
    "d.get_priority"
    "d.get_chunk_size"
    "d.get_up_rate"
    "d.get_up_total"
    "d.get_state"
    "d.views"
    "d.is_active"
    "d.is_hash_checked"
    "d.is_hash_checking"
    "d.is_open"
    "d.is_pex_active"))

;; (defun mentor-property-to-rpc-method () nil)

(defun mentor-update-torrent-data (tor)
  (let* ((hash (mentor-property 'hash tor))
         (id (mentor-property 'local_id tor)))
    (dolist (method mentor-d-interesting-methods)
      (let ((property (mentor-rpc-method-to-property method))
            (new-value (mentor-rpc-command method hash)))
        (setcdr (assq property tor) new-value)))
    (puthash id tor mentor-torrents)))

(defun mentor-view-torrent-list-add (tor)
  (let* ((id (mentor-property 'local_id tor))
         (view (intern mentor-current-view))
         (list (assq view mentor-view-torrent-list)))
    (push id (cdr list))))

(defun mentor-view-torrent-list-delete (tor &optional view)
  (let* ((id (mentor-property 'local_id tor))
         (view (or view (intern mentor-current-view)))
         (list (assq view mentor-view-torrent-list)))
    (delete id list)))

(defun mentor-view-torrent-list-delete-all (tor)
  (dolist (view mentor-view-torrent-list)
    (mentor-view-torrent-list-delete tor (car view))))

(defun mentor-view-torrent-list-clear ()
  (let ((view (intern mentor-current-view)))
    (setq mentor-view-torrent-list
          (assq-delete-all view mentor-view-torrent-list))
    (setq mentor-view-torrent-list
          (cons (list view) mentor-view-torrent-list))))

(defconst mentor-methods-to-prefix-with-cat
  (regexp-opt '("bytes_done" "completed_bytes"
                "left_bytes" "size_bytes" "chunk_size"
                "completed_chunks" "size_chunks"))
  "Methods that should be prefixed with cat= when fetched.")

(defun mentor-prefix-method-with-cat (method)
  "Used to get some properties as a string, since older versions
of libxmlrpc-c cannot handle integers longer than 4 bytes."
  (let ((re (concat "\\(?:[df]\\.get_"
                    mentor-methods-to-prefix-with-cat
                    "\\)")))
    (if (string-match re method)
        (concat "cat=$" method)
      method)))

(defun create-torrent (properties values)
  (mapcar* 
   (lambda (p v)
     (cons p
           (if (string-match mentor-methods-to-prefix-with-cat
                             (symbol-name p))
               (string-to-number v)
             v)))
   properties values))

(defun mentor-rpc-d.multicall (methods)
  (let* ((methods+ (mapcar 'mentor-prefix-method-with-cat methods))
         (methods= (mapcar (lambda (m) (concat m "=")) methods+))
         (value-list (apply 'mentor-rpc-command "d.multicall" mentor-current-view methods=))
         (properties (mapcar 'mentor-rpc-method-to-property methods)))
    (mentor-view-torrent-list-clear)
    (mapcar (lambda (values)
              (let ((tor (create-torrent properties values)))
                (mentor-view-torrent-list-add tor)
                tor))
            value-list)))

(put 'mentor-need-init
     'error-conditions
     '(error mentor-error mentor-need-init))

(defun mentor-update-interesting-torrent-data ()
  (interactive)
  (message "Updating torrent data...")
  (condition-case err
      (progn
        (let* ((methods (cons "d.get_local_id" mentor-d-interesting-methods))
               (torrents (mentor-rpc-d.multicall methods)))
          (dolist (tor-new torrents)
            (let* ((id (mentor-property 'local_id tor-new))
                   (tor-old (mentor-get-torrent id)))
              (dolist (row tor-new)
                (let* ((property (car row))
                       (value (cdr row))
                       (alist (assq property tor-old)))
                  (if alist
                      (setcdr alist value)
                    (signal 'mentor-need-init `("No such torrent" ,id))))))))
        (message "Updating torrent data...DONE"))
    (mentor-need-init
     (mentor-init-torrent-list))))

(defun mentor-init-torrent-list ()
  "Initialize torrent list from rtorrent.

All torrent information will be re-fetched, making this an
expensive operation."
  (message "Initializing torrent list...")
  (let* ((methods (mentor-rpc-list-methods "^d\\.\\(get\\|is\\|views$\\)"))
         (torrents (mentor-rpc-d.multicall methods)))
    (dolist (tor torrents)
      (let ((id (mentor-property 'local_id tor)))
        (mentor-set-property 'marked nil tor)
        (puthash id tor mentor-torrents))))
  (mentor-views-update-views)
  (message "Initializing torrent list... DONE"))


;;; Torrent information

(defun mentor-get-torrent (id)
  (gethash id mentor-torrents))

(defun mentor-property (property &optional tor)
  "Get property for a torrent.
If `torrent' is nil, use torrent at point."
  (mentor-use-tor
   (cdr (assoc property tor))))

(defun mentor-set-property (property val &optional tor)
  "Set a property for a torrent.
If `torrent' is nil, use torrent at point."
  (mentor-use-tor
   (let ((id (mentor-property 'local_id tor)))
     (assq-delete-all property tor)
     (let ((new-torrent (cons (cons property val) tor)))
       (puthash id new-torrent mentor-torrents)))))

(defun mentor-torrent-get-progress (torrent)
  (let* ((donev (mentor-property 'bytes_done torrent))
         (totalv (mentor-property 'size_bytes torrent))
         (done (abs (or donev 0)))
         (total (abs (or totalv 1)))
         (percent (* 100 (/ (+ 0.0 done) total))))
    (if (= (truncate percent) 100)
        ""
      (format "[%2d%%]" percent))))

;; TODO show an "I" for incomplete torrents
(defun mentor-torrent-get-state (&optional torrent)
  (concat
   (or (when (> (mentor-property 'hashing   torrent) 0) "H")
       (if   (= (mentor-property 'is_active torrent) 1) " " "S"))
   (if (= (mentor-property 'is_open torrent) 1) " " "C")))

(defun mentor-torrent-get-speed-down (torrent)
  (mentor-bytes-to-kilobytes
   (mentor-property 'down_rate torrent)))

(defun mentor-torrent-get-speed-up (torrent)
  (mentor-bytes-to-kilobytes
   (mentor-property 'up_rate torrent)))

(defun mentor-torrent-get-size (torrent)
  (let ((done (mentor-property 'bytes_done torrent))
        (total (mentor-property 'size_bytes torrent)))
    (if (= done total)
        (format "         %-.6s" (mentor-bytes-to-human total))
      (format "%6s / %-6s"
              (mentor-bytes-to-human done)
              (mentor-bytes-to-human total)))))

(defun mentor-torrent-get-size-done (torrent)
  (mentor-bytes-to-human
   (mentor-property 'bytes_done torrent)))

(defun mentor-torrent-get-size-total (torrent)
  (mentor-bytes-to-human
   (mentor-property 'size_bytes torrent)))

(defun mentor-torrent-get-file-list (&optional tor)
  (mentor-use-tor
   (let ((id (mentor-property 'local_id tor))
         (hash (mentor-property 'hash tor))
         (files (cdr-safe (assoc 'files tor))))
     (when (not files)
       (progn
         (message "Receiving file list...")
         (setq files (mentor-rpc-command
                      "f.multicall" hash "" "f.get_path_components="))
         (setcdr tor (cons (cons 'files files) (cdr tor)))
         (puthash id tor mentor-torrents)))
     files)))

(defun mentor-torrent-has-view (tor view)
  "Returns t if the torrent has the specified view."
  (member view (mentor-torrent-get-views tor)))

(defun mentor-torrent-get-views (tor)
  (mentor-property 'views tor))

(defun mentor-torrent-get-prio (tor)
  (let ((prio (mentor-property 'priority tor)))
    (cond ((= 0 prio) "off")
          ((= 1 prio) "low")
          ((= 2 prio) "")
          ((= 3 prio) "hig"))))

(defun mentor-torrent-priority-fun (val)
  (mentor-use-tor
   (let ((hash (mentor-property 'hash))
         (prio (mentor-property 'priority)))
     (list "d.set_priority" hash (mentor-limit-num (+ prio val) 0 3)))))


;;; View functions

(defun mentor-add-torrent-to-view (view &optional tor)
  (interactive 
   (list (mentor-prompt-complete "Add torrent to view: " 
                                 (remove-if-not 'mentor-views-is-custom-view 
                                                mentor-torrent-views)
                                 nil mentor-current-view)))
  (mentor-use-tor
   (when (not (mentor-views-is-custom-view view))
     (setq view (concat mentor-custom-view-prefix view)))
   (if (not (mentor-views-valid-view-name view))
       (message "Not a valid name for a view!")
     (if (or (mentor-views-is-view-defined view)
             (when (y-or-n-p (concat "View " view " was not found. Create it? "))
               (mentor-views-add view) t))
         (mentor-rpc-command "d.views.push_back_unique" 
                             (mentor-property 'hash tor) view)
       (message "Nothing done")))))

(defvar mentor-torrent-views)
(make-variable-buffer-local 'mentor-torrent-views)

(defconst mentor-torrent-default-views
  '("main" "name" "started" "stopped" "complete" 
    "incomplete" "hashing" "seeding" "active"))

(defconst mentor-custom-view-prefix "mentor-"
  "The string to add to the view name before adding it to
  rtorrent.")

;; TODO find out what a valid name is in rtorrent
(defun mentor-views-valid-view-name (name)
  t)

(defun mentor-set-view (new)
  (if mentor-current-view
      (setq mentor-last-used-view mentor-current-view)
    (setq mentor-last-used-view mentor-default-view))
  (setq mentor-current-view new)
  (setq mode-line-buffer-identification (concat "*mentor* " mentor-current-view)))

(defun mentor-switch-to-view (&optional new)
  (interactive)
  (when (null new)
    (setq new (mentor-prompt-complete 
               "Show view: " mentor-torrent-views 
               1 mentor-last-used-view)))
  (when (numberp new)
    (setq new (mentor-get-custom-view-name new)))
  (when (not (equal new mentor-current-view))
    (mentor-set-view new)
    (mentor-update)
    (message "Showing view \"%s\"" mentor-current-view)))

(defun mentor-views-add (view)
  "Adds the specified view to rtorrents \"view_list\" and sets
the new views view_filter. SHOULD BE USED WITH CARE! Atleast in
rtorrent 0.8.6, rtorrent crashes if you try to add the same view
twice!"
  (mentor-rpc-command "view_add" view)
  (setq mentor-torrent-views (cons view mentor-torrent-views))
  (mentor-views-update-filter view))

(defun mentor-views-init ()
  "Gets all unique views from torrents, adds all views not
already in view_list and sets all new view_filters."
  ;; should always update the views before potentially adding new ones
  (mentor-views-update-views)
  (maphash 
   (lambda (id torrent)
     (mapcar (lambda (view) 
               (when (and (mentor-views-is-custom-view view)
                          (not (mentor-views-is-view-defined view)))
                 (mentor-views-add view)))
             (cdr (assoc 'views torrent))))
   mentor-torrents))

(defun mentor-views-update-views ()
  "Updates the view list and returns all views defined by
rtorrent."
  (setq mentor-torrent-views (mentor-rpc-command "view_list")))

(defun mentor-views-update-filter (view)
  "Updates the view_filter for the specified view. You need to do
this everytime you add/remove a torrent to a view since
rtorrent (atleast as of 0.8.6) does not add/remove new torrents
to a view unless the filter is updated."
  (mentor-rpc-command "view_filter" view
                      (concat "d.views.has=" view)))

(defun mentor-views-update-filters ()
  "Updates all view_filters for custom views in rtorrent."
  (mapc (lambda (view)
          (when (mentor-views-is-custom-view  view)
            (mentor-views-update-filter view)))
        mentor-torrent-views))

(defun mentor-views-is-view-defined (view)
  (member view mentor-torrent-views))

(defun mentor-views-is-custom-view (view)
  ;;(not (member view mentor-torrent-default-views)))
  (string-match (concat "^" mentor-custom-view-prefix) view))

(defun mentor-views-is-default-view (view)
  (member view mentor-torrent-default-views))


;;; Torrent details screen

(defvar mentor-selected-torrent nil)
(make-variable-buffer-local 'mentor-selected-torrent)
(put 'mentor-selected-torrent 'permanent-local t)

(defvar mentor-selected-torrent-info nil)
(make-variable-buffer-local 'mentor-selected-torrent)
(put 'mentor-selected-torrent 'permanent-local t)

(defvar mentor-torrent-details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") 'mentor-details-next-directory)
    (define-key map (kbd "P") 'mentor-details-previous-directory)
    map)
  "Keymap used in `mentor-torrent-details-mode'.")

(defconst mentor-f-interesting-methods
  '("f.get_priority" "f.get_completed_chunks" "f.get_size_chunks"))
(put 'mentor-f-interesting-methods 'permanent-local t)

(define-minor-mode mentor-torrent-details-mode
  "Minor mode for managing a torrent in mentor."
  :group mentor
  :init-value nil
  :lighter nil
  :keymap mentor-torrent-details-mode-map)

(defstruct mentor-file
  "The datastructure that contains the information about torrent
files.  A mentor-file can be either a regular file or a filename
and if it is the latter it will contain a list of the files it
contain.  If it is a regular file it will contain an id which is
the integer index used by rtorrent to identify this file."
  name show marked size completed_chunks
  size_chunks priority files type id)

(defun mentor-file-at-point ()
  (get-text-property (point) 'file))

(defun mentor-file-is-dir (file)
  (and (mentor-file-p file) (eq 'dir (mentor-file-type file))))

(defun mentor-file-at-point-is-dir ()
  (mentor-file-is-dir (mentor-file-at-point)))

(defun mentor-file-prio-string (file)
  (let ((prio (mentor-file-priority file)))
    (cond ((eq prio 0) "off")
          ((eq prio 1) "")
          ((eq prio 2) "hig"))))

(defun mentor-file-progress (file)
  (let* ((chunk-size (mentor-property
                     'chunk_size mentor-selected-torrent))
         (done (mentor-file-completed_chunks file))
         (size (mentor-file-size_chunks file)))
    (format "%d" (* 100 (/ (+ 0.0 done) size)))))

(defun mentor-file-size (file)
  (let* ((chunk-size (mentor-property
                      'chunk_size mentor-selected-torrent)))
    (mentor-bytes-to-human
     (* chunk-size (mentor-file-size_chunks file)))))

(defun mentor-file-priority-fun (val)
  (let* ((file (mentor-file-at-point))
         (id   (mentor-file-id file))
         (prio (mentor-file-priority file))
         (hash (mentor-property 'hash mentor-selected-torrent)))
    (when (not (mentor-file-is-dir file))
      (list "f.set_priority" hash id (mentor-limit-num (+ prio val) 0 2)))))

(defun mentor-toggle-file (file)
  (interactive)
  (let ((start-point (point)))
    (when (mentor-file-is-dir file)
      (setf (mentor-file-show file)
            (if (mentor-file-show file)
                nil
              t))
      (mentor-details-redisplay))
    (goto-char start-point)))

(defun mentor-file-get-file (dir name)
  "Returns the file with the specified name in the directory
`dir'."
  (let* ((pred (lambda (x) (string= name (mentor-file-name x))))
         (file^ (find-if pred (mentor-file-files dir))))
    (when file^
      file^)))

(defun mentor-file-add-file (dir file)
  "Adds a file to the back of the specified directory."
  (setf (mentor-file-files dir)
        (nconc (mentor-file-files dir) (list file))))

(defun mentor-file-properties (file)
  (let ((face (if (mentor-file-is-dir file)
                  'mentor-directory-face
                nil)))
    (list 'face face
          'type (mentor-file-type file)
          'field (mentor-file-id file)
          'file file
          'show (mentor-file-show file))))

(defun mentor-torrent-detail-screen (&optional tor)
  "Show information about the specified torrent or the torrent at
point."
  (interactive)
  (mentor-use-tor
   (switch-to-buffer "*mentor: torrent details*")
   (setq mentor-sub-mode 'file-details)
   (mentor-mode)
   (mentor-init-header-line)
   (setq mentor-priority-fun 'mentor-file-priority-fun)
   (mentor-torrent-details-mode t)
   (setq mentor-selected-torrent tor)
   (mentor-details-files-update t)
   (mentor-details-redisplay)
   (setq mode-line-buffer-identification (concat "*mentor: torrent details* "
                                                 (mentor-property 'name tor)))
   (if (not (mentor-item-type))
       (mentor-next-item t)
     (mentor-beginning-of-item))))

(defun mentor-details-add-files (name-list)
  (let ((root (make-mentor-file :name "/" :type 'dir :id -1 :show t))
        (all-files (make-hash-table :test 'eql))
        (dir-id -1)
        (file-id -1))
    (dolist (names name-list)
      (let* ((file (pop names))
             (len (length names))
             (last-dir root)
             (curr-dir nil))
        (while (> len 0)
          (if (mentor-file-get-file last-dir file)
              (setq last-dir (mentor-file-get-file last-dir file))
            (setq curr-dir (make-mentor-file :name file
                                             :type 'dir :show nil
                                             :id (decf dir-id)))
            (mentor-file-add-file last-dir curr-dir)
            (setq last-dir curr-dir))
          (setq file (pop names))
          (decf len))
        (setq file (make-mentor-file :name file :type 'file
                                     :id (incf file-id)))
        (mentor-file-add-file last-dir file)
        (puthash file-id file all-files)))
    (setq mentor-selected-torrent-info
          (cons (cons 'root root)
                (cons (cons 'files all-files)
                      mentor-selected-torrent-info)))))

(defun mentor-details-files-update (&optional add-files)
  (interactive)
  (when add-files
    (setq mentor-selected-torrent-info
          (assq-delete-all 'root mentor-selected-torrent-info))
    (setq mentor-selected-torrent-info
          (assq-delete-all 'files mentor-selected-torrent-info)))
  (let* ((tor mentor-selected-torrent)
         (hash (mentor-property 'hash tor))
         (methods mentor-f-interesting-methods)
         (methods+ (mapcar
                    'mentor-prefix-method-with-cat
                    (if add-files
                        (cons "f.get_path_components" methods)
                      methods)))
         (methods= (mapcar (lambda (m) (concat m "=")) methods+))
         (value-list (apply 'mentor-rpc-command
                            "f.multicall" hash "" methods=))
         (properties (mapcar 'mentor-rpc-method-to-property methods)))
    (when add-files
      (mentor-details-add-files (mapcar 'car value-list))
      (setq value-list (mapcar 'cdr value-list)))
    (let ((files (cdr (assq 'files mentor-selected-torrent-info)))
          (id -1))
      (dolist (values value-list)
        (let ((file (gethash (incf id) files)))
          (mapc (lambda (p)
                  (let* ((file-fun (mentor-concat-symbols 'mentor-file- p))
                         (val (if (string-match mentor-methods-to-prefix-with-cat
                                                (symbol-name p))
                                  (string-to-number (pop values))
                                (pop values))))
                    (eval `(setf (,file-fun file) ,val))))
                properties)))))
  (mentor-details-redisplay))

(defvar mentor-file-detail-columns
  '(((mentor-file-progress) -5 "Cmp")
    ((mentor-file-prio-string) -5 "Pri")
    ((mentor-file-size) 6 "Size")
    (nil 0 "File" 6)))
(defvar mentor-file-detail-width 22)

(defun mentor-insert-file (file infix &optional last)
  (interactive)
  (let ((props (mentor-file-properties file))
        (text (mentor-process-view-columns file mentor-file-detail-columns)))
    (insert (apply 'propertize
                   (concat text " " infix (if last "└── " "├── ")
                           (mentor-file-name file))
                   (cons 'item-start (cons (+ 5 (point) (length text) (length infix))
                                           props)))
            "\n")))

(defun mentor-insert-dir-content (dir &optional infix)
  (interactive)
  (let* ((files (mentor-file-files dir))
         (total (length files))
         (infix (or infix ""))
         (count 1))
    (dolist (file files)
      (if (mentor-file-is-dir file)
          (let* ((show (mentor-file-show file))
                 (symb (if show
                           (if (= count total) "└── " "├── ")
                         "+── "))
                 (margin (concat (make-string mentor-file-detail-width ? )
                                 infix
                                 symb))
                 (text (concat margin (mentor-file-name file)))
                 (infix-next (concat infix
                                      (if (= count total)
                                          "    "
                                        "│   "))))
            (insert (apply 'propertize text
                           'item-start (+ (point) (length margin))
                           (mentor-file-properties file)) "\n")
            (when show
              (mentor-insert-dir-content file infix-next)))
        (mentor-insert-file file infix (= count total)))
      (when (mentor-file-marked file)
        (save-excursion
          (mentor-previous-item t)
          (mentor-mark-item)))
      (incf count))))

(defun mentor-details-redisplay ()
  (interactive)
  (let* ((inhibit-read-only t)
         (pos (point))
         (root (cdr (assq 'root mentor-selected-torrent-info))))
    (erase-buffer)
    (mentor-reload-header-line)
    (mentor-insert-dir-content root)
    (goto-char pos)))

(defun mentor-details-next-directory ()
  (interactive)
  (when (mentor-file-is-dir (mentor-file-at-point))
    (mentor-next-item))
  (while (not (mentor-file-is-dir (mentor-file-at-point)))
    (mentor-next-item))
  (mentor-beginning-of-item))

(defun mentor-details-previous-directory ()
  (interactive)
  (when (mentor-file-is-dir (mentor-file-at-point))
    (mentor-previous-item))
  (while (not (mentor-file-is-dir (mentor-file-at-point)))
    (mentor-previous-item)
    (mentor-beginning-of-item)))

(defun mentor-mark-dir (file &optional clear-mark no-redisplay)
  (interactive)
  (when (not (mentor-file-show file))
    (setf (mentor-file-show file) t))
  (dolist (curr-file (mentor-file-files file))
    (let ((curr-file curr-file)
          (new-mark (if clear-mark nil t)))
      (if (mentor-file-is-dir curr-file)
          (mentor-mark-dir curr-file clear-mark t)
        (setf (mentor-file-marked curr-file) new-mark))))
  (when (not no-redisplay)
    (mentor-details-redisplay)))


;;; Marking items

(defun mentor-item-is-marked ()
  (get-text-property (point) 'marked))

(defun mentor-mark-item (&optional clear-mark no-jump)
  "Mark the item at point unless `clear-mark' is non nil then
unmark the item instead. If `no-jump' is non nil stay at current
item instead of jumping to next."
  (interactive)
  (let* ((type (get-text-property (point) 'type))
         (inhibit-read-only t)
         (new-mark (if clear-mark nil t))
         (new-face (if new-mark mentor-marked-item-face
                 (assq (mentor-item-type) mentor-default-item-faces)))
         (mark-char (if clear-mark " " ?*))
         (start-point (point)))
    (when type
      (when (not (eq type 'dir))
        (add-text-properties (mentor-get-item-beginning)
                             (mentor-get-item-end)
                             `(face ,new-face
                               marked ,new-mark))
        ;; insert at point-at-bol + 1 to inherit all properties
        (goto-char (+ 1 (point-at-bol))) (insert-and-inherit mark-char)
        (delete-region (point-at-bol) (+ 1 (point-at-bol))))
      (goto-char start-point)
      (cond ((eq type 'torrent)
             (mentor-set-property 'marked new-mark))
            ((eq type 'file)
             (setf (mentor-file-marked (mentor-file-at-point)) new-mark))
            ((eq type 'dir) (mentor-mark-dir (mentor-file-at-point) clear-mark)))
      (when (and (not no-jump) (not (eq type 'dir)))
        (mentor-next-item t)))))

(defun mentor-unmark-item (&optional no-jump)
  "Unmark the item at point."
  (interactive)
  (mentor-mark-item t no-jump))

(defun mentor-mark-all ()
  "Mark all visible items except directories."
  (interactive)
  (mentor-do-all-items
   (when (not (eq (mentor-item-type) 'dir))
     (mentor-mark-item nil t))))

(defun mentor-unmark-all ()
  "Unmark all visible items."
  (interactive)
  (mentor-do-all-items
   (when (mentor-item-is-marked)
     (mentor-unmark-item t))))


;;; Utility functions

(defmacro mentor-do-all-items (&rest body)
  `(save-excursion
     (goto-char (point-min))
     (when (not (mentor-item-type))
       (mentor-next-item t))
     (while (mentor-item-type)
       ,@body
       (mentor-next-item t))))

(defmacro mentor-do-marked (&rest body)
  `(save-excursion
     (goto-char (point-min))
     (when (not (mentor-item-type))
       (mentor-next-item t))
     (while (mentor-item-type)
       (when (mentor-item-is-marked)
         ,@body)
       (mentor-next-item t))))

(defun mentor-limit-num (num min max)
  (if (< num min)
      min
    (if (> num max)
        max
      num)))

(defun mentor-concat-symbols (&rest symbols)
  (intern (apply 'concat (mapcar 'symbol-name symbols))))

(defun mentor-item-type ()
  (interactive)
  (get-text-property (point) 'type))

(defun mentor-prompt-complete (prompt list require-match default)
  (completing-read prompt list nil require-match nil nil
                   mentor-last-used-view))

(defun mentor-get-custom-view-name (view-id)
  (cdr (assoc view-id mentor-custom-views)))

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

(defun mentor-enforce-length (str maxlen)
  (if (not str)
      (make-string (abs maxlen) ? )
    (format (concat "%"
                    (when (< maxlen 0)
                      "-")
                    (number-to-string
                     (abs maxlen))
                    "s")
            (substring str
                       0 (min (length str)
                              (abs maxlen))))))

(defun mentor-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
        (substring str 0 (- (length str) 1))
      str)))

(provide 'mentor)

;;; mentor.el ends here
