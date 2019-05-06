;;; mentor.el --- Frontend for the rTorrent bittorrent client  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2019 Stefan Kangas.

;; Author: Stefan Kangas <stefankangas@gmail.com>
;; Version: 0.3.4
;; Keywords: comm, processes, bittorrent
;; Package-Requires: ((xml-rpc "1.6.9") (seq "1.11") (cl-lib "0.5") (async "1.9.3"))

(defconst mentor-version "0.3.4"
  "The version of Mentor that you're using.")

;; This file is NOT part of GNU Emacs.

;; Mentor is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Mentor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Mentor.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mentor is a GNU Emacs frontend for the `rTorrent' bittorrent client.

;; By default, it will start and run rTorrent from within Emacs but can also be
;; configured to use an external rTorrent instance over XML-RPC.

;; This project aims to provide a feature complete and highly customizable
;; interface, which will feel familiar to Emacs users.  Key bindings are chosen
;; to be as close to the vanilla rTorrent curses interface as possible.

;; You can find the latest development version of mentor here:
;;
;; https://www.github.com/skangas/mentor

;; Bug reports, comments, and suggestions are welcome! Send them to
;; Stefan Kangas <stefankangas@gmail.com> or report them on GitHub.

;;; Code:
(eval-when-compile
  (require 'sort))

(require 'async)
(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'term)
(require 'xml-rpc)

(require 'mentor-data)
(require 'mentor-files)
(require 'mentor-rpc)
(require 'url-scgi)


;;;; Customizable variables

(defgroup mentor nil
  "Emacs frontend for the rTorrent bittorrent client, using XML-RPC."
  :prefix "mentor-"
  :group 'external)

(defcustom mentor-custom-views
  '((1 . "main")
    (2 . "main") ; by default "name" is the same as main except for sorting
    (3 . "started")
    (4 . "stopped")
    (5 . "complete")
    (6 . "incomplete")
    (7 . "hashing")
    (8 . "seeding")
    (9 . "leeching")
    (0 . "active"))
  "A list of mappings to bind keys to views.

This takes the form of a list of \"(BINDING . NAME)\" where
BINDING is the key to which the specified view NAME will be bound
to."
  :group 'mentor
  :type '(alist :key-type integer :value-type string))

(defcustom mentor-default-view "main"
  "The default view to use when browsing torrents."
  :group 'mentor
  :type 'string)

(defcustom mentor-directory-prefix ""
  "Prefix to use before all directories.

If rTorrent is running on a remote host, you could set this to
something like `/ssh:user@example.com:'."
  :group 'mentor
  :type 'string)

(defcustom mentor-highlight-enable nil
  "If non-nil, highlight the line of the current torrent."
  :group 'mentor
  :type 'boolean)

(defcustom mentor-rtorrent-download-directory nil
  "Download directory for background rTorrent."
  :package-version '(mentor . "0.2")
  :group 'mentor
  :type 'string)

(defcustom mentor-rtorrent-keep-session nil
  "If non-nil, save session in background rTorrent."
  :package-version '(mentor . "0.2")
  :group 'mentor
  :type 'boolean)

(defcustom mentor-rtorrent-extra-conf nil
  "Extra configuration to add to background rTorrent."
  :package-version '(mentor . "0.2")
  :group 'mentor
  :type 'string)

(defcustom mentor-rtorrent-external-rpc nil
  "URL to an external rTorrent XML-RPC socket.

The default nil value indicates that Mentor should spawn a new
rTorrent instance in the background.

To connect using a local socket file, use
`~/.rtorrent-rpc.socket' or `/any/full/path'.  Note that you must
start the path with `/' or '~' for it to be recognized as a file
socket.

To connect using http, use `http://HOST[:PORT]/PATH'.  This would
be the case when using a web server in front of rTorrent.

For security reasons, we strongly suggest to use one of the
methods above.  However, it is also possibly to connect using a
scgi_port by specifying `scgi://HOST:PORT'.

Example values:

 (1) ~/.rtorrent-rpc.socket
 (2) http://localhost:8080/RPC2
 (3) scgi://localhost:5000 [not recommended]"
  :package-version '(mentor . "0.2")
  :group 'mentor
  :type 'string)

(defcustom mentor-rtorrent-use-system-daemon nil
  "Set \"system.daemon = true\" when we generate rtorrent.rc.

This will only work with rTorrent 0.9.7 or later."
  :package-version '(mentor . "0.3.5")
  :group 'mentor
  :type 'string)

(defface mentor-download-name '((t :foreground "#94BFF3"))
  "Face for mentor download name."
  :group 'mentor-faces)

(defface mentor-download-state '((t :foreground "#DCA3A3"))
  "Face for mentor download name."
  :group 'mentor-faces)

(defface mentor-download-progress '((t :foreground "#DCDCCC"))
  "Face for mentor download name."
  :group 'mentor-faces)

(defface mentor-download-speed-up '((t :foreground "#F0DFAF"))
  "Face for mentor download name."
  :group 'mentor-faces)

(defface mentor-download-speed-down '((t :foreground "#8FB28F"))
  "Face for mentor download name."
  :group 'mentor-faces)

(defface mentor-download-size '((t :foreground "#4C7073"))
  "Face for mentor download name."
  :group 'mentor-faces)

(defface mentor-tracker-name '((t :foreground "#4C7073"))
  "Face for mentor tracker name."
  :group 'mentor-faces)

(defface mentor-download-message '((t :foreground "#AC7373"))
  "Face for mentor download name."
  :group 'mentor-faces)

(defcustom mentor-view-columns
  '(((mentor-download-state-column) -2 "State" mentor-download-state)
    ((mentor-download-speed-up-column) -5 "Up" mentor-download-speed-up)
    ((mentor-download-speed-down-column) -5 "Down" mentor-download-speed-down)
    ((mentor-download-progress-column) -3 "Cmp" mentor-download-progress)
    ((mentor-download-size-column) -4 "Size" mentor-download-size)
    (name -50 "Name" mentor-download-name)
    ((mentor-download-tracker-name-column) -20 "Tracker" mentor-tracker-name)
    (message -40 "Message" mentor-download-message)
    (directory -100 "Directory"))
  "A list of all columns to show in mentor view."
  :group 'mentor
  :type '(repeat (list symbol integer string)))


;;;; Internal variables

(defvar mentor-mode-hook)
(defvar mentor-current-view)
(defvar mentor-home-dir (expand-file-name (locate-user-emacs-file "mentor/"))
  "Where Mentor should put its files.")
(defvar mentor--header-line)
(make-variable-buffer-local 'mentor--header-line)

(defvar mentor-rtorrent-client-version)
(defvar mentor-rtorrent-library-version)
(defvar mentor-rtorrent-buffer-name "*mentor-term*"
  "Name of the buffer that will run rTorrent process.")

(defvar mentor--rtorrent-session-directory nil)

(defvar mentor-rtorrent-name)

(defvar mentor-sort-list '(name))
(make-variable-buffer-local 'mentor-sort-list)

(defvar mentor-last-used-view)
(make-variable-buffer-local 'mentor-last-used-view)

(defvar mentor-last-move-target "~")
(make-variable-buffer-local 'mentor-last-move-target)

(defvar mentor-view-torrent-list nil
  "A list of torrents in given views.")

(defvar mentor-marker-char ?*)

(defface mentor-highlight-face
  '((((class color) (background light))
     :background "gray13")
    (((class color) (background dark))
     :background "dark goldenrod"))
  "Face for highlighting the current torrent."
  :group 'mentor)

(defvar mentor-default-item-faces
  '((torrent . nil) (file . nil) (dir . mentor-directory-face))
  "An alist with the default face for item types.")

;; Variables that should be changed by sub-modes

(defvar mentor-item-update-this-fun)
(make-variable-buffer-local 'mentor-item-update-this-fun)

(defvar mentor-set-priority-fun)
(make-variable-buffer-local 'mentor-set-priority-fun)

(defvar mentor--columns-var 'mentor-view-columns)
(make-variable-buffer-local 'mentor--columns-var)


;;;; Mentor major-mode

(defvar mentor-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)

    ;; navigation
    (define-key map (kbd "<up>") 'mentor-previous-item)
    (define-key map (kbd "<down>") 'mentor-next-item)
    (define-key map (kbd "p") 'mentor-previous-item)
    (define-key map (kbd "n") 'mentor-next-item)

    ;; download list actions
    (define-key map (kbd "DEL") 'mentor-download-load-torrent)
    (define-key map (kbd "l") 'mentor-download-load-magnet-link-or-url)
    (define-key map (kbd "g") 'mentor-update)
    (define-key map (kbd "G") 'mentor-reload)
    (define-key map (kbd "M-g") 'mentor-update-item)

    ;; item actions
    (define-key map (kbd "+") 'mentor-increase-priority)
    (define-key map (kbd "-") 'mentor-decrease-priority)

    ;; single download actions
    (define-key map (kbd "C") 'mentor-download-copy-data)
    (define-key map (kbd "R") 'mentor-download-move)
    (define-key map (kbd "b") 'mentor-download-set-inital-seeding)
    (define-key map (kbd "e") 'mentor-download-set-create-resized-queued-flags)
    (define-key map (kbd "o") 'mentor-download-change-target-directory)
    (define-key map (kbd "d") 'mentor-download-stop)
    (define-key map (kbd "D") 'mentor-download-remove)
    (define-key map (kbd "k") 'mentor-download-close)
    (define-key map (kbd "K") 'mentor-download-remove-including-files)
    (define-key map (kbd "r") 'mentor-download-hash-check)
    (define-key map (kbd "s") 'mentor-download-start)
    (define-key map (kbd "x") 'mentor-call-command)

    ;; misc actions
    (define-key map (kbd "RET") 'mentor-show-download-files)
    (define-key map (kbd "TAB") 'mentor-toggle-item)

    (define-key map (kbd "m") 'mentor-mark)
    (define-key map (kbd "u") 'mentor-unmark)
    (define-key map (kbd "M") 'mentor-mark-all)
    (define-key map (kbd "U") 'mentor-unmark-all)

    (define-key map (kbd "v") 'mentor-dired-jump)

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
    (define-key map (kbd "Q") 'mentor-shutdown)

    ;; view bindings
    (define-key map (kbd "a") 'mentor-add-torrent-to-view)
    (define-key map (kbd "w") 'mentor-switch-to-view)
    (define-key map (kbd "1") 'mentor-switch-to-view-1)
    (define-key map (kbd "2") 'mentor-switch-to-view-2)
    (define-key map (kbd "3") 'mentor-switch-to-view-3)
    (define-key map (kbd "4") 'mentor-switch-to-view-4)
    (define-key map (kbd "5") 'mentor-switch-to-view-5)
    (define-key map (kbd "6") 'mentor-switch-to-view-6)
    (define-key map (kbd "7") 'mentor-switch-to-view-7)
    (define-key map (kbd "8") 'mentor-switch-to-view-8)
    (define-key map (kbd "9") 'mentor-switch-to-view-9)
    (define-key map (kbd "0") 'mentor-switch-to-view-0)
    map))

(easy-menu-define mentor-mode-menu mentor-mode-map
  "Mentor menu"
  '("Mentor"
    ["Load torrent" mentor-download-load-torrent t]
    ["Load Magnet Link or URL" mentor-download-load-magnet-link-or-url t]
    "---"
    ["Update data" mentor-update t]
    ["Re-initialize data" mentor-reload t]
    ["Update item at point" mentor-update-item t]
    ("Sort"
     ["Sort by name" mentor-sort-by-name t]
     ["Sort by directory" mentor-sort-by-directory t]
     ["Sort by state" mentor-sort-by-state t]
     ["Sort by size" mentor-sort-by-size t]
     ["Sort by tied file name" mentor-sort-by-tied-file-name t]
     ["Sort by download speed" mentor-sort-by-download-speed t]
     ["Sort by upload speed" mentor-sort-by-upload-speed t]
     ["Sort by any property..." mentor-sort-by-property-prompt t])
    "---"
    ["Mark" mentor-mark t]
    ["Unmark" mentor-unmark t]
    ["Mark all" mentor-mark-all t]
    ["Unmark all" mentor-unmark-all t]
    "---"
    ["Start download" mentor-download-start t]
    ["Stop download" mentor-download-stop t]
    ["Close download" mentor-download-close t]
    ["Change directory" mentor-download-change-target-directory t]
    ["Move download" mentor-download-move t]
    ["Copy download data" mentor-download-copy-data t]
    ["Remove download" mentor-download-remove t]
    ["Remove including data " mentor-download-remove-including-files t]
    "---"
    ["Open file view" mentor-show-download-files t]
    ["Open in dired" mentor-dired-jump t]
    ["Hash check" mentor-download-hash-check t]
    ["Set resized/queued" mentor-download-set-create-resized-queued-flags t]
    ["Set initial seeding" mentor-download-set-inital-seeding t]
    ["Increase priority" mentor-increase-priority t]
    ["Decrease priority" mentor-decrease-priority t]
    "---"
    ("Switch view"
     ["View 1" mentor-switch-to-view-1 t]
     ["View 2" mentor-switch-to-view-2 t]
     ["View 3" mentor-switch-to-view-3 t]
     ["View 4" mentor-switch-to-view-4 t]
     ["View 5" mentor-switch-to-view-5 t]
     ["View 6" mentor-switch-to-view-6 t]
     ["View 7" mentor-switch-to-view-7 t]
     ["View 8" mentor-switch-to-view-8 t]
     ["View 9" mentor-switch-to-view-9 t]
     ["View 0" mentor-switch-to-view-0 t]
     ["Named view..." mentor-switch-to-view t])
    ["Add torrent to view" mentor-add-torrent-to-view t]
    "---"
    ["Run XML-RPC Command" mentor-call-command t]
    ["Customize Mentor" mentor-customize t]
    "---"
    ["Bury Mentor Buffer" bury-buffer t]
    ["Quit Mentor" mentor-shutdown]))

(eval-after-load 'dired-x
  '(define-key mentor-mode-map [remap dired-jump] 'mentor-dired-jump))

;;;###autoload
(define-derived-mode mentor-mode special-mode "mentor"
  "Major mode for controlling rTorrent from GNU Emacs

Type \\[mentor] to start Mentor.

rTorrent operations:

  `\\[mentor-download-load-torrent]' - Add torrent
  `\\[mentor-download-load-magnet-link-or-url]' - Add Magnet link, URL or torrent file path
  `\\[mentor-update]' - Reload data from rTorrent
  `\\[mentor-reload]' - Re-initialize all data from rTorrent

Operations on download at point (or marked downloads):

  `\\[mentor-download-start]' - Start download
  `\\[mentor-download-stop]' - Stop download
  `\\[mentor-download-close]' - Close download
  `\\[mentor-download-hash-check]' - Initiate hash check
  `\\[mentor-download-move]' - Move download
  `\\[mentor-download-change-target-directory]' - Change target directory
  `\\[mentor-download-remove]' - Remove download
  `\\[mentor-download-remove-including-files]' - Remove download including data
  `\\[mentor-download-copy-data]' - Copy downloaded data to location
  `\\[mentor-increase-priority]' - Increase priority of download
  `\\[mentor-decrease-priority]' - Decrease priority of download
  `\\[mentor-download-set-create-resized-queued-flags]' - Set the 'create/resize queued' flags on all files in a torrent.
      This is necessary if the underlying files in a torrent have been deleted
      or truncated, and thus rtorrent must recreate them.

Operations on download at point:

  `\\[mentor-show-download-files]' - Enter files view
  `\\[mentor-update-item]' - Reload data from rTorrent
  `\\[mentor-dired-jump]' - Show download in Dired

Marking commands:

  `\\[mentor-mark]' - Mark download
  `\\[mentor-unmark]' - Unmark download
  `\\[mentor-mark-all]' - Mark all downloads
  `\\[mentor-unmark-all]' - Unmark all downloads

Sorting downloads:

  `\\[mentor-sort-by-directory]' - Sort downloads by directory
  `\\[mentor-sort-by-name]' - Sort by name
  `\\[mentor-sort-by-tied-file-name]' - Sort by tied file name
  `\\[mentor-sort-by-size]' - Sort by size
  `\\[mentor-sort-by-state]' - Sort by state
  `\\[mentor-sort-by-download-speed]' - Sort by download speed
  `\\[mentor-sort-by-upload-speed]' - Sort by upload speed
  `\\[mentor-sort-by-property-prompt]' - Sort by any property (shows prompt)

Misc commands:

  `0' to `9' - Change currently active view
  `\\[mentor-switch-to-view]' - Switch to view (prompt)
  `\\[mentor-add-torrent-to-view]' - Add download to view
  `\\[mentor-call-command]' - Send XML-RPC command to rTorrent
  `\\[mentor-shutdown] - Shutdown Mentor
  `\\[bury-buffer] - Bury Mentor buffer

\\{mentor-mode-map}"
  :group 'mentor
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil)
  (setq mentor-current-view mentor-default-view
        mentor-items (make-hash-table :test 'equal))
  (add-hook 'post-command-hook #'mentor-post-command-hook t t)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  (run-mode-hooks 'mentor-mode-hook))

(defun mentor-rtorrent-create-conf (filename rpc)
  "Generate rTorrent configuration and write to FILENAME.

It will use the RPC argument as value for scgi_local."
  (let ((output
         (concat
          (format "## Autogenerated by mentor.el at %s\n"
                  (format-time-string "%Y-%m-%dT%H:%M:%S"))
          (format "scgi_local = %s\n" rpc)
          (when mentor-rtorrent-use-system-daemon
            "system.daemon = true\n")
          (if mentor-rtorrent-download-directory
              (format "directory = %s\n" mentor-rtorrent-download-directory) "")
          (if mentor-rtorrent-keep-session
              (format "session = %s\n" mentor--rtorrent-session-directory) "")
          "encoding.add = utf8\n"
          (if mentor-rtorrent-extra-conf
              (concat "\n## User additions from `mentor-rtorrent-extra-conf':\n"
                      mentor-rtorrent-extra-conf) "") "\n")))
    (with-temp-file filename
      (insert output))))

(defun mentor-rtorrent-already-running (buf)
  (if (and buf (get-buffer buf))
      (condition-case _error
          ;; Already running
          (progn
            (mentor-rpc-command "system.pid")
            t)
        (error
         ;; Running with problems -- needs restart
         nil))
    nil))

(defun mentor-rtorrent-run-in-background ()
  "Start rTorrent in a new buffer."
  (make-directory mentor-home-dir t)
  (when mentor-rtorrent-keep-session
    (make-directory
     (setq mentor--rtorrent-session-directory
           (expand-file-name "session" mentor-home-dir)) t))
  (let ((bufname mentor-rtorrent-buffer-name)
        (buf nil)
        (rpc (expand-file-name "rtorrent.rpc" mentor-home-dir))
        (conf (expand-file-name "rtorrent.rc" mentor-home-dir)))
    (setq mentor-rpc--rtorrent-url (concat "scgi://" rpc))
    (when (not (mentor-rtorrent-already-running bufname))
      (when (bufferp bufname)
        (kill-buffer bufname))
      (mentor-rtorrent-create-conf conf rpc)
      (let ((rtorrent (executable-find "rtorrent")))
        (if (not rtorrent)
            (error "Unable to find rtorrent executable"))
        (get-buffer-create bufname)
        (setq buf (term-ansi-make-term
                   bufname "rtorrent" nil "-n" "-o" (concat "import=" conf))))
      (with-current-buffer buf
        (term-mode)
        (term-char-mode)
        (let (term-escape-char)
          (term-set-escape-char ?\C-x))
        (bury-buffer)))))

(defun mentor-normalize-rpc-url (url)
  (if (string-match "^[~/]" url)
      (concat "scgi://" url)
    url))

(defun mentor-setup-rtorrent ()
  (if mentor-rtorrent-external-rpc
      (setq mentor-rpc--rtorrent-url (mentor-normalize-rpc-url mentor-rtorrent-external-rpc))
    (let ((rtorrent-started nil)
          (since (float-time)))
      (mentor-rtorrent-run-in-background)
      (message "Waiting for rtorrent to start...")
      (while (not rtorrent-started)
        (condition-case err
            (progn (mentor-rpc-command "system.pid")
                   (setq rtorrent-started t))
          (error
           (if (string-match "make client process failed: connection refused"
                             (error-message-string err))
               (if (< (float-time) (+ since 10))
                   (sleep-for 0.1)
                 (error "XML-RPC not up after 10 seconds: %s" err))
             (signal (car err) (cdr err)))))))))

;;;###autoload
(defun mentor ()
  "Control rTorrent from Emacs using XML-RPC.

If mentor is already running, switch to its buffer.  Otherwise,
start a new session.

Full documentation is available under `mentor-mode'."
  (interactive)
  (if (get-buffer "*mentor*")
      ;; Assume that it's set up correctly if it exists
      (switch-to-buffer (get-buffer-create "*mentor*"))
    ;; Otherwise create and set it up
    (switch-to-buffer (get-buffer-create "*mentor*"))
    (mentor-mode)
    (mentor-setup-rtorrent)
    (setq mentor-item-update-this-fun 'mentor-download-update-and-reinsert-at-point)
    (setq mentor-set-priority-fun 'mentor-download-set-priority-fun)
    (setq mentor--columns-var  'mentor-view-columns)
    (setq mentor-sort-list '((up.rate . t) name))
    (mentor-init-header-line)
    (setq mentor-rtorrent-client-version (mentor-rpc-command "system.client_version")
          mentor-rtorrent-library-version (mentor-rpc-command "system.library_version")
          mentor-rtorrent-name (mentor-rpc-command "session.name"))
    (let* ((pwd-with-trailing-newline (mentor-rpc-command "execute.capture" "" "pwd"))
           (pwd (substring pwd-with-trailing-newline 0 -1)))
      (cd (file-name-as-directory pwd)))
    (mentor-set-view mentor-default-view)
    (when (equal mentor-current-view mentor-last-used-view)
      (setq mentor-last-used-view (mentor-get-custom-view-name 2)))
    (mentor-download-data-init)
    (mentor-views-init)
    (mentor-redisplay)
    (goto-char (point-min))))

;;;###autoload
(defun mentor-customize ()
  "Call the customize function with mentor as argument."
  (interactive)
  (customize-browse 'mentor))

(defun mentor-post-command-hook ()
  (when mentor-highlight-enable
    (mentor-highlight-torrent)))

(defun mentor-init-header-line ()
  (setq header-line-format
        '(:eval (concat
                 (propertize " " 'display '((space :align-to 1)))
                 (substring mentor--header-line
                            (min (length mentor--header-line)
                                 (window-hscroll)))))))


;;;; Mentor items

(defun mentor-between-items ()
  (not (mentor-item-id-at-point)))

(defun mentor-marker-regexp ()
  (concat "^" (regexp-quote (char-to-string mentor-marker-char))))

(defun mentor-repeat-over-lines (arg function)
  "Repeat FUNCTION over ARG lines.

This version skips non-file lines."
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (while (and (not (eobp)) (mentor-between-items)) (forward-line 1))
      (save-excursion
        (forward-line 1)
        (move-marker pos (1+ (point))))
      (save-excursion (funcall function))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0) (not (bobp)))
      (setq arg (1+ arg))
      (forward-line -1)
      (while (and (not (bobp)) (mentor-between-items)) (forward-line -1))
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    ;; (dired-move-to-filename)
    ))

(defmacro mentor-map-over-marks (body arg &optional _show-progress)
  "Eval BODY with point on each marked line.  Return a list of BODY's results.
If no marked item could be found, execute BODY on the current line.
ARG, if non-nil, specifies the items to use instead of the marked items.
  If ARG is an integer, use the next ARG (or previous -ARG, if
   ARG<0) items.  In that case, point is dragged along.  This is
   so that commands on the next ARG (instead of the marked) items
   can be chained easily.
  For any other non-nil value of ARG, use the current item.
If optional third arg SHOW-PROGRESS evaluates to non-nil,
  redisplay item after it has been processed.
No guarantee is made about the position on the marked line.
  BODY must ensure this itself if it depends on this.
Search starts at the beginning of the buffer, thus the car of the list
  corresponds to the line nearest to the buffer's bottom.  This
  is also true for (positive and negative) integer values of ARG.
BODY should not be too long as it is expanded four times."
  ;;
  ;;Warning: BODY must not add new lines before point - this may cause an
  ;;endless loop.
  ;;This warning should not apply any longer, sk  2-Sep-1991 14:10.
  `(prog1
       (let ((inhibit-read-only t) case-fold-search found results)
         (if ,arg
             (if (integerp ,arg)
                 (progn ;; no save-excursion, want to move point.
                   (mentor-repeat-over-lines
                    ,arg
                    (function (lambda ()
                                ;; (if ,show-progress (sit-for 0))
                                (setq results (cons ,body results)))))
                   (if (< ,arg 0)
                       (nreverse results)
                     results))
               ;; non-nil, non-integer ARG means use current file:
               (list ,body))
           (let ((regexp (mentor-marker-regexp)) next-position)
             (save-excursion
               (goto-char (point-min))
               ;; remember position of next marked file before BODY
               ;; can insert lines before the just found file,
               ;; confusing us by finding the same marked file again
               ;; and again and...
               (setq next-position (and (re-search-forward regexp nil t)
                                        (point-marker))
                     found (not (null next-position)))
               (while next-position
                 (goto-char next-position)
                 ;; (if ,show-progress (sit-for 0))
                 (setq results (cons ,body results))
                 ;; move after last match
                 (goto-char next-position)
                 (forward-line 1)
                 (set-marker next-position nil)
                 (setq next-position (and (re-search-forward regexp nil t)
                                          (point-marker)))))
             (if found
                 results
               (list ,body)))))))
     ;; ;; save-excursion loses, again
     ;; (dired-move-to-filename)))

(defun mentor-get-marked-items (&optional arg)
  "Return the marked items' names as list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Optional argument ARG, if non-nil, specifies items near
 point instead of marked items.  It usually comes from the prefix
 argument.
  If ARG is an integer, use the next ARG items.
  Any other non-nil value means to use the current file instead."
  (save-excursion
    (nreverse
     (mentor-map-over-marks
      (mentor-item-property 'name (mentor-get-item-at-point))
      arg))))

;; Based on `dired-mark-pop-up'
(defun mentor-mark-pop-up (bufname items function &rest args)
  "Return FUNCTION's result on ARGS after showing which items are marked.
Displays the file names in a window showing a buffer named
BUFNAME; the default name being \" *Marked Items*\".  The window
is not shown if there is just one item.

ITEMS is the list of marked items.

FUNCTION should not manipulate items, just read input
\(an argument or confirmation)."
  (if (= (length items) 1)
      (apply function args)
    (let ((buffer (get-buffer-create (or bufname " *Marked Items*"))))
      (with-current-buffer buffer
        (with-current-buffer-window
         buffer
         (cons 'display-buffer-below-selected
               '((window-height . fit-window-to-buffer)))
         #'(lambda (window _value)
             (with-selected-window window
               (unwind-protect
                   (apply function args)
                 (when (window-live-p window)
                   (quit-restore-window window 'kill)))))
         (erase-buffer)
         (completion--insert-strings items))))))

(defun mentor-mark-prompt (arg items)
  "Return a string suitable for use in a mentor prompt.
ARG is normally the prefix argument for the calling command.
ITEMS should be a list of item names."
  (let ((count (length items)))
    (if (= count 1)
        (car items)
      ;; more than 1 item:
      (if (integerp arg)
          (format "[%s %d items]" (if (> arg 0) "next" "previous") count)
        (format "[%d items]" count)))))

(defun mentor-mark-confirm (desc arg)
  (let ((items (mentor-get-marked-items arg)))
    (mentor-mark-pop-up nil items (function y-or-n-p)
                  (concat desc " "
                          (mentor-mark-prompt arg items) "? "))))


;;;; Getting torrent data

(defun mentor-download-data-init ()
  "Initialize torrent data from rTorrent.

All torrent information will be re-fetched, making this an
expensive operation."
  (message "Initializing torrent data...")
  (mentor-rpc-d.multicall mentor-rpc-d-methods mentor-rpc-t-methods t)
  (mentor-views-update-views)
  (message "Initializing torrent data... DONE"))

(defun mentor-download-data-update-all ()
  (message "Updating torrent data...")
  (condition-case _err
      (progn
        (mentor-rpc-d.multicall mentor-rpc-volatile-d-methods mentor-rpc-t-methods)
        (message "Updating torrent data...DONE"))
    (mentor-need-init
     (mentor-download-data-init))))

(defun mentor-download-update-this (download)
  "Update specified DOWNLOAD."
  (let* ((hash (mentor-item-property 'hash download))
         (methods (append mentor-rpc-volatile-d-methods))
         (values (cl-mapcar
                  (lambda (method)
                    (mentor-rpc-command method hash))
                  methods)))
    (let ((d-properties (mentor-rpc-methods-to-properties methods)))
      (mentor-data-download-update-from d-properties nil values))))

(defun mentor-download-update-and-reinsert-at-point ()
  "Update download at point and reinsert in buffer."
  (let ((download (mentor-get-item-at-point)))
    (mentor-download-update-this download)
    (mentor-download-reinsert-at-point)))


;;;; Main torrent view

(defmacro mentor-keep-position (&rest body)
  "Run BODY form but keep the current position."
  `(let ((kept-torrent-id (mentor-item-id-at-point))
         (kept-torrent-pos-in-line (- (point) (line-beginning-position)))
         (kept-point (point)))
     ,@body
     (if kept-torrent-id
         (condition-case _err
             (progn (mentor-goto-download kept-torrent-id)
                    (forward-char kept-torrent-pos-in-line))
           (mentor-missing-torrent
            (goto-char kept-point)))
       (goto-char kept-point))))

(defun mentor-insert-torrent (id)
  "Insert download ID at point."
  (let* ((item (mentor-get-item id))
         (text (mentor-process-view-columns item mentor-view-columns))
         (marked (mentor-item-marked item)))
    (insert (propertize text
                        'marked marked
                        'field id
                        'collapsed t
                        'type 'torrent) "\n")
    (when marked
      (save-excursion
        (mentor-previous-item)
        (mentor-mark)))))

(defun mentor-insert-torrents ()
  "Insert downloads in current view at point."
  (let ((tor-ids (cdr (assoc (intern mentor-current-view)
                             mentor-view-torrent-list))))
    (dolist (id tor-ids)
      (mentor-insert-torrent id))))

(defun mentor-download-reinsert-at-point ()
  "Reinsert download at point."
  (let ((inhibit-read-only t)
        (id (mentor-item-id-at-point)))
    (mentor-delete-item-from-buffer (point))
    (mentor-insert-torrent id)
    (mentor-previous-item))
  (mentor-goto-item-name-column))

(defun mentor-process-columns-helper (cols lenfun strfun)
  (replace-regexp-in-string
   " *$" "" ; Remove trailing whitespace
   (apply 'concat
    (cl-mapcar (lambda (column)
              (let* ((len (funcall lenfun column))
                     (str (funcall strfun column)))
                (concat (mentor-enforce-length str len) " ")))
            cols))))

(defun mentor-process-view-header-columns (cols)
  (mentor-process-columns-helper
   cols
   (lambda (col) ;lenfun
     (cadr col))
   (lambda (col) ; strfun
     (cl-caddr col))))

(defun mentor-process-view-columns (item cols)
  (concat
   " "
   (mentor-process-columns-helper
    cols
    (lambda (col) ; lenfun
      (cadr col))
    (lambda (col) ; strfun
      (let* ((col-name (car col))
             (text
              (cond ((not col-name) "")
                    ((listp col-name)
                     (apply (car col-name) item (cdr col-name)))
                    (t
                     (let ((text (mentor-item-property col-name item)))
                       (if text text "")))))
             (col-face (cl-cadddr col)))
        (if col-face
            (propertize text
                        'face col-face)
          text))))))

(defun mentor--find-name-column (cols)
  (1+ (apply '+ (cl-mapcar
                 (lambda (col) (1+ (abs (cadr col))))
                 (seq-take-while
                  (lambda (fmt)
                    (not (eq (car fmt) 'name)))
                  cols)))))

(defun mentor-reload-header-line ()
  (setq mentor--header-line
        (mentor-process-view-header-columns (eval mentor--columns-var))))

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


;;;; Sorting

(defun mentor-do-sort ()
  (mentor-keep-position
   (goto-char (point-min))
   (save-excursion
     (let ((sort-fold-case t)
           (inhibit-read-only t))
       (sort-subr nil
                  (lambda () (ignore-errors (mentor-forward-item 1)))
                  (lambda () (ignore-errors (mentor-end-of-item)))
                  (lambda ()
                    (let ((item (mentor-get-item-at-point)))
                      (cl-mapcar (lambda (p)
                                 (let ((prop (or (and (listp p) (car p)) p)))
                                  (mentor-item-property prop item)))
                               mentor-sort-list)))
                  nil
                  (lambda (a b)
                    (mentor-cmp-properties a b mentor-sort-list)))))))

(defun mentor-cmp-properties (x y &optional props)
  (let* ((a (car x))
         (b (car y))
         (reverse (cdr-safe (car props)))
         (cmp (if (stringp a)
                  (if reverse (not (string< a b)) (string< a b))
                (if reverse (> a b) (< a b)))))
    (when (and (not cmp) (equal a b) (> (length props) 1))
      (setq cmp (mentor-cmp-properties (cdr x) (cdr y) (cdr props))))
    cmp))

(defun mentor-sort (&optional property reverse append)
  "Sort the mentor torrent buffer.
Defaults to sorting according to `mentor-sort-list'.

PROPERTY gives according to which property the torrents should be
sorted.

If REVERSE is non-nil, the result of the sort is reversed.

When APPEND is non-nil, instead of sorting directly, add the
result to the end of `mentor-sort-list'.  This makes it possible
to sort according to several properties."
  (when property
    (let ((elem (cons property reverse)))
      (if append
          (add-to-list 'mentor-sort-list elem t)
        (setq mentor-sort-list (list elem)))))
  (mentor-do-sort))

(defun mentor-sort-by-directory (append)
  (interactive "P")
  (mentor-sort 'directory nil append))

(defun mentor-sort-by-download-speed (append)
  (interactive "P")
  (mentor-sort 'down.rate t append))

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
  (mentor-sort 'up.rate t append))


;;; Navigation

(defmacro mentor-while-same-item (condition skip-blanks &rest body)
  `(let* ((item (mentor-item-id-at-point)))
     (while (and ,condition
                 (or (and ,skip-blanks
                          (not (mentor-item-id-at-point)))
                     (equal item (mentor-item-id-at-point))))
       ,@body)))

(defun mentor-beginning-of-item ()
  "Goto the beginning of the item at point.

If the item at point has an item-start property defined and
REAL-START is nil goto that point.  Otherwise goto the real start
point."
  (interactive)
  (let ((start (or (get-text-property (point) 'item-start)
                   (field-beginning nil nil (point-at-bol)))))
    (when start
      (goto-char start))))

(defun mentor-end-of-item ()
  "Goto the end of the item at point."
  (interactive)
  (ignore-errors (mentor-forward-item 1))
  (mentor-while-same-item (not (bobp)) nil (backward-char)))

(defun mentor-get-item-beginning ()
  (save-excursion
    (mentor-beginning-of-item)
    (point)))

(defun mentor-get-item-end ()
  (save-excursion
    (mentor-end-of-item)
    (point)))

(defun mentor-goto-item-name-column ()
  "Go to the column where the item name starts."
  (beginning-of-line)
  (goto-char (+ (point) (mentor--find-name-column
                         (eval mentor--columns-var)))))

(defun mentor-forward-item (&optional arg)
  "Move point forward ARG items."
  (let* ((arg (or arg 1))
         (reverse (< arg 0))
         (i (abs arg))
         (done nil))
    (while (and (> i 0))
      (cl-decf i)
      (setq done nil)
      (mentor-while-same-item (not done) t
       (let ((at-buf-limit (if reverse (bobp) (eobp)))
             (step (if reverse -1 1)))
         (if (not at-buf-limit)
             (forward-line step)
           (setq done t)))))))

(defun mentor-next-item (&optional arg)
  "Move cursor down ARG items."
  (interactive "P")
  (mentor-forward-item arg)
  (mentor-goto-item-name-column))

(defun mentor-previous-item (&optional arg)
  "Move cursor vertically up ARG items."
  (interactive "P")
  (mentor-next-item (- 0 (or arg 1))))

(put 'mentor-missing-torrent
     'error-conditions
     '(error mentor-error mentor-missing-torrent))

(defun mentor-delete-item-from-buffer (item)
  (let ((inhibit-read-only t))
    (goto-char item)
    (delete-region (mentor-get-item-beginning)
                   (+ 1 (mentor-get-item-end)))))

(defun mentor-goto-download (id)
  "Move mark to download with ID."
  (let ((pos (save-excursion
               (goto-char (point-min))
               (while (and (not (equal id (mentor-item-id-at-point)))
                           (not (= (point) (point-max))))
                 (mentor-forward-item 1))
               (point))))
    (if (not (= pos (point-max)))
        (goto-char pos)
      (signal 'mentor-missing-torrent `("No such torrent" ,id)))))

(defun mentor-toggle-item ()
  (interactive)
  (let ((type (mentor-get-item-type)))
    (cond ((eq type 'dir)
           (mentor-toggle-file (get-text-property (point) 'file))))))


;;;; Interactive item commands

(defun mentor-item-update-this ()
  (when mentor-item-update-this-fun
    (funcall mentor-item-update-this-fun)))

(defun mentor-set-priority (val)
  (setq val (or val 1))
  (funcall mentor-set-priority-fun val))

(defun mentor-decrease-priority ()
  (interactive)
  (mentor-set-priority -1)
  (mentor-item-update-this))

(defun mentor-increase-priority ()
  (interactive)
  (mentor-set-priority 1)
  (mentor-item-update-this))

(defun mentor-update-item (&optional arg)
  (interactive "P")
  (mentor-map-over-marks (mentor-item-update-this)
   arg))

(defun mentor-mark (&optional arg)
  "Mark the current (or next ARG) items.

Use \\[mentor-unmark-all-files] to remove all marks
and \\[mentor-unmark] on a subdir to remove the marks in
this subdir."
  (interactive "P")
  (let ((inhibit-read-only t))
    (mentor-repeat-over-lines
     (prefix-numeric-value arg)
     (function (lambda ()
                 ;; insert at point-at-bol + 1 to inherit all properties
                 (goto-char (1+ (point-at-bol)))
                 (insert-and-inherit mentor-marker-char)
                 (delete-region (point-at-bol) (+ 1 (point-at-bol)))))))
  (mentor-goto-item-name-column))

(defun mentor-unmark (&optional arg)
  "Unmark the current (or next ARG) items."
  (interactive "P")
  (let ((mentor-marker-char ?\040))
    (mentor-mark arg)))

(defun mentor-mark-all ()
  "Mark all visible items except directories."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (mentor-mark))))

(defun mentor-unmark-all ()
  "Unmark all visible items."
  (interactive)
  (mentor-map-over-marks
   (mentor-unmark)
   nil))


;;;; Interactive commands on downloads

(defun mentor-get-old-torrent-path (tor)
  (let ((path (or (mentor-item-property 'base_path tor)
                  (and (= (mentor-item-property 'bytes_done tor) 0)
                       (mentor-item-property 'directory tor))))
        (is-multi-file (= 1 (mentor-item-property 'is_multi_file tor))))
    (when (not path)
      (error "Unable to get path for torrent"))
    (if (or is-multi-file (not (file-directory-p path)))
        (substring (directory-file-name path)
                   0 (- (length (file-name-nondirectory path))))
      path)))

(defun mentor-download-load-torrent (prefix)
  "Add a torrent file to rTorrent and start download.

If PREFIX is set, the download will not be started.

This does not support Magnet links.  Use
`mentor-download-load-magnet-link-or-url' instead."
  (interactive "P")
  (let* ((is-torrent-p (lambda (x)
                         (or (and (not (string-match "^\\." x))
                                  (file-directory-p x))
                             (string-match "\.torrent$" x))))
         (file (read-file-name "Add torrent: " nil nil
                               nil nil is-torrent-p)))
    (mentor-file-sanity-check file)
    (mentor-rpc-c-load-raw file prefix))
  (mentor-update))

(defun mentor-download-load-magnet-link-or-url (prefix file)
  "Add Magnet link or URL to rTorrent and start download.

If PREFIX is set, the download will not be started.

This can also take a file path, but it has no completion.  Unlike
``mentor-download-load-torrent'' this would work with a file path
when rTorrent is running on a remote host."
  (interactive "P\nMLoad Magnet link or URL: ")
  (mentor-rpc-c-load file prefix))

(defun mentor-call-command (&optional cmd)
  "Send XML-RPC command CMD to rTorrent."
  (interactive "MEnter command: ")
  (apply 'mentor-rpc-command (split-string cmd)))

(defun mentor-download-get-file-list (tor)
  (let ((files (mentor-item-property 'files tor)))
    (when (not (cdr-safe files))
      (progn
        (setq files (mentor-rpc-f.multicall (mentor-item-property 'hash tor) "f.path_components="))
        (mentor-item-set-property 'files files tor)))
    files))

(defun mentor-delete-file (file)
  (if (file-exists-p file)
      (let ((dired-recursive-deletes nil))
        (condition-case _err
            (dired-delete-file file)
          (file-error (message "Unable to delete: %s" file))))
    (message "Mentor: Downloaded file missing on delete: %s" file)))

;; TODO: Check directory of multi file download for lingering files
(defun mentor--download-remove-helper (remove-files &optional arg)
  (when (mentor-mark-confirm
         (or (and remove-files "Remove including data") "Remove")
         arg)
    (mentor-map-over-marks
     (progn
       (let* ((download (mentor-get-item-at-point))
              (files (and remove-files (mentor-download-get-file-list download)))
              (base-path (mentor-item-property 'base_path))
              (is-multi-file (mentor-item-property 'is_multi_file download))
              dirs)
         (mentor-rpc-d-erase (mentor-item-property 'hash download))
         (mentor-view-torrent-list-delete-all download)
         (remhash (mentor-item-property 'local_id download) mentor-items)
         (when remove-files
           (if (= is-multi-file 0)
               (mentor-delete-file base-path)
             (progn
               (dolist (file files)
                 (let* ((file (concat base-path "/" (caar file)))
                        (dir (file-name-directory file)))
                   (mentor-delete-file file)
                   (setq dirs (cl-adjoin dir dirs :test 'equal))))
               (setq dirs (sort dirs (lambda (a b) (not (string< a b)))))
               (dolist (dir dirs)
                 (mentor-delete-file dir))))
           (when (file-exists-p base-path)
             (error "Mentor: Still exists after delete: %s" base-path)))
         (mentor-delete-item-from-buffer (point))
         (mentor-goto-item-name-column)))
     arg)))

(defun mentor-download-remove (&optional arg)
  "Remove download at point or marked downloads."
  (interactive "P")
  (mentor--download-remove-helper nil arg))

(defun mentor-download-remove-including-files (&optional arg)
  "Remove download at point or marked downloads, including data."
  (interactive "P")
  (mentor--download-remove-helper t arg))

(defun mentor-get-new-path (&optional prompt old)
  (let* ((old (concat mentor-directory-prefix
                      (or old mentor-last-move-target)))
         (prompt (or prompt "New path: "))
         (new (read-file-name prompt old nil t)))
    (if (condition-case _err
            (mentor-rpc-command "execute2" "" "ls" "-d" new)
          (error nil))
        (setq mentor-last-move-target new)
      (error (concat "No such file or directory: " new)))))

;; TODO: Sort out the "copied" and "moved" messages below to give a summary
;; instead of just the last file.

(defun mentor-download-copy-data (&optional arg)
  "Copy download data to another location."
  (interactive "P")
  (let* ((items (mentor-get-marked-items))
         (prompt (concat "Copy " (mentor-mark-prompt arg items) " to: "))
         (new (mentor-mark-pop-up nil items 'mentor-get-new-path prompt)))
   (mentor-map-over-marks
    (let* ((old (mentor-item-property 'base_path)))
      (when (and (not (null old))
                 (file-exists-p old))
        (mentor-rpc-c-execute2 "cp" "-Rn" old new))
      (message "Copied %s to %s" (mentor-item-property 'name) new)
      (mentor-download-reinsert-at-point))
    arg)))

(defun mentor-download-move-async (downloads)
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`\\(mentor\\)-")
      (setq load-path (quote ,load-path))
      (require 'mentor)
      (let ((was-started ,(cdr (assoc 'was-started (car downloads))))
            (hash ,(cdr (assoc 'hash (car downloads))))
            (old ,(cdr (assoc 'old (car downloads))))
            (new ,(cdr (assoc 'new (car downloads))))
            (no-move ,(if (cdr (assoc 'no-move (car downloads))) t nil)))
        (when was-started
          (mentor-rpc-d-stop hash))
        (mentor-rpc-d-close hash)
        (if (not no-move)
            (mentor-rpc-c-execute2 "mv" "-u" old new))
        (mentor-rpc-d-directory-set hash new)
        (quote ,downloads)))
   (lambda (remaining)
     (let ((local_id (cdr (assoc 'local_id (car remaining))))
           (name (cdr (assoc 'name (car remaining))))
           (new (cdr (assoc 'new (car remaining))))
           (hash (cdr (assoc 'hash (car remaining))))
           (was-started (cdr (assoc 'was-started (car remaining))))
           (verbstr (if (cdr (assoc 'no-move (car remaining))) "Changed directory of" "Moved")))
       (with-current-buffer "*mentor*"
         (mentor-keep-position
           (mentor-goto-download local_id)
           (when was-started
             (mentor-rpc-d-start hash))
           (mentor-download-update-and-reinsert-at-point)))
       (message "mentor: %s '%s' to '%s'" verbstr name new))
     (when (cdr remaining)
       (mentor-download-move-async (cdr remaining))))))

(defun mentor-download-move (&optional no-move arg)
  (interactive "P")
  (let* ((items (mentor-get-marked-items))
         (verbstr (or (and no-move "Change directory of ") "Move "))
         (prompt (concat verbstr (mentor-mark-prompt arg items) " to: "))
         (new (mentor-mark-pop-up nil items 'mentor-get-new-path prompt))
         (downloads (mentor-map-over-marks
                     (let* ((old (mentor-item-property (if no-move 'directory 'base_path))))
                       (when (not no-move)
                         (when (null old)
                           (error "Download has no base path: %s" (mentor-item-property 'name)))
                         ;; FIXME: Should work also on remote host, i.e. use rpc "execute2"
                         ;; to look for the file.
                         (when (not (file-exists-p old))
                           (error (concat "Download base path \"%s\" does not exist: \""
                                          (mentor-item-property 'name) "\"\n"
                                          "Try `mentor-download-change-target-directory'")
                                  old))
                         (when (and (not (= (mentor-item-property 'is_multi_file) 1)) (file-directory-p old))
                           (error "Moving single torrent, base_path is a directory.  This is probably a bug"))
                         (let ((target (concat new (file-name-nondirectory old))))
                           (when (file-exists-p target)
                             (error "Destination already exists: %s" target))))
                       (if (equal (file-name-directory old) new)
                           (progn (message "Skipping %s since it is already in %s"
                                           (mentor-item-property 'name) new)
                                  nil)
                         `((name . ,(mentor-item-property 'name))
                           (was-started . ,(= (mentor-item-property 'is_active) 1))
                           (hash . ,(mentor-item-property 'hash))
                           (old . ,old)
                           (new . ,new)
                           (local_id . ,(mentor-item-property 'local_id))
                           (no-move . ,no-move))))
                     arg)))
    (mentor-download-move-async downloads)))

(defun mentor-download-change-target-directory (&optional arg)
  "Change target directory of download without moving data."
  (interactive "P")
  (mentor-download-move 'nomove arg))

(defun mentor-download-hash-check (&optional arg)
  "Initiate hash check on download."
  (interactive "P")
  (mentor-map-over-marks
   (progn
     (let ((tor (mentor-get-item-at-point)))
       (mentor-rpc-command "d.check_hash" (mentor-item-property 'hash tor))
       (mentor-item-set-property 'hashing 1 tor)
       (mentor-item-set-property 'is_open 1 tor)
       (mentor-download-update-and-reinsert-at-point)))
   arg))

(defun mentor-download-pause (&optional arg)
  "Pause download.

This runs the `d.pause' XML-RPC command.

This is probably not what you want, use `mentor-download-stop'
instead."
  (interactive "P")
  (mentor-map-over-marks
   (progn (mentor-rpc-command "d.pause" (mentor-item-property 'hash))
          (mentor-download-update-and-reinsert-at-point))
   arg))

(defun mentor-download-resume (&optional arg)
  "Resume download.

This runs the `d.resume' XML-RPC command.

This is probably not what you want, use `mentor-download-start'
instead."
  (interactive "P")
  (mentor-map-over-marks
   (progn (mentor-rpc-command "d.resume" (mentor-item-property 'hash))
          (mentor-download-update-and-reinsert-at-point))
   arg))

(defun mentor-download-start (&optional arg)
  "Start download.

This runs the `d.start' XML-RPC command."
  (interactive "P")
  (mentor-map-over-marks
   (progn (mentor-rpc-d-start (mentor-item-property 'hash))
          (mentor-download-update-and-reinsert-at-point))
   arg))

(defun mentor-download-stop (&optional arg)
  "Stop download.

This runs the `d.stop' XML-RPC command."
  (interactive "P")
  (mentor-map-over-marks
   (progn (mentor-rpc-d-stop (mentor-item-property 'hash))
          (mentor-download-update-and-reinsert-at-point))
   arg))

(defun mentor-download-open (&optional arg)
  "Set download status to `open'.

This runs the `d.open' XML-RPC command."
  (interactive "P")
  (mentor-map-over-marks
   (progn (mentor-rpc-command "d.open" (mentor-item-property 'hash))
          (mentor-download-update-and-reinsert-at-point))
   arg))

(defun mentor-download-close (&optional arg)
  "Set download status to `closed'.

This runs the `d.close' XML-RPC command, which corresponds to the
^K command in the ncurses ui."
  (interactive "P")
  (mentor-map-over-marks
   (progn (mentor-rpc-d-close (mentor-item-property 'hash))
          (mentor-download-update-and-reinsert-at-point))
   arg))

(defun mentor-download-set-create-resized-queued-flags (arg)
  "Set the 'create/resize queued' flags on all files in a torrent.

Corresponds to ^E in the ncurses ui."
  (interactive "P")
  (mentor-map-over-marks
   (progn
     (mentor-rpc-f.multicall (mentor-item-property 'hash) "f.set_create_queued=0" "f.set_resize_queued=0")
     (message "mentor: Queued create/resize of files in torrent: %s" (mentor-item-property 'name)))
   arg))

(defun mentor-download-set-inital-seeding ()
  "Set download to perform initial seeding.
Only use when you are the first and only seeder so far for the download."
  (interactive)
  (message "TODO: mentor-download-set-inital-seeding"))

(defun mentor-dired-jump ()
  "Visit files for download at point using Dired."
  (interactive)
  (let* ((is-multi-file (= (mentor-item-property 'is_multi_file) 1))
         (directory (mentor-item-property 'directory))
         (name (mentor-item-property 'name))
         (target (if is-multi-file directory (expand-file-name name directory))))
    (when (not directory)
      (error "Download does not have a 'directory' set"))
    (if (file-exists-p target)
        (progn (find-file directory)
               (when (not is-multi-file)
                 (dired-goto-file (expand-file-name name directory))))
      (error "No such file or directory: %s" target))))

(defun mentor-update ()
  "Update all torrents and redisplay."
  (interactive)
  (mentor-keep-position
   (when (mentor-views-is-custom-view mentor-current-view)
     (mentor-views-update-filter mentor-current-view))
   (mentor-download-data-update-all)
   (mentor-redisplay)))

(defun mentor-reload ()
  "Re-initialize all torrents and redisplay."
  (interactive)
  (mentor-keep-position
   (when (mentor-views-is-custom-view mentor-current-view)
     (mentor-views-update-filter mentor-current-view))
   (setq mentor-items (make-hash-table :test 'equal))
   (mentor-download-data-init)
   (mentor-redisplay)))

(defun mentor-redisplay ()
  "Redisplay the mentor torrent view buffer."
  (interactive)
  (mentor-reload-header-line)
  (when (equal major-mode 'mentor-mode)
    (save-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mentor-insert-torrents)
        (mentor-sort)
        (goto-char (point-max))
        (insert "\n mentor " mentor-version " - rTorrent "
                mentor-rtorrent-client-version "/"
                mentor-rtorrent-library-version
                " (" mentor-rtorrent-name ")\n")))))

(defun mentor-show-download-files ()
  "Show file details for download at point."
  (interactive)
  (let ((download (mentor-get-item-at-point)))
    (when download
      (switch-to-buffer "*mentor: torrent details*")
      (setq mentor-files-selected-download download)
      (mentor-files-mode))))

(defun mentor-shutdown ()
  "Exit Mentor, killing any running rTorrent processes."
  (interactive)
  (when (y-or-n-p "Really shutdown mentor? ")
    (kill-buffer (current-buffer))
    ;; system.shutdown currently does nothing.  Oh, well.
    ;; (mentor-rpc-command "system.shutdown")
    ;; TODO: Clean shutdown...
    (ignore-errors
     (kill-buffer mentor-rtorrent-buffer-name))))


;;;; Torrent views

(defun mentor-view-torrent-list-add (tor)
  (let* ((id (mentor-item-property 'local_id tor))
         (view (intern mentor-current-view))
         (list (assq view mentor-view-torrent-list)))
    (push id (cdr list))))

(defun mentor-view-torrent-list-clear ()
  (let ((view (intern mentor-current-view)))
    (setq mentor-view-torrent-list
          (assq-delete-all view mentor-view-torrent-list))
    (setq mentor-view-torrent-list
          (cons (list view) mentor-view-torrent-list))))

(defun mentor-view-torrent-list-delete (&optional tor view)
  (let* ((view (or view (intern mentor-current-view)))
         (list (assq view mentor-view-torrent-list)))
    (delete (mentor-item-property 'local_id tor) list)))

(defun mentor-view-torrent-list-delete-all (&optional tor)
  (dolist (view mentor-view-torrent-list)
    (mentor-view-torrent-list-delete tor (car view))))


;;;; Download columns

(defun mentor-download-progress-column (download)
  (let* ((donev (mentor-item-property 'bytes_done download))
         (totalv (mentor-item-property 'size_bytes download))
         (done (abs (or donev 0)))
         (total (abs (or totalv 1)))
         (percent (* 100 (/ (+ 0.0 done) total))))
    (if (= (truncate percent) 100)
        ""
      (format "%2d%%" percent))))

(defun mentor-download-state-column (download)
  (let* ((h (mentor-item-property 'hashing download))
         (a (mentor-item-property 'is_active download))
         (o (mentor-item-property 'is_open download))
         (first-char (if (> h 0) "H" (if (= a 1) " " "S")))
         (second-char (if (= o 1) " " "C")))
    (concat first-char second-char)))

(defun mentor-download-speed-down-column (download)
  (let ((bytes (mentor-item-property 'down.rate download)))
    (if (> bytes 0)
        (mentor-bytes-to-kilobytes bytes)
      "")))

(defun mentor-download-speed-up-column (download)
  (let ((bytes (mentor-item-property 'up.rate download)))
    (if (> bytes 0)
        (mentor-bytes-to-kilobytes bytes)
      "")))

(defun mentor-download-size-progress-column (download)
  (let ((done (mentor-item-property 'bytes_done download))
        (total (mentor-item-property 'size_bytes download)))
    (if (= done total)
        (format "         %-.6s" (mentor-bytes-to-human total))
      (format "%6s / %-6s"
              (mentor-bytes-to-human done)
              (mentor-bytes-to-human total)))))

(defun mentor-download-priority-column (download)
  (let ((prio (mentor-item-property 'priority download)))
    (cond ((= 0 prio) "off")
          ((= 1 prio) "low")
          ((= 2 prio) "")
          ((= 3 prio) "hig"))))

(defun mentor-download-size-column (download)
  (let ((total (mentor-item-property 'size_bytes download)))
    (format "%4.6s" (mentor-bytes-to-human total))))

(defun mentor-remove-subdomains (domain)
  (replace-regexp-in-string
   "^\\([^.]*\\.\\)+\\([^.]+\\.[^.]+\\)" "\\2"
   domain))

(defun mentor-keep-domain-name (url)
  (replace-regexp-in-string
   "https?://\\([^/:]+\\)\\(:[0-9]+\\)?.*" "\\1"
   url))

(defun mentor-download-tracker-name-column (&optional download)
  (let* ((t_urls (mentor-item-property 't_url download))
         (t_is_enableds (mentor-item-property 't_is_enabled download))
         (active-trackers
          (cl-mapcar (lambda (url is_enabled) (when is_enabled url))
                     t_urls t_is_enableds))
         (main-tracker (if active-trackers (car active-trackers) ""))
         (shortened (mentor-remove-subdomains
                     (mentor-keep-domain-name main-tracker))))
    (if (>= (length shortened) 20)
        (seq-subseq shortened -20)
      (format "%20s" shortened))))

;;; Get dowload data from rTorrent

(defun mentor-download-get-size-done (torrent)
  (mentor-bytes-to-human
   (mentor-item-property 'bytes_done torrent)))

(defun mentor-download-get-size-total (torrent)
  (mentor-bytes-to-human
   (mentor-item-property 'size_bytes torrent)))

(defun mentor-download-has-view (download view)
  "Return t if DOWNLOAD has given VIEW."
  (member view (mentor-download-get-views download)))

(defun mentor-download-get-views (download)
  (mentor-item-property 'views download))

(defun mentor-download-set-priority-fun (val)
  (let ((hash (mentor-item-property 'hash))
        (prio (mentor-item-property 'priority)))
    (mentor-rpc-command "d.priority.set" hash (mentor-limit-num (+ prio val) 0 3))))


;;;; View functions

(defvar mentor-download-views)
(make-variable-buffer-local 'mentor-download-views)

(defconst mentor-custom-view-prefix "mentor-"
  "String to add to view name before adding it to rTorrent.")

(defun mentor-add-torrent-to-view (view)
  (interactive
   (list (mentor-prompt-complete "Add torrent to view: "
                                 (cl-remove-if-not 'mentor-views-is-custom-view
                                                mentor-download-views)
                                 nil mentor-current-view)))
  (let ((tor (mentor-get-item-at-point)))
   (when (not (mentor-views-is-custom-view view))
     (setq view (concat mentor-custom-view-prefix view)))
   (if (not (mentor-views-valid-view-name view))
       (message "Not a valid name for a view!")
     (if (or (mentor-views-is-view-defined view)
             (when (y-or-n-p (concat "View " view " was not found. Create it? "))
               (mentor-views-add view) t))
         (mentor-rpc-command "d.views.push_back_unique"
                             (mentor-item-property 'hash tor) view)
       (message "Nothing done")))))

(defconst mentor-download-default-views
  '("main" "name" "started" "stopped" "complete"
    "incomplete" "hashing" "seeding" "active"))

;; TODO find out what a valid name is in rTorrent
(defun mentor-views-valid-view-name (_name)
  t)

(defun mentor-set-view (new)
  (setq mentor-last-used-view (or mentor-current-view
                                  mentor-default-view))
  (setq mentor-current-view new)
  (setq mode-line-buffer-identification (concat "*mentor* " mentor-current-view)))

(defun mentor-switch-to-view (&optional new)
  (interactive)
  (when (null new)
    (setq new (mentor-prompt-complete
               "Show view: " mentor-download-views
               1 mentor-last-used-view)))
  (when (numberp new)
    (setq new (mentor-get-custom-view-name new)))
  (when (not (equal new mentor-current-view))
    (mentor-set-view new)
    (mentor-update)
    (message "Showing view: %s" mentor-current-view)))

(defun mentor-switch-to-view-1 () (interactive) (mentor-switch-to-view 1))
(defun mentor-switch-to-view-2 () (interactive) (mentor-switch-to-view 2))
(defun mentor-switch-to-view-3 () (interactive) (mentor-switch-to-view 3))
(defun mentor-switch-to-view-4 () (interactive) (mentor-switch-to-view 4))
(defun mentor-switch-to-view-5 () (interactive) (mentor-switch-to-view 5))
(defun mentor-switch-to-view-6 () (interactive) (mentor-switch-to-view 6))
(defun mentor-switch-to-view-7 () (interactive) (mentor-switch-to-view 7))
(defun mentor-switch-to-view-8 () (interactive) (mentor-switch-to-view 8))
(defun mentor-switch-to-view-9 () (interactive) (mentor-switch-to-view 9))
(defun mentor-switch-to-view-0 () (interactive) (mentor-switch-to-view 0))

(defun mentor-views-add (view)
  "Add VIEW to rTorrent's \"view_list\" and set the new view_filter.

SHOULD BE USED WITH CARE! Atleast in rTorrent 0.8.6, rTorrent
crashes if you try to add the same view twice!"
  (mentor-rpc-command "view.add" view)
  (setq mentor-download-views (cons view mentor-download-views))
  (mentor-views-update-filter view))

(defun mentor-views-init ()
  "Initialize views.
Gets all unique views from torrents, adds all views not
already in view_list and sets all new view_filters."
  ;; should always update the views before potentially adding new ones
  (mentor-views-update-views))

;; FIXME: this was part of mentor-views-init, but why?
  ;; (maphash
  ;;  (lambda (id torrent)
  ;;    (cl-mapcar (lambda (view)
  ;;              (when (and (mentor-views-is-custom-view view)
  ;;                         (not (mentor-views-is-view-defined view)))
  ;;                (mentor-views-add view)))
  ;;            (cdr (assoc 'views torrent))))
  ;;  mentor-items))

(defun mentor-views-update-views ()
  "Update view list with all views defined in rTorrent."
  (setq mentor-download-views (mentor-rpc-command "view.list")))

(defun mentor-views-update-filter (view)
  "Update view_filter for given VIEW.
You need to do this everytime you add/remove a torrent to a view
since rTorrent (at least as of 0.8.6) does not add/remove new
torrents to a view unless the filter is updated."
  (mentor-rpc-command "view.filter" view
                      (concat "d.views.has=" view)))

(defun mentor-views-update-filters ()
  "Update all view_filters for custom views in rTorrent."
  (mapc (lambda (view)
          (when (mentor-views-is-custom-view  view)
            (mentor-views-update-filter view)))
        mentor-download-views))

(defun mentor-views-is-view-defined (view)
  (member view mentor-download-views))

(defun mentor-views-is-custom-view (view)
  ;;(not (member view mentor-download-default-views)))
  (string-match (concat "^" mentor-custom-view-prefix) view))

(defun mentor-views-is-default-view (view)
  (member view mentor-download-default-views))


;;;; Utility functions

(defun mentor-limit-num (num min max)
  (cond ((< num min) min)
        ((> num max) max)
        (t num)))

(defun mentor-get-item-type ()
  (interactive)
  (get-text-property (point) 'type))

(defun mentor-prompt-complete (prompt list require-match default)
  (completing-read prompt list nil require-match nil default
                   mentor-last-used-view))

(defun mentor-get-custom-view-name (view-id)
  (cdr (assoc view-id mentor-custom-views)))

(defun mentor-bytes-to-human (bytes)
  "Convert BYTES to human readable and try to keep it short."
  (if bytes
      (let* ((bytes (if (stringp bytes) (string-to-number bytes) bytes))
             (kb 1024.0)
             (mb (* kb 1024.0))
             (gb (* mb 1024.0)))
        (cond ((< bytes 0) "???") ;; workaround for old xmlrpc-c
              ((= bytes 0.0) (format "%d" bytes))
              ((< bytes 999.0) (format "%dB" bytes))
              ((< bytes (* kb 999.5)) (format "%.0fK" (/ bytes kb)))
              ((< bytes (* mb 999.5))
               (let ((fmt (if (< bytes (* 9.95 mb))
                              "%.1fM"
                            "%.0fM")))
                 (format fmt (/ bytes mb))))
              ((< bytes (* gb 1000))
               (let ((fmt (if (< bytes (* 9.95 gb))
                              "%.1fG"
                            "%.0fG")))
                 (format fmt (/ bytes gb))))
              (t "1TB+")))
    ""))

(defun mentor-bytes-to-kilobytes (bytes)
  (if (numberp bytes)
      (if (< bytes 0)
          "???" ;; workaround for old xmlrpc-c
        (number-to-string (/ bytes 1024)))
    ""))

(defun mentor-enforce-length (str maxlen)
  "Return string of length MAXLEN with STR prefixed by spaces."
  (if (not str)
      (make-string (abs maxlen) ? )
    (format (concat "%" (number-to-string maxlen) "s")
            (substring str 0 (min (length str) (abs maxlen))))))

(defun mentor-file-sanity-check (file)
  (when (not (file-exists-p file))
    (error "No such file: %s" file))
  (when (= (nth 7 (file-attributes file 'string)) 0)
    (error "File is empty: %s" file)))

(provide 'mentor)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; indent-tabs-mode: nil
;; End:
;;; mentor.el ends here
