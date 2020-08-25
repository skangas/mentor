;;; mentor-trackers.el --- Mentor trackers view  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Jesse Gildersleve.

;; Author: Jesse Gildersleve <jessejohngildersleve@protonmail.com>

;; This file is NOT part of GNU Emacs.

;; Mentor is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Mentor is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Mentor.  If not, see <https://www.gnu.org/licenses>.

;;; Commentary:

;; This file contains code for the Mentor trackers view.

;;; Code:

(defvar mentor-tracker-rpc-t-methods)
(defvar mentor-mode-map)
(defvar mentor-tracker--hash)
(declare-function mentor-item-property "mentor.el")
(define-key mentor-mode-map (kbd "T") 'mentor-open-tracker-view-at-point)

(setq mentor-tracker-rpc-t-methods
      '("t.url"
        "t.is_enabled"
        "t.scrape_complete"
        "t.scrape_downloaded"
        "t.scrape_incomplete"
        "t.success_counter"
        "t.failed_counter"
        "t.scrape_counter"))

(defun mentor-open-tracker-view-at-point ()
  "Opens the tracker view for the torrent at point."
  (interactive)
  (setq mentor-tracker--hash (mentor-item-property 'hash))
  (setq mode-line-buffer-identification
        (concat "*mentor* "
                (mentor-item-property 'name)))
  (switch-to-buffer "*mentor: torrent details*")
  (mentor-tracker-mode))

(defun princ-to-string (x)
  "Convert to X string."
  (with-output-to-string (princ x)))

(defun mentor-build-tracker-tab-list ()
  "Build the \"tabulated-list-entries\" for torrent."
  (let* ((methods= (mapcar (lambda (m) (concat m "=")) mentor-tracker-rpc-t-methods))
        (tracker-list (apply 'mentor-rpc-command "t.multicall" mentor-tracker--hash "" methods=))
        (id 0))
    (dolist (tracker-info tracker-list)
      (if tabulated-list-entries
          (setq tabulated-list-entries
                (append tabulated-list-entries (list (list id (cl-map 'vector 'princ-to-string  tracker-info)))))
        (setq tabulated-list-entries
              (list (list id (cl-map 'vector 'princ-to-string  tracker-info)))))
      (setq id (1+ id)))))


(define-derived-mode mentor-tracker-mode tabulated-list-mode "mentor-tracker-mode"
  "Major mode for looking at mentor tracker information."
  (setq tabulated-list-format [("Enabled" 7 t)
                               ("URL"  70 t)
                               ("Complete" 8 t)
                               ("Download" 8 t)
                               ("Incomplete" 10 t)
                               ("Success" 7 t)
                               ("Failed" 6 t)
                               ("Counter" 0 t)])
  (setq tabulated-list-entries '())
  (setq tabulated-list-padding 4)
  (setq tabulated-list-sort-key (cons "Enabled" nil))
  (mentor-build-tracker-tab-list)
  (tabulated-list-init-header)
  (tabulated-list-print t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mentor-trackers.el ends here
