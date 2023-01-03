;;; mentor-trackers.el --- Mentor trackers view  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Jesse Gildersleve

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
(defvar mentor-tracker-selected-download)
(make-variable-buffer-local 'mentor-tracker-selected-download)
(put 'mentor-tracker-selected-download 'permanent-local t)

(declare-function mentor-item-property "mentor.el")
(declare-function mentor-get-item-at-point "mentor-data.el")
(declare-function mentor-rpc-command "mentor-rpc.el")
(define-key mentor-mode-map (kbd "T") 'mentor-tracker-open-view-at-point)

(setq mentor-tracker-rpc-t-methods
      '("t.url="
        "t.is_enabled="
        "t.scrape_complete="
        "t.scrape_downloaded="
        "t.scrape_incomplete="
        "t.success_counter="
        "t.failed_counter="
        "t.scrape_counter="))

(defvar mentor-tracker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "*") 'mentor-tracker-toggle)
    (define-key map (kbd "t") 'mentor-tracker-announce)
    (define-key map (kbd "T") 'mentor-tracker-announce-force)
    (define-key map (kbd "g") 'mentor-tracker-refresh)
    (define-key map (kbd "G") 'mentor-tracker-refresh)
    map)
  "Keymap used in `mentor-tracker-mode'.")

(define-derived-mode mentor-tracker-mode tabulated-list-mode "mentor-tracker-mode"
  "Major mode for looking at mentor tracker information.

\\{mentor-tracker-mode-map}"
  (setq tabulated-list-format [("URL"  70 t)
                               ("Enabled" 7 t)
                               ("Complete" 8 t)
                               ("Download" 8 t)
                               ("Incomplete" 10 t)
                               ("Success" 7 t)
                               ("Failed" 6 t)
                               ("Counter" 7 t)])
  (setq tabulated-list-padding 4)
  (setq tabulated-list-sort-key (cons "Enabled" nil))
  (setq mode-line-buffer-identification
        (concat "*mentor* "
                (mentor-item-property 'name mentor-tracker-selected-download)))
  (mentor-tracker-refresh))

(defun mentor-princ-to-string (x)
  "Convert to X string."
  (with-output-to-string (princ x)))

(defun mentor-tracker-announce ()
  "Announce yourself to the tracker."
  (interactive)
  (mentor-rpc-command "d.tracker_announce" mentor-tracker--hash)
  (mentor-tracker-refresh))

(defun mentor-tracker-announce-force ()
  "Force announce yourself to the tracker."
  (interactive)
  (mentor-rpc-command "d.tracker_announce.force" mentor-tracker--hash)
  (mentor-tracker-refresh))

(defun mentor-tracker-enable ()
  "Enable the tracker at the point."
  (interactive)
  (let* ((tracker-id (int-to-string (tabulated-list-get-id)))
         (tracker-hash (concat mentor-tracker--hash ":t" tracker-id)))
    (mentor-rpc-command "t.enable"  tracker-hash))
  (mentor-tracker-refresh))

(defun mentor-tracker-disable ()
  "Disable the tracker at the point."
  (interactive)
  (let* ((tracker-id (int-to-string (tabulated-list-get-id)))
         (tracker-hash (concat mentor-tracker--hash ":t" tracker-id)))
    (mentor-rpc-command "t.disable"  tracker-hash))
  (mentor-tracker-refresh))

(defun mentor-tracker-toggle ()
  "Toggle if the tracker at the point is enabled or disabled."
  (interactive)
  (if (= 1 (string-to-number (elt (tabulated-list-get-entry) 1)))
      (mentor-tracker-disable)
    (mentor-tracker-enable))
  (mentor-tracker-refresh))

(defun mentor-tracker-refresh ()
  "Refresh the list of trackers."
  (interactive)
  (setq tabulated-list-entries '())
  (mentor-tracker-build-tab-list)
  (tabulated-list-init-header)
  (tabulated-list-print t))

(defun mentor-tracker-open-view-at-point ()
  "Opens the tracker view for the torrent at point."
  (interactive)
  (setq mentor-tracker--hash (mentor-item-property 'hash))
  (let ((download (mentor-get-item-at-point)))
    (switch-to-buffer "*mentor: torrent details*")
    (setq mentor-tracker-selected-download download)
    (mentor-tracker-mode)))

(defun mentor-tracker-build-tab-list ()
  "Build the `tabulated-list-entries' for torrent."
  (let* ((methods= mentor-tracker-rpc-t-methods)
        (tracker-list (apply 'mentor-rpc-command "t.multicall" mentor-tracker--hash "" methods=))
        (id 0))
    (dolist (tracker-info tracker-list)
      (if tabulated-list-entries
          (setq tabulated-list-entries
                (append tabulated-list-entries `((,id ,(cl-map 'vector 'mentor-princ-to-string  tracker-info)))))
        (setq tabulated-list-entries
              `((,id ,(cl-map 'vector 'mentor-princ-to-string  tracker-info)))))
      (setq id (1+ id)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mentor-trackers.el ends here
