;;; mentor-files.el --- Mentor file screen  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019 Stefan Kangas.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
;; along with Mentor.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This file contains code for the Mentor file view.

;;; Code:

(require 'cl-lib)

;; Silence compiler warnings
(defvar mentor-set-priority-fun)
(defvar mentor-item-update-this-fun)
(defvar mentor--columns-var)
(declare-function mentor-get-item-at-point "mentor.el")
(declare-function mentor-mode "mentor.el")
(declare-function mentor-item-property "mentor.el")
(declare-function mentor-init-header-line "mentor.el")
(declare-function mentor-get-item-type "mentor.el")
(declare-function mentor-forward-item "mentor.el")
(declare-function mentor-beginning-of-item "mentor.el")
(declare-function mentor-limit-num "mentor.el")
(declare-function find-if "mentor.el")
(declare-function mentor-rpc-methods-to-properties "mentor.el")
(declare-function mentor-process-view-columns "mentor.el")
(declare-function mentor-previous-item "mentor.el")
(declare-function mentor-mark "mentor.el")
(declare-function mentor-reload-header-line "mentor.el")
(declare-function mentor-bytes-to-human "mentor.el")
(declare-function mentor-rpc-command "mentor-rpc.el")

(cl-defstruct mentor-file
  "The datastructure that contains the information about torrent
files.  A mentor-file can be either a regular file or a filename
and if it is the latter it will contain a list of the files it
contain.  If it is a regular file it will contain an id which is
the integer index used by rtorrent to identify this file."
  name show marked size completed_chunks
  size_chunks priority files type id)

(defvar mentor-files-selected-download nil)
(make-variable-buffer-local 'mentor-files-selected-download)
(put 'mentor-files-selected-download 'permanent-local t)

(defvar mentor-selected-torrent-info '())

(defconst mentor-volatile-rpc-f-methods
  '("f.priority" "f.completed_chunks" "f.size_chunks"))

(defvar mentor-file-detail-columns
  '(((mentor-file-progress) -5 "Cmp")
    ((mentor-file-prio-string) -5 "Pri")
    ((mentor-file-readable-size) 4 "Size")
    (nil 6 "File")))
(defvar mentor-file-detail-width 18)

(defvar mentor-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") 'mentor-files-details-next-directory)
    (define-key map (kbd "P") 'mentor-files-details-previous-directory)

    (define-key map (kbd "g") 'mentor-files-update)
    (define-key map (kbd "G") 'mentor-files-reload)

    map)
  "Keymap used in `mentor-files-mode'.")

(define-derived-mode mentor-files-mode mentor-mode "mentor files"
  "Mode for changing status of files in a download.

\\{mentor-files-mode-map}"
  :group 'mentor
  (setq mentor-set-priority-fun 'mentor-files-set-priority-fun)
  ;; FIXME: Add function to update only one item
  (setq mentor-item-update-this-fun 'mentor-files-update)
  (setq mentor--columns-var  'mentor-file-detail-columns)

  (mentor-files-update t)
  (setq mode-line-buffer-identification
        (concat "*mentor* "
                (mentor-item-property 'name mentor-files-selected-download)))
  (mentor-init-header-line)
  (if (not (mentor-get-item-type))
      (mentor-forward-item 1)
    (mentor-beginning-of-item)))

(defun mentor-file-at-point ()
  (get-text-property (point) 'file))

(defun mentor-file-is-dir (file)
  (and (mentor-file-p file) (eq 'dir (mentor-file-type file))))

(defun mentor-files--priority-set (hash file val)
  (mentor-rpc-command "f.priority.set"
                      (format "%s:f%s" hash (mentor-file-id file))
                      (mentor-limit-num (+ (mentor-file-priority file) val)
                                        0 2)))

(defun mentor-files-set-priority-fun (val &optional file)
  (let* ((hash (mentor-item-property 'hash mentor-files-selected-download))
         (file (or file
                  (mentor-file-at-point))))
    (if (mentor-file-is-dir file)
        (dolist (file (mentor-file-files file))
          (mentor-files-set-priority-fun val file))
      (mentor-files--priority-set hash file val))
    (mentor-rpc-command "d.update_priorities" hash)))

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
         (file^ (cl-find-if pred (mentor-file-files dir))))
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

(defun mentor-details-add-files (name-list)
  (let ((root (make-mentor-file :name "/"
                                :type 'dir
                                :id -1
                                :show t))
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
                                             :type 'dir
                                             :show nil
                                             :id (cl-decf dir-id)))
            (mentor-file-add-file last-dir curr-dir)
            (setq last-dir curr-dir))
          (setq file (pop names))
          (cl-decf len))
        (setq file (make-mentor-file :name file :type 'file
                                     :id (cl-incf file-id)))
        (mentor-file-add-file last-dir file)
        (puthash file-id file all-files)))
    (push (cons 'files all-files) mentor-selected-torrent-info)
    (push (cons 'root root) mentor-selected-torrent-info)))

(defun mentor--concat-symbols (&rest symbols)
  (intern (apply 'concat (mapcar 'symbol-name symbols))))

;;; Interactive commands

(defun mentor-files-update (&optional add-files)
  (interactive)
  (when add-files
    (setq mentor-selected-torrent-info
          (assq-delete-all 'root mentor-selected-torrent-info))
    (setq mentor-selected-torrent-info
          (assq-delete-all 'files mentor-selected-torrent-info)))
  (let* ((tor mentor-files-selected-download)
         (hash (mentor-item-property 'hash tor))
         (methods (if add-files
                      (cons "f.path_components" mentor-volatile-rpc-f-methods)
                    mentor-volatile-rpc-f-methods))
         (methods= (mapcar (lambda (m) (concat m "=")) methods))
         (value-list (apply 'mentor-rpc-command
                            "f.multicall" hash "" methods=)))
    (when add-files
      (mentor-details-add-files (mapcar 'car value-list))
      (setq value-list (mapcar 'cdr value-list)))
    (let ((files (cdr (assq 'files mentor-selected-torrent-info)))
          (id -1)
          (properties (mentor-rpc-methods-to-properties
                       mentor-volatile-rpc-f-methods)))
      (dolist (values value-list)
        (let ((filex (gethash (cl-incf id) files)))
          (mapc (lambda (p)
                  (let* ((file-fun (mentor--concat-symbols 'mentor-file- p))
                         (val (pop values)))
                    (eval `(setf (,file-fun ,filex) ,val))))
                properties)))))
  (mentor-details-redisplay))

(defun mentor-files-reload ()
  (interactive)
  (mentor-files-update t))

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
          (mentor-mark)))
      (cl-incf count))))

(defun mentor-details-redisplay ()
  (interactive)
  (let* ((inhibit-read-only t)
         (pos (point))
         (root (cdr (assq 'root mentor-selected-torrent-info))))
    (erase-buffer)
    (mentor-reload-header-line)
    (mentor-insert-dir-content root)
    (goto-char pos)))

(defun mentor-files-details-next-directory ()
  (interactive)
  (when (mentor-file-is-dir (mentor-file-at-point))
    (mentor-forward-item 1))
  (while (not (mentor-file-is-dir (mentor-file-at-point)))
    (mentor-forward-item 1))
  (mentor-beginning-of-item))

(defun mentor-files-details-previous-directory ()
  (interactive)
  (when (mentor-file-is-dir (mentor-file-at-point))
    (mentor-previous-item))
  (while (not (mentor-file-is-dir (mentor-file-at-point)))
    (mentor-previous-item)
    (mentor-beginning-of-item)))

(defun mentor-mark-dir (file &optional clear-mark no-redisplay)
  (interactive)
  (error "FIXME")
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

;; Table

(defun mentor-file-prio-string (file)
  (let ((prio (mentor-file-priority file)))
    (cond ((eq prio 0) "off")
          ((eq prio 1) "")
          ((eq prio 2) "hig"))))

(defun mentor-file-progress (file)
  (let* ((done (mentor-file-completed_chunks file))
         (size (mentor-file-size_chunks file)))
    (format "%d" (* 100 (/ (+ 0.0 done) size)))))

(defun mentor-file-readable-size (file)
  (let* ((chunk-size (mentor-item-property
                      'chunk_size mentor-files-selected-download)))
    (mentor-bytes-to-human
     (* chunk-size (mentor-file-size_chunks file)))))

(provide 'mentor-files)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mentor-files.el ends here
