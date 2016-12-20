;;; mentor-files.el --- Mentor file screen  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Stefan Kangas.

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

;; This library contains functions to speak with rTorrent over XML-RPC.

;;; Code:

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

(defconst mentor-volatile-rpc-f-methods
  '("f.priority" "f.completed_chunks" "f.size_chunks"))

(define-minor-mode mentor-torrent-details-mode
  "Minor mode for managing a torrent in mentor."
  :group mentor
  :init-value nil
  :lighter nil
  :keymap mentor-torrent-details-mode-map)

(defun mentor-file-at-point ()
  (get-text-property (point) 'file))

(defun mentor-file-is-dir (file)
  (and (mentor-file-p file) (eq 'dir (mentor-file-type file))))

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
                      'chunk_size mentor-selected-torrent)))
    (mentor-bytes-to-human
     (* chunk-size (mentor-file-size_chunks file)))))

(defun mentor-file-set-priority-fun (val)
  (let* ((file (mentor-file-at-point))
         (id   (mentor-file-id file))
         (prio (mentor-file-priority file))
         (hash (mentor-item-property 'hash mentor-selected-torrent)))
    (when (not (mentor-file-is-dir file))
      (list "f.priority.set" hash id (mentor-limit-num (+ prio val) 0 2)))))

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

(defun mentor-torrent-detail-screen ()
  "Show information about the specified torrent or the torrent at
point."
  (interactive)
  (let ((tor (mentor-get-item-at-point)))
    (switch-to-buffer "*mentor: torrent details*")
    (setq mentor-sub-mode 'file-details)
    (mentor-mode)
    (setq mentor-set-priority-fun 'mentor-file-set-priority-fun)
    (setq mentor-columns-var  'mentor-file-detail-columns)
    (mentor-reload-header-line)
    (mentor-torrent-details-mode t)
    (setq mentor-selected-torrent tor)
    (mentor-files-update t)
    (mentor-details-redisplay)
    (setq mode-line-buffer-identification (concat "*mentor* "
                                                  (mentor-item-property 'name tor)))
    (if (not (mentor-get-item-type))
        (mentor-next-item 1 t)
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
    (push (cons 'files all-files) mentor-selected-torrent-info)
    (push (cons 'root root) mentor-selected-torrent-info)))

;; TODO: benchmark if add-files nil really means any performance gain
;;       for large examples
(defun mentor-files-update (&optional add-files)
  (interactive)
  (when add-files
    (setq mentor-selected-torrent-info
          (assq-delete-all 'root mentor-selected-torrent-info))
    (setq mentor-selected-torrent-info
          (assq-delete-all 'files mentor-selected-torrent-info)))
  (let* ((tor mentor-selected-torrent)
         (hash (mentor-item-property 'hash tor))
         (methods mentor-volatile-rpc-f-methods)
         (methods+ (mapcar
                    'mentor-get-some-methods-as-string
                    (if add-files
                        (cons "f.path_components" methods)
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
        (let ((_file (gethash (incf id) files)))
          (mapc (lambda (p)
                  (let* ((file-fun (mentor-concat-symbols 'mentor-file- p))
                         (val (if (string-match mentor-methods-to-get-as-string
                                                (symbol-name p))
                                  (string-to-number (pop values))
                                (pop values))))
                    (eval `(setf (,file-fun _file) ,val))))
                properties)))))
  (mentor-details-redisplay))

(defvar mentor-file-detail-columns
  '(((mentor-file-progress) -5 "Cmp")
    ((mentor-file-prio-string) -5 "Pri")
    ((mentor-file-readable-size) 6 "Size")
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
          (mentor-mark)))
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


;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mentor.el ends here
