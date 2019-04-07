;;; mentor-data.el --- Mentor data structures  -*- lexical-binding: t -*-

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

;; This library contains internal data structures used by Mentor.

;;; Code:

;;; Mentor items

(require 'cl-lib)

(declare-function mentor-view-torrent-list-add "mentor.el")
(defconst mentor-rpc-t-multicall-sep "#")

(cl-defstruct mentor-item
  "A structure containing an item that can be displayed
in a buffer, like a torrent, file, directory, peer etc."
  id data marked type)

(defvar mentor-items nil
  "Hash table containing all items for the current buffer.
This can be torrents, files, peers etc.  All values should be made
using `make-mentor-item'.")
(make-variable-buffer-local 'mentor-items)

(defun mentor-get-item (id)
  (gethash id mentor-items))

(defun mentor-item-id-at-point ()
  (get-text-property (point) 'field))

(defun mentor-get-item-at-point ()
  (mentor-get-item (mentor-item-id-at-point)))

(defun mentor-item-set-property (property value &optional item must-exist)
  "Set data PROPERTY to given VALUE of an item.

If ITEM is nil, use torrent at point.

If MUST-EXIST is non-nil, give a warning if the property does not
  already exist."
  
  (let ((it (or item
                (mentor-get-item-at-point)
                (error "There is no item here"))))
    (let ((prop (assq property (mentor-item-data it))))
      (if prop
          (setcdr prop value)
        (if must-exist
            (error "Tried updating non-existent property")
          (push (cons property value) (mentor-item-data it)))))))

(defun mentor-item-property (property &optional item)
  "Get PROPERTY for item at point or ITEM."
  (let ((it (or item
                (mentor-get-item-at-point)
                (error "There is no item here"))))
    (cdr (assq property (mentor-item-data it)))))

;;; Download data structure

(defun mentor-download-create (data)
  (make-mentor-item
   :id   (cdr (assq 'local_id data))
   :type 'torrent
   :marked nil
   :data data))

(defun mentor-download-update (new &optional is-init)
  "Add or update a torrent using data in NEW."
  (let* ((id  (mentor-item-property 'local_id new))
         (old (mentor-get-item id)))
    (when (and (null old)
               (not is-init))
      (signal 'mentor-need-init `("No such torrent" ,id)))
    (if is-init
        (progn (setf (mentor-item-marked new) nil)
               (puthash id new mentor-items))
      (dolist (row (mentor-item-data new))
        (let* ((p (car row))
               (v (cdr row)))
          (mentor-item-set-property p v old 'must-exist))))
    (mentor-view-torrent-list-add new)))

(defun mentor-data-download-update-from (d-methods t-methods values &optional is-init)
  "Parses results from mentor-rpc-d.multicall and updates download data.

If T-METHODS is nil, do not handle tracker data.

Assumes that VALUES is of the form (dval_0 dval_1 ... tvals), where dval_i
corresponds to (nth i D-METHODS) and tvals is a string of the form
\"tval_t0_p0#tval_t0_p1#...#tval_t1_p0#tval_t1p1#...#\", where tval_ti_pj
corresponds to (nth j T-METHODS) for the ith tracker. The # in this example
is the value of mentor-rpc-t-multicall-sep."
  (let ((result ()))
    (if t-methods
        (let ((d-values (butlast values))
              (t-values (butlast (split-string (car (last values)) mentor-rpc-t-multicall-sep)))
              (t-methods-len (length t-methods))
              (t-accum ()))
          (cl-mapc (lambda (m v) (push (cons m v) result)) d-methods d-values)
          ;; Group the values by tracker by chopping the list of length
          ;; num_trackers*num_t_methods into num_trackers lists of length
          ;; num_t_methods.
          (while t-values
            (push (cl-subseq t-values 0 t-methods-len) t-accum)
            (setq t-values (nthcdr t-methods-len t-values)))
          (setq t-accum (nreverse t-accum))
          ;; Group the values by property by transposing the list of values:
          ;; (tval_t0_p0 tval_t0_p1) (tval_t1_p0 tval_t1_p1) ->
          ;; (tval_t0_p0 tval_t1_p0) (tval_t0_p1 tval_t0_p1)
          (setq t-accum (apply #'cl-mapcar #'list t-accum))
          ;; Zip value-lists with method names:
          ;; ((tmthd0 . (tval_t0_p0 tval_t1_p0)) (tmthd1 . (tval_t0_p1 tval_t1_p1)))
          ;; ==
          ;; ((tmthd0 tval_t0_p0 tval_t1_p0) (tmthd1 tval_t0_p1 tval_t1_p1))
          ;; When one of these t properties is retrieved, is retrieved, the returned
          ;; value will be a list.
          (cl-mapc (lambda (m v) (push (cons m v) result)) t-methods t-accum))
      (cl-mapc (lambda (m v) (push (cons m v) result)) d-methods values))
    (mentor-download-update (mentor-download-create result) is-init)))


(put 'mentor-need-init
     'error-conditions
     '(error mentor-error mentor-need-init))

(provide 'mentor-data)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mentor-data.el ends here
