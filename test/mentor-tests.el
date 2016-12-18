;;; mentor-tests.el --- Test suite for mentor.el -*- lexical-binding: t -*-

;; Copyright (C) 2016 Stefan Kangas.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

;;; Code:

(require 'cl)
(require 'ert)

(ert-deftest mentor-rtorrent-already-running ()
  (let ((existent (generate-new-buffer "fooa")) ; incl. buffer
        (non-existent (generate-new-buffer-name "foob")) ; only name
        (rpc "<mock-value>"))
    (cl-letf (((symbol-function 'mentor-rpc-command)
               (lambda (x) t)))
      (should (equal (mentor-rtorrent-already-running nil rpc) nil))
      (should (equal (mentor-rtorrent-already-running existent rpc) t))
      (should (equal (mentor-rtorrent-already-running non-existent rpc) nil)))))

(provide 'mentor-tests)

;;; mentor-tests.el ends here
