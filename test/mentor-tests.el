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
               (lambda (_) t)))
      (should (equal (mentor-rtorrent-already-running nil rpc) nil))
      (should (equal (mentor-rtorrent-already-running existent rpc) t))
      (should (equal (mentor-rtorrent-already-running non-existent rpc) nil)))))

(ert-deftest mentor-rtorrent-bytes-to-human ()
  (let* ((kb 1024.0)
         (mb (* 1024.0 kb))
         (gb (* 1024.0 mb))
         (tb (* 1024.0 tb)))
    (should (equal (mentor-bytes-to-human 0)            "0"))
    (should (equal (mentor-bytes-to-human 1)            "1B"))
    (should (equal (mentor-bytes-to-human 998)          "998B"))
    (should (equal (mentor-bytes-to-human 999)          "1K"))
    (should (equal (mentor-bytes-to-human kb)           "1K"))
    (should (equal (mentor-bytes-to-human (* kb 5.3))   "5K"))
    (should (equal (mentor-bytes-to-human (* kb 15))    "15K"))
    (should (equal (mentor-bytes-to-human (* kb 999.5)) "1.0M"))
    (should (equal (mentor-bytes-to-human (* kb 1000))  "1.0M"))
    (should (equal (mentor-bytes-to-human mb)           "1.0M"))
    (should (equal (mentor-bytes-to-human (* mb 5))     "5.0M"))
    (should (equal (mentor-bytes-to-human (* mb 9.95))  "10M"))
    (should (equal (mentor-bytes-to-human (* mb 15.4))  "15M"))
    (should (equal (mentor-bytes-to-human (* mb 999.5)) "1.0G"))
    (should (equal (mentor-bytes-to-human (* mb 1000))  "1.0G"))
    (should (equal (mentor-bytes-to-human (* mb 5000))  "4.9G"))
    (should (equal (mentor-bytes-to-human gb)           "1.0G"))
    (should (equal (mentor-bytes-to-human (* gb 9.95))  "10G"))
    (should (equal (mentor-bytes-to-human (* gb 15.4))  "15G"))
    (should (equal (mentor-bytes-to-human (* tb 1))     "1TB+"))))

(provide 'mentor-tests)

;;; mentor-tests.el ends here
