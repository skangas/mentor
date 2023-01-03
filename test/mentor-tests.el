;;; mentor-tests.el --- Test suite for mentor.el -*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Stefan Kangas.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'mentor)

(ert-deftest mentor-normalize-rpc-url ()
  ;; No difference
  (should (equal (mentor-normalize-rpc-url "scgi://localhost:123")         "scgi://localhost:123"))
  (should (equal (mentor-normalize-rpc-url "scgi://~/.rtorrent.rpc")       "scgi://~/.rtorrent.rpc"))
  (should (equal (mentor-normalize-rpc-url "scgi:///path/to/rtorrent.rpc") "scgi:///path/to/rtorrent.rpc"))
  (should (equal (mentor-normalize-rpc-url "http://localhost:8080/RPC")    "http://localhost:8080/RPC"))
  (should (equal (mentor-normalize-rpc-url "https://localhost:8080/RPC")   "https://localhost:8080/RPC"))
  ;; Add scgi
  (should (equal (mentor-normalize-rpc-url "~/.rtorrent.rpc")       "scgi://~/.rtorrent.rpc"))
  (should (equal (mentor-normalize-rpc-url "/path/to/rtorrent.rpc") "scgi:///path/to/rtorrent.rpc")))

(ert-deftest mentor-rtorrent-already-running ()
  (let ((existent (generate-new-buffer "fooa")) ; incl. buffer
        (non-existent (generate-new-buffer-name "foob"))) ; only name
    (unwind-protect
        (cl-letf (((symbol-function 'mentor-rpc-command)
                   (lambda (_) t)))
          (should (equal (mentor-rtorrent-already-running nil) nil))
          (should (equal (mentor-rtorrent-already-running existent) t))
          (should (equal (mentor-rtorrent-already-running non-existent) nil)))
      (ignore-errors
        (kill-buffer existent))
      (ignore-errors
        (kill-buffer non-existent)))))

(ert-deftest mentor-rtorrent-keep-domain-name ()
  (should (equal (mentor-keep-domain-name "http://foo.bar1.com/announce?xxxx")
                 "foo.bar1.com"))
  (should (equal (mentor-keep-domain-name "https://baz.bar2.com/announce?xxxx")
                 "baz.bar2.com")))

(ert-deftest mentor-remove-subdomains ()
  (should (equal (mentor-remove-subdomains "foo.bar.baz.com") "baz.com"))
  (should (equal (mentor-remove-subdomains "bar.baz1.com") "baz1.com"))
  (should (equal (mentor-remove-subdomains "baz1.com") "baz1.com"))
  (should (equal (mentor-remove-subdomains "localhost") "localhost")))

(ert-deftest mentor-limit-num ()
  (should (= (mentor-limit-num 1 3 5) 3))
  (should (= (mentor-limit-num 3 3 5) 3))
  (should (= (mentor-limit-num 5 3 5) 5))
  (should (= (mentor-limit-num 9 3 5) 5)))

(ert-deftest mentor-rtorrent-bytes-to-human ()
  (let* ((kb 1024.0)
         (mb (* 1024.0 kb))
         (gb (* 1024.0 mb))
         (tb (* 1024.0 gb)))
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

(ert-deftest mentor-rtorrent-enforce-length ()
  (should (equal (mentor-enforce-length nil 2)    "  "))
  (should (equal (mentor-enforce-length "abc" 0)  ""))
  (should (equal (mentor-enforce-length "abc" 2)  "ab"))
  (should (equal (mentor-enforce-length "abc" -2) "ab"))
  (should (equal (mentor-enforce-length "abc" 3)  "abc"))
  (should (equal (mentor-enforce-length "abc" -4) "abc "))
  (should (equal (mentor-enforce-length "abc" 4)  " abc")))

(provide 'mentor-tests)

;;; mentor-tests.el ends here
