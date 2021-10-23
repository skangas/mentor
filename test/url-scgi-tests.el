;;; url-scgi-tests.el --- Test suite for url-scgi.el -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021 Stefan Kangas.

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

(require 'ert)
(require 'url-scgi)

(ert-deftest url-scgi-string-to-netstring ()
  (should (equal (url-scgi-string-to-netstring "abcde") "5:abcde,")))

(ert-deftest url-scgi-add-null-bytes ()
  (should (equal (url-scgi-add-null-bytes "foo") "foo\^@"))
  (should (equal (url-scgi-add-null-bytes "foo" "bar") "foo\^@bar\^@")))

(ert-deftest url-scgi-make-request-header ()
  (should (equal (url-scgi-make-request-header "foobar")
                 "24:CONTENT_LENGTH\^@6\^@SCGI\^@1\^@,")))

(ert-deftest url-scgi-create-request ()
  (let ((url-request-data "foobar"))
    (should (equal (url-scgi-create-request)
                   "24:CONTENT_LENGTH\^@6\^@SCGI\^@1\^@,foobar"))))

(ert-deftest url-scgi-handle-home-dir ()
  (should (equal (url-scgi-handle-home-dir "/~/foo")
                 (expand-file-name "~/foo")))
  (should (equal (url-scgi-handle-home-dir "/foo") "/foo")))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; url-scgi-tests.el ends here
