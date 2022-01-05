;;; mentor-rpc-tests.el --- Test suite for mentor-rpc.el -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Stefan Kangas.

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
(require 'mentor-rpc)

(ert-deftest mentor-rpc-join-t-methods ()
  (should
   (equal
    (mentor-rpc-join-t-methods
     '("t.url" "t.type" "t.is_enabled" "t.group" "t.scrape_complete"
       "t.scrape_incomplete" "t.scrape_downloaded"))
    "cat=\"$t.multicall=d.hash=,t.url=,cat=#,t.type=,cat=#,t.is_enabled=,cat=#,t.group=,cat=#,t.scrape_complete=,cat=#,t.scrape_incomplete=,cat=#,t.scrape_downloaded=,cat=#\"")))

;;; mentor-rpc-tests.el ends here
