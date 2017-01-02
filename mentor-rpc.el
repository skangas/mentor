;;; mentor-rpc.el --- Mentor XML-RPC handling  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 Stefan Kangas.

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

(require 'mentor-data)

(defvar mentor-rpc--rtorrent-url nil
  "The current rtorrent XML-RPC api URL.")

(defconst mentor-rpc-method-exclusions-regexp "d\\.get_\\(mode\\|custom.*\\|bitfield\\)"
  "Do not try methods that makes rtorrent crash.")

(defvar mentor-rpc--rtorrent-methods-cache nil)

(defvar url-http-response-status)

;; Silence compiler warnings
(defvar mentor-current-view)
(declare-function mentor-view-torrent-list-clear "mentor.el")
(declare-function mentor-download-update-from "mentor.el")

(defun mentor-rpc-command (&rest args)
  "Run command as an XML-RPC call to rtorrent.

ARGS is a list of strings to run."
  (let* ((url-http-response-status 200)
         (response (apply 'xml-rpc-method-call mentor-rpc--rtorrent-url args)))
    (if (equal response '((nil . "URL/HTTP Error: 200")))
        ;; Add warning about bug#23606.
        ;; Remove when Emacs 25 hits Debian stable.
        (if (and (string-match "localhost" mentor-rpc--rtorrent-url)
                 (< emacs-major-version 25))
            (error "Unable to connect to %s [try using 127.0.0.1 instead -- see bug#23606]" mentor-rpc--rtorrent-url)
          (error "Unable to connect to %s" mentor-rpc--rtorrent-url))
      response)))

(defun mentor-rpc-list-methods (&optional regexp)
  "Return a list of all available commands.

This uses the RPC method `system.listMethods'.

If REGEXP is specified it only returns the matching functions."
  (when (not mentor-rpc--rtorrent-methods-cache)
    (let ((methods (mentor-rpc-command "system.listMethods")))
      (setq mentor-rpc--rtorrent-methods-cache
            (delq nil
                  (mapcar (lambda (m)
                            (when (not (string-match mentor-rpc-method-exclusions-regexp m))
                              m))
                          methods)))))
  (if regexp
      (delq nil (mapcar (lambda (m)
                          (when (string-match regexp m)
                            m))
                        mentor-rpc--rtorrent-methods-cache))
    mentor-rpc--rtorrent-methods-cache))

;; General RPC commands, prefix c

(defun mentor-rpc-c-load (file &optional stopped)
  (let ((cmd (if stopped "load.verbose" "load.start_verbose")))
    (mentor-rpc-command cmd "" file)))

(defun mentor-rpc-c-load-raw (file &optional stopped)
  (let ((cmd (if stopped "load.raw" "load.raw_start"))
        (data (list
               :base64
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (setq buffer-file-coding-system 'binary)
                 (insert-file-contents-literally file)
                 (buffer-substring-no-properties (point-min) (point-max))))))
    (mentor-rpc-command cmd "" data)))

(defun mentor-rpc-c-execute2 (&rest args)
  (apply 'mentor-rpc-command "execute2" "" args))

;; Download RPC commands, prefix d

(defun mentor-rpc-d-close (&optional tor)
  (mentor-rpc-command "d.close" (mentor-item-property 'hash tor)))

(defun mentor-rpc-d-directory-set (new &optional tor)
  (mentor-item-set-property 'directory new)
  (mentor-rpc-command "d.directory.set" (mentor-item-property 'hash tor) new))

(defun mentor-rpc-d-erase (tor)
  (mentor-rpc-command "d.erase" (mentor-item-property 'hash tor)))

(defun mentor-rpc-d-start (&optional tor)
  (mentor-rpc-command "d.start" (mentor-item-property 'hash tor)))

(defun mentor-rpc-d-stop (&optional tor)
  (mentor-rpc-command "d.stop" (mentor-item-property 'hash tor)))

(defun mentor-rpc-d.multicall (methods &optional is-init)
  "Call `d.multicall2' with METHODS.

Optional argument IS-INIT if this is initializing."
  (let* ((methods= (mapcar (lambda (m) (concat m "=")) methods))
         (list-of-values (apply 'mentor-rpc-command "d.multicall2"
                                "" mentor-current-view methods=)))
    (mentor-view-torrent-list-clear)
    (let ((properties (mentor-rpc-methods-to-properties methods)))
      (dolist (values list-of-values)
        (mentor-download-update-from properties values is-init)))))

;; Download data

(defun mentor-rpc-methods-to-properties (methods)
  (mapcar
   (lambda (method)
    (intern
     (replace-regexp-in-string "^[df]\\.\\(get_\\)?\\|=$" "" method)))
   methods))

(defconst mentor-rpc-d-methods
  '("d.hash"
    "d.local_id"
    ;; "d.local_id_html"
    "d.base_filename"
    "d.base_path"
    ;; "d.bitfield"
    "d.bytes_done"
    "d.chunk_size"
    ;; "d.chunks_hashed"
    ;; "d.complete"
    ;; "d.completed_bytes"
    ;; "d.completed_chunks"
    ;; "d.connection_current"
    ;; "d.connection_leech"
    ;; "d.connection_seed"
    ;; "d.creation_date"
    ;; "d.custom"
    ;; "d.custom1"
    ;; "d.custom2"
    ;; "d.custom3"
    ;; "d.custom4"
    ;; "d.custom5"
    ;; "d.custom_throw"
    "d.directory"
    "d.directory_base"
    "d.down.rate"
    ;; "d.down.total"
    ;; "d.free_diskspace"
    "d.hashing"
    "d.hashing_failed"
    ;; "d.ignore_commands"
    ;; "d.left_bytes"
    ;; "d.loaded_file"
    ;; "d.max_file_size"
    ;; "d.max_size_pex"
    "d.message"
    ;; "d.mode"
    "d.name"
    ;; "d.peer_exchange"
    ;; "d.peers_accounted"
    ;; "d.peers_complete"
    ;; "d.peers_connected"
    ;; "d.peers_max"
    ;; "d.peers_min"
    ;; "d.peers_not_connected"
    "d.priority"
    ;; "d.priority_str"
    ;; "d.ratio"
    "d.size_bytes"
    ;; "d.size_chunks"
    ;; "d.size_files"
    ;; "d.size_pex"
    ;; "d.skip.rate"
    ;; "d.skip.total"
    "d.state"
    ;; "d.state_changed"
    ;; "d.state_counter"
    ;; "d.throttle_name"
    "d.tied_to_file"
    ;; "d.tracker_focus"
    ;; "d.tracker_numwant"
    ;; "d.tracker_size"
    "d.up.rate"
    "d.up.total"
    ;; "d.uploads_max"
    "d.is_active"
    "d.is_hash_checked"
    "d.is_hash_checking"
    "d.is_multi_file"
    "d.is_open"
    "d.is_not_partially_done"
    "d.is_pex_active"
    "d.is_partially_done"
    "d.is_private"
    "d.views"))

(defconst mentor-rpc-volatile-d-methods
  '("d.local_id" ;; must not be removed
    "d.base_path"        "d.bytes_done"
    "d.directory"        "d.down.rate"
    "d.hashing"          "d.hashing_failed"
    "d.message"
    "d.priority"         "d.chunk_size"
    "d.up.rate"          "d.up.total"
    "d.state"            "d.views"
    "d.is_active"        "d.is_hash_checked"
    "d.is_hash_checking" "d.is_open"
    "d.is_pex_active"))

(defun mentor-rpc-t-multicall (&rest args)
  (apply 'mentor-rpc-command "t.multicall" args))

(defun mentor-rpc-t-get-tracker-info (download)
  (mentor-rpc-t-multicall
   (mentor-item-property 'hash download)
   "" "t.url=" "t.is_enabled="))

;; sys.multicall -- unused?

;; TODO: Is sys.multicall a possible optimization for later?  From wiki:
;;     Process an array of calls, and return an array of
;;     results. Calls should be structs of the form {'methodName':
;;     string, 'params': array}. Each result will either be a
;;     single-item array containg the result value, or a struct of the
;;     form {'faultCode': int, 'faultString': string}. This is useful
;;     when you need to make lots of small calls without lots of round
;;     trips. See rTorrent-system_multicall for syntax.

(defun mentor-rpc--multicall-string (method &rest args)
  (list (cons "methodName" method) (cons "params" args)))

(defun mentor-rpc-sys-multicall (&rest calls)
  "Perform a `system.multicall' with CALLS.

CALLS is a list of lists where the first element is the method
name and all consecutive elements is its arguments."
  (mentor-rpc-command
   "system.multicall"
   (mapcar (lambda (c)
             (apply 'mentor--multicall-string
                    (car c) (cdr c))) calls)))

(provide 'mentor-rpc)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mentor-rpc.el ends here
