;;; mentor-rpc.el --- Mentor XML-RPC handling  -*- lexical-binding: t -*-

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

(require 'mentor-data)

(defvar mentor--rtorrent-url nil
  "The current rtorrent XML-RPC api URL.")

(defvar mentor-method-exclusions-regexp "d\\.get_\\(mode\\|custom.*\\|bitfield\\)"
  "Do not try methods that makes rtorrent crash.")

(defvar mentor-rtorrent-rpc-methods-cache nil)

(defun mentor-rpc-command (&rest args)
  "Run command as an XML-RPC call to rtorrent.

ARGS is a list of strings to run."
  (let* ((response (apply 'xml-rpc-method-call mentor--rtorrent-url args)))
    (if (equal response '((nil . "URL/HTTP Error: 200")))
        ;; Add warning about bug#23606.
        ;; Remove when Emacs 25 hits Debian stable.
        (if (and (string-match "localhost" mentor--rtorrent-url)
                 (< emacs-major-version 25))
            (error "Unable to connect to %s [try using 127.0.0.1 instead -- see bug#23606]" mentor--rtorrent-url)
          (error "Unable to connect to %s" mentor--rtorrent-url))
      response)))

(defun mentor-rpc-list-methods (&optional regexp)
  "Return a list of all available commands.

This uses the RPC method `system.listMethods'.

If REGEXP is specified it only returns the matching functions."
  (when (not mentor-rtorrent-rpc-methods-cache)
    (let ((methods (mentor-rpc-command "system.listMethods")))
      (setq mentor-rtorrent-rpc-methods-cache
            (delq nil
                  (mapcar (lambda (m)
                            (when (not (string-match mentor-method-exclusions-regexp m))
                              m))
                          methods)))))
  (if regexp
      (delq nil (mapcar (lambda (m)
                          (when (string-match regexp m)
                            m))
                        mentor-rtorrent-rpc-methods-cache))
    mentor-rtorrent-rpc-methods-cache))

;; General RPC commands, prefix c

(defun mentor-c-load (file &optional stopped)
  (let ((cmd (if stopped "load.verbose" "load.start_verbose")))
    (mentor-rpc-command cmd "" file)))

(defun mentor-c-load-raw (file &optional stopped)
  (let ((cmd (if stopped "load.raw" "load.raw_start"))
        (data (list
               :base64
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (setq buffer-file-coding-system 'binary)
                 (insert-file-contents-literally file)
                 (buffer-substring-no-properties (point-min) (point-max))))))
    (mentor-rpc-command cmd "" data)))

(defun mentor-c-use-deprecated-set (arg)
  "Same as command line -D flag."
  (mentor-rpc-command "method.use_deprecated.set" "" arg))

;; Download RPC commands, prefix d

(defun mentor-d-close (&optional tor)
  (mentor-rpc-command "d.close" (mentor-d-get-hash tor)))

(defun mentor-d-erase (tor)
  (mentor-rpc-command "d.erase" (mentor-d-get-hash tor)))

(defun mentor-d-get-base-path (&optional tor)
  (mentor-item-property 'base_path tor))

(defun mentor-d-get-directory (&optional tor)
  (mentor-item-property 'directory tor))

(defun mentor-d-get-hash (&optional tor)
  (mentor-item-property 'hash tor))

(defun mentor-d-get-local-id (tor)
  (mentor-item-property 'local_id tor))

(defun mentor-d-get-name (&optional tor)
  (mentor-item-property 'name tor))

(defun mentor-d-is-active (&optional tor)
  (= (mentor-item-property 'is_active tor) 1))

(defun mentor-d-is-multi-file (&optional tor)
  (= (mentor-item-property 'is_multi_file tor) 1))

(defun mentor-d-set-directory (new &optional tor)
  ;; FIXME: Is this the only property that needs updating?
  (mentor-item-set-property 'directory new)
  (mentor-rpc-command "d.directory.set" (mentor-d-get-hash tor) new))

(defun mentor-d-start (&optional tor)
  (mentor-rpc-command "d.start" (mentor-d-get-hash tor)))

(defun mentor-d-stop (&optional tor)
  (mentor-rpc-command "d.stop" (mentor-d-get-hash tor)))

(defun mentor-execute (&rest args)
  (apply 'mentor-rpc-command "execute2" "" args))

;; Multicall

(defun mentor-rpc-d.multicall (methods &optional is-init)
  "Call `d.multicall2' with METHODS.

Optional argument IS-INIT if this is initializing."
  (let* ((methods+ (mapcar 'mentor-get-some-methods-as-string methods))
         (methods= (mapcar (lambda (m) (concat m "=")) methods+))
         (list-of-values (apply 'mentor-rpc-command "d.multicall2"
                                "" mentor-current-view methods=)))
    (mentor-view-torrent-list-clear)
    (dolist (values list-of-values)
      (mentor-download-update-from methods values is-init))))

;; Download data

(defconst mentor-methods-to-get-as-string
  (regexp-opt '("bytes_done" "completed_bytes"
                "left_bytes" "size_bytes" "chunk_size"
                "completed_chunks" "size_chunks"))
  "Methods that should be prefixed with cat= when fetched.")

(defun mentor-get-some-methods-as-string (method)
  "Used to get some properties as a string, since older versions
of libxmlrpc-c cannot handle integers longer than 4 bytes."
  (let ((re (concat "\\(?:[df]\\.get_"
                    mentor-methods-to-get-as-string
                    "\\)")))
    (if (string-match re method)
        (concat "cat=$" method)
      method)))

(defun mentor-rpc-method-to-property (method)
  (intern
   (replace-regexp-in-string "^[df]\\.\\(get_\\)?\\|=$" "" method)))

(defun mentor-rpc-value-to-real-value (method value)
  (if (and (string-match mentor-methods-to-get-as-string method)
           (stringp value))
      (string-to-number value)
    value))

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
    ;; "d.message"
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

(defconst mentor-volatile-rpc-d-methods
  '("d.local_id" ;; must not be removed
    "d.base_path"        "d.bytes_done"
    "d.directory"        "d.down.rate"
    "d.hashing"          "d.hashing_failed"
    "d.priority"         "d.chunk_size"
    "d.up.rate"          "d.up.total"
    "d.state"            "d.views"
    "d.is_active"        "d.is_hash_checked"
    "d.is_hash_checking" "d.is_open"
    "d.is_pex_active"))

;; sys.multicall -- unused?

;; TODO: Is sys.multicall a possible optimization for later?  From wiki:
;;     Process an array of calls, and return an array of
;;     results. Calls should be structs of the form {'methodName':
;;     string, 'params': array}. Each result will either be a
;;     single-item array containg the result value, or a struct of the
;;     form {'faultCode': int, 'faultString': string}. This is useful
;;     when you need to make lots of small calls without lots of round
;;     trips. See rTorrent-system_multicall for syntax.

(defun mentor--multicall-string (method &rest args)
  (list (cons "methodName" method) (cons "params" args)))

(defun mentor-sys-multicall (&rest calls)
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
