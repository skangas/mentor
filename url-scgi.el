;;; url-scgi.el --- SCGI Uniform Resource Locator retrieval code

;; Copyright (C) 2011-2016 Stefan Kangas.

;; Author: Stefan Kangas <stefankangas@gmail.com>
;; Version: 0.3
;; Keywords: comm, data, processes, scgi

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

;; The SCGI specification document can be found at:
;;
;; http://python.ca/scgi/protocol.txt

;;; Change Log:

;; 0.3 Support scgi over local socket

;; 0.2 Support Emacs 24

;; 0.1 First public version

;;; Code:

(require 'url-parse)

(defvar url-scgi-version "0.3"
  "The version of scgi that you're using.")

(defvar url-scgi-connection-opened)

(defconst url-scgi-asynchronous-p t "SCGI retrievals are asynchronous.")

;; Silence byte-compiler
(defvar url-callback-function)
(defvar url-callback-arguments)
(defvar url-current-object)
(defvar url-request-data)

(defun url-scgi-string-to-netstring (str)
  "Converts a string into a SCGI protocol netstring."
  (format "%d:%s," (length str) str))

(defun url-scgi-add-null-bytes (&rest args)
  (apply 'concat (mapcar (lambda (a) (concat a "\000")) args)))

(defun url-scgi-make-request-header (data)
  (url-scgi-string-to-netstring
   (url-scgi-add-null-bytes
    "CONTENT_LENGTH" (number-to-string (length data))
    "SCGI" "1")))

(defun url-scgi-create-request ()
  (concat (url-scgi-make-request-header url-request-data)
          url-request-data))

(defun url-scgi-activate-callback ()
  "Activate callback specified when this buffer was created."
  (apply url-callback-function url-callback-arguments))

(defun url-scgi-handle-home-dir (filename)
  (expand-file-name
   (if (string-match "^/~" filename)
       (substring filename 1)
     filename)))

;;;###autoload
(defun url-scgi (url callback cbargs)
  "Handle SCGI URLs from internal Emacs functions."
  (check-type url vector "Need a pre-parsed URL.")
  (declare (special url-scgi-connection-opened
                    url-callback-function
                    url-callback-arguments
                    url-current-object))

  (let* ((host (url-host url))
         (port (url-port url))
         (filename (url-filename url))
         (is-local-socket (string-match "^/." filename))
         (bufname (format " *scgi %s*" (if is-local-socket
                                           filename
                                         (format "%s:%d" host port))))
         (buffer (generate-new-buffer bufname))
         (connection (cond
                      (is-local-socket
                       (let ((filename (url-scgi-handle-home-dir filename)))
                        (make-network-process :name "scgi"
                                              :buffer buffer
                                              :remote filename)))
                      (t ; scgi over tcp
                       (url-open-stream host buffer host port)))))
    (if (not connection)
        ;; Failed to open the connection for some reason
        (progn
          (kill-buffer buffer)
          (setq buffer nil)
          (error "Could not create connection to %s:%d" host port))
      (with-current-buffer buffer
        (setq url-current-object url
              mode-line-format "%b [%s]")

        (dolist (var '(url-scgi-connection-opened
                       url-callback-function
                       url-callback-arguments))
          (set (make-local-variable var) nil))

        (setq url-callback-function callback
              url-callback-arguments cbargs
              url-scgi-connection-opened nil)

        (pcase (process-status connection)
          (`connect
           ;; Asynchronous connection
           (set-process-sentinel connection 'url-scgi-async-sentinel))
          (`failed
           ;; Asynchronous connection failed
           (error "Could not create connection to %s:%d" host port))
          (_
           (setq url-scgi-connection-opened t)
           (process-send-string connection (url-scgi-create-request))))))
    buffer))

(defun url-scgi-async-sentinel (proc why)
  ;; We are performing an asynchronous connection, and a status change
  ;; has occurred.
  (with-current-buffer (process-buffer proc)
    (cond
     (url-scgi-connection-opened
      (url-scgi-activate-callback))
     ((string= (substring why 0 4) "open")
      (setq url-scgi-connection-opened t)
      (process-send-string proc (url-scgi-create-request)))
     (t
      (setf (car url-callback-arguments)
            (nconc (list :error (list 'error 'connection-failed why
                                      :host (url-host url-current-object)
                                      :service (url-port url-current-object)))
                   (car url-callback-arguments)))
      (url-scgi-activate-callback)))))

(provide 'url-scgi)

;;; url-scgi.el ends here
