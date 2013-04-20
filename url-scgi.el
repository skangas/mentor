;;; url-scgi.el --- SCGI Uniform Resource Locator retrieval code

;; Copyright (C) 2011 Stefan Kangas.

;; Author: Stefan Kangas
;; Version: 0.2
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

;; 0.2 Support Emacs 24

;; 0.1 First public version

;;; Code:

(eval-when-compile (require 'cl))

(defvar url-scgi-content-length)
(defvar url-scgi-content-type)
(defvar url-scgi-connection-opened)

(defconst url-scgi-asynchronous-p t "SCGI retrievals are asynchronous.")

(defun scgi-string-to-netstring (str)
  "Converts a string into a netstring as defined by the SCGI
specification."
  (let ((len (length str)))
    (concat (number-to-string len)
            ":" str ",")))

(defun scgi-add-null-bytes (&rest args)
  (apply 'concat (mapcar (lambda (a) (concat a "\000")) args)))

(defun scgi-make-request-header (data)
  (scgi-add-null-bytes "CONTENT_LENGTH"
                       (number-to-string (length data))
                       "SCGI" "1"))

(defun url-scgi-create-request ()
  (declare (special url-request-data))
  (concat
   (scgi-string-to-netstring
    (scgi-make-request-header url-request-data))
   url-request-data))

(defun url-scgi-activate-callback ()
  "Activate callback specified when this buffer was created."
  (declare (special url-callback-function
		    url-callback-arguments))
  (apply url-callback-function url-callback-arguments))

(defun url-scgi-parse-headers ()
  (declare (special url-scgi-content-length
                    url-scgi-content-type))
  (save-restriction
    (save-match-data
      (mail-narrow-to-head)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (let ((status (mail-fetch-field "status"))
            (content-length (mail-fetch-field "content-length"))
            (content-type (mail-fetch-field "content-type")))
        (when content-length
          (setq url-scgi-content-length content-length))
        (when content-type
          (setq url-scgi-content-length content-type))
        (when (and status (not (equal status "200 OK")))
          (error (message (concat "Got status response: " status)))))))
  t)

(defun url-scgi-get-connection (host port)
  (let ((buf (generate-new-buffer " *url-scgi-temp*")))
    ;; `url-open-stream' needs a buffer in which to do things
    ;; like authentication.  But we use another buffer afterwards.
    (unwind-protect
        (let ((proc (url-open-stream host buf host port)))
          ;; url-open-stream might return nil.
          (when (processp proc)
            ;; Drop the temp buffer link before killing the buffer.
            (set-process-buffer proc nil))
          proc)
      (kill-buffer buf))))

;;;###autoload
(defun url-scgi (url callback cbargs)
  "Handle SCGI URLs from internal Emacs functions."
  (check-type url vector "Need a pre-parsed URL.")
  (declare (special url-scgi-connection-opened
		    url-callback-function
		    url-callback-arguments))
  (let* ((host (url-host url))
	 (port (url-port url))
	 (connection (url-scgi-get-connection host port))
	 (buffer (generate-new-buffer (format " *scgi %s:%d*" host port))))
    (if (not connection)
	;; Failed to open the connection for some reason
	(progn
	  (kill-buffer buffer)
	  (setq buffer nil)
	  (error "Could not create connection to %s:%d" host port))
      (with-current-buffer buffer
	(mm-disable-multibyte)
	(setq url-current-object url
	      mode-line-format "%b [%s]")

	(dolist (var '(url-scgi-connection-opened
                       url-scgi-content-type
		       url-scgi-content-length
		       url-callback-function
		       url-callback-arguments))
	  (set (make-local-variable var) nil))

	(setq url-callback-function callback
	      url-callback-arguments cbargs
	      url-scgi-connection-opened nil)

	(set-process-buffer connection buffer)
	(set-process-filter connection 'url-scgi-filter)
        (set-process-sentinel connection 'url-scgi-async-sentinel)
	(let ((status (process-status connection)))
          (cond ((eq status 'failed)
                 (error "Could not create connection to %s:%d" host port))  
                ((eq status 'open)
                 (process-send-string connection (url-scgi-create-request))
                 (setq url-scgi-connection-opened t))))))
    buffer))

(defun url-scgi-async-sentinel (proc why)
  (declare (special url-callback-arguments))
  (with-current-buffer (process-buffer proc)
    (cond
     (url-scgi-connection-opened
      (if (url-scgi-parse-headers)
          (url-scgi-activate-callback)))
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

(defun url-scgi-filter (proc data)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert data))))

(provide 'url-scgi)

;;; url-scgi.el ends here
