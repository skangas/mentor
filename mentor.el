;;; mentor.el --- My Emacs kNows TORrents!  Control rtorrent from emacs

;; Copyright (C) 2010, Stefan Kangas

;; Maintainer: Stefan Kangas
;; Keywords: bittorrent, rtorrent

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Change Log:

;;; Code:

;; ((("d.get_base_filename" . "Sjowall-Wahloo_-_Roseanna-1965-6CD-AUDiOBOOK-SE-2005-NOAHWYLE")
;;   ("d.get_base_path" . "/home/skangas/download/Sjowall-Wahloo_-_Roseanna-1965-6CD-AUDiOBOOK-SE-2005-NOAHWYLE")
;;   ("d.get_bytes_done" . 383080136.0)
;;   ("d.get_chunk_size" . 524288)
;;   ("d.get_chunks_hashed" . 731)
;;   ("d.get_complete" . 1)
;;   ("d.get_completed_bytes" . 383080136.0)
;;   ("d.get_completed_chunks" . 731)
;;   ("d.get_connection_current" . "seed")
;;   ("d.get_connection_leech" . "leech")
;;   ("d.get_connection_seed" . "seed")
;;   ("d.get_creation_date" . 1233754363.0)
;;   ("d.get_directory" . "/home/skangas/download/Sjowall-Wahloo_-_Roseanna-1965-6CD-AUDiOBOOK-SE-2005-NOAHWYLE")
;;   ("d.get_directory_base" . "/home/skangas/download/Sjowall-Wahloo_-_Roseanna-1965-6CD-AUDiOBOOK-SE-2005-NOAHWYLE")
;;   ("d.get_down_rate" . 0)
;;   ("d.get_down_total" . 0)
;;   ("d.get_free_diskspace" . 1242800128.0)
;;   ("d.get_hash" . "6AF265D0747D883059846B2FE6A0F6257EF7EC76")
;;   ("d.get_hashing" . 0)
;;   ("d.get_hashing_failed" . 0)
;;   ("d.get_ignore_commands" . 0)
;;   ("d.get_left_bytes" . 0)
;;   ("d.get_loaded_file" . "/home/skangas/.rtorrent-session/6AF265D0747D883059846B2FE6A0F6257EF7EC76.torrent")
;;   ("d.get_local_id" . "2D6C74304336302D0B1BE2019432CE1E6201ECA2")
;;   ("d.get_local_id_html" . "-lt0C60-%0B%1B%E2%01%942%CE%1Eb%01%EC%A2")
;;   ("d.get_max_file_size" . 0)
;;   ("d.get_max_size_pex" . 8)
;;   ("d.get_message")
;;   ("d.get_name" . "Sjowall-Wahloo_-_Roseanna-1965-6CD-AUDiOBOOK-SE-2005-NOAHWYLE")
;;   ("d.get_peer_exchange" . 0)
;;   ("d.get_peers_accounted" . 0)
;;   ("d.get_peers_complete" . 0)
;;   ("d.get_peers_connected" . 0)
;;   ("d.get_peers_max" . 100)
;;   ("d.get_peers_min" . 40)
;;   ("d.get_peers_not_connected" . 0)
;;   ("d.get_priority" . 2)
;;   ("d.get_priority_str" . "normal")
;;   ("d.get_ratio" . 5237)
;;   ("d.get_size_bytes" . 383080136.0)
;;   ("d.get_size_chunks" . 731)
;;   ("d.get_size_files" . 91)
;;   ("d.get_size_pex" . 0)
;;   ("d.get_skip_rate" . 0)
;;   ("d.get_skip_total" . 0)
;;   ("d.get_state" . 1)
;;   ("d.get_state_changed" . 1264866318.0)
;;   ("d.get_state_counter" . 14)
;;   ("d.get_throttle_name")
;;   ("d.get_tied_to_file" . "~/download/torrents/Sjowall-Wahloo_-_Roseanna-1965-6CD-AUDiOBOOK-SE-2005-NOAHWYLE.torrent")
;;   ("d.get_tracker_focus" . 1)
;;   ("d.get_tracker_numwant" . -1)
;;   ("d.get_tracker_size" . 1)
;;   ("d.get_up_rate" . 0)
;;   ("d.get_up_total" . 2006442368.0)
;;   ("d.get_uploads_max" . 50)
;;   ("d.is_active" . 1)
;;   ("d.is_hash_checked" . 1)
;;   ("d.is_hash_checking" . 0)
;;   ("d.is_multi_file" . 1)
;;   ("d.is_open" . 1)
;;   ("d.is_pex_active" . 0)
;;   ("d.is_private" . 1))


(require 'xml-rpc)

(defgroup mentor nil
  "Controlling rtorrent from Emacs."
  :prefix "mentor-"
  :group 'tools)

(defcustom mentor-scgi-url "scgi://localhost:5000"
  "The scgi URL, as specified in rtorrent config var scgi_port"
  :group 'mentor
  :type 'string)


(defvar mentor-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "g") 'mentor-update)
    (define-key map (kbd "TAB") 'mentor-toggle-object)
    ;; (define-key map (kbd "d") 'mentor-stop-torrent)
    ;; (define-key map (kbd "k") 'mentor-kill-torrent)
    ;; (define-key map (kbd "n") 'mentor-next)
    ;; (define-key map (kbd "p") 'mentor-prev)
    ;; (define-key map (kbd "s") 'mentor-sort)
    ;; (define-key map (kbd "Q") 'mentor-shutdown-rtorrent)
    map))

(defun mentor-mode ()
  "Major mode for controlling rtorrent from emacs"
  (kill-all-local-variables)
  (setq major-mode 'rtorrent-control-mode
        mode-name "rtorrent"
        truncate-lines t)
  (use-local-map mentor-mode-map)
  (run-mode-hooks 'mentor-mode-hook))

(defun mentor-init ()
  (interactive)
  (if (mentor-not-connectable-p)
      (message "Unable to connect")
    (progn (switch-to-buffer (get-buffer-create "*mentor*"))
           (mentor-mode)
           (mentor-update))))

(defun mentor-not-connectable-p ()
  ;; TODO
  nil)


(defun mentor-kill-torrent ()
  (message "TODO"))


(defun mentor-command (&rest args)
  "Run command as an XML-RPC call via SCGI."
  ;; (when (not (listp args))
  ;;   (setq args (list args)))
  (xml-rpc-xml-to-response
   (with-temp-buffer
     (apply 'call-process
            "/home/skangas/.emacs.d/lisp-personal/xmlrpc2scgi.py"
            nil t nil (apply 'append `(,mentor-scgi-url) args))
     ;; (xml-rpc-value-to-xml-list
     (xml-rpc-request-process-buffer (current-buffer)))))

(defun mentor-command-multi (&rest args)
  (mentor-command (append '("d.multicall" "default") args)))

;; Needed to work around buggy expressions in rtorrent
(defvar mentor-method-exclusions-regexp "d\\.get_\\(mode\\|custom.*\\|bitfield\\)")

(defun mentor-rpc-system-listmethods (&optional regexp)
  "system.listMethods \
Returns a list of all available commands.  First argument is \
interpreted as a regexp, and if specified only returns matching \
functions"
  (let ((methods (mentor-command '("system.listMethods")))
        (retval '()))
    (when regexp
      (mapc (lambda (cur)
              (when (and (string-match regexp cur)
                         (not (string-match mentor-method-exclusions-regexp cur)))
                (setq retval (cons cur retval))))
            methods))
    retval))


(defun mentor-update ()
  (interactive)
  (mentor-update-torrent-list)
  (erase-buffer)
  (mentor-insert-torrents))

(defun mentor-insert-torrents ()
  (let ((torrents mentor-torrent-list))
    (while torrents
      (mentor-insert-torrent (car torrents))
      (setq torrents (cdr torrents)))))

(defun mentor-insert-torrent (torrent)
  ;; TODO allow for different formats
  (insert
   (propertize (concat (mentor-get-field "d.get_name" torrent)  "\n")
               'torrent-id (mentor-get-field "d.local_id" torrent)
               'collapsed t)))

(defun mentor-get-field (field torrent)
  (cdr (assoc field torrent)))

 ;; (let ((buf (get-buffer-create "*mentor-process*")))
 ;;    (save-excursion
 ;;      (set-buffer buf)
 ;;      (setq buffer-read-only t)
 ;;      (let ((inhibit-read-only t))
 ;;        (erase-buffer)
 ;;        (message cmd)
 ;;        (



;; (defun mentor-toggle-object ()
;;   (get-text-property 'torrent-id

(defun mentor-get-torrent-list ()
  (let* ((methods (mentor-rpc-system-listmethods "^d\\.\\(get\\|is\\)"))
         (tor-list (mentor-command-multi (mapcar (lambda (x) (concat x "="))
                                                   methods)))
         (attributes (mapcar (lambda (name)
                               (replace-regexp-in-string "^d\\.\\(get_\\)" "" name))
                             methods)))
    (mapcar (lambda (torrent)
              (mentor-join-lists attributes torrent))
            tor-list)))

(defvar mentor-torrent-list nil)
(make-variable-buffer-local 'mentor-torrent-list)

(defun mentor-update-torrent-list ()
  (setq mentor-torrent-list (mentor-get-torrent-list)))

(defun mentor-torrent-at-point ()
  (get-text-property 'torrent-id))


;;; Utility functions

(defun mentor-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
	(substring str 0 (- (length str) 1))
      str)))

(defun mentor-join-lists (list1 list2)
  (let ((result '()))
    (while list1
      (setq result (cons `(,(car list1) . ,(car list2)) result))
      (setq list1 (cdr list1))
      (setq list2 (cdr list2)))
    result))
(provide 'mentor)


;;; mentor.el ends here

