;;; binjo-eshell-util.el --- utils used in eshell

;; Copyright 2009 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-eshell-util.el,v 0.0 2009/10/29 02:43:07 binjo Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; utils for eshell

;;; History:

;; 29/10/2009, init release.
;; 07/11/2009, add macro m-w32-shell-execute, and cmds call it.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-eshell-util)

;;; Code:

(provide 'binjo-eshell-util)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defconst program-hiew
  (executable-find "hiew32.exe"))

(defconst program-ida
  (executable-find "idag.exe"))

(defconst program-bintext
  (executable-find "BinText.exe"))

(defconst program-ollydbg
  (executable-find "OLLYDBG.EXE"))

(defconst program-peid
  (executable-find "PEiD.exe"))

(defconst program-stud-pe
  (executable-find "Stud_PE_hh.exe"))

(defun eshell/cls()
  "Clearing EShell Buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/url (lru)
  "Decrypt LRU(url) from %u7468%u7074%u2f3a to 'http:/'."
  (interactive)
  (let ((a_url (split-string lru "%u"))
        (a ""))
    (mapc (lambda (x)
              (unless (string= x "")
                (unless (string= (substring x 2 4) "00")
                  (setq a (concat a (format "%c" (string-to-number (substring x 2 4) 16)))))
                (unless (string= (substring x 0 2) "00")
                  (setq a (concat a (format "%c" (string-to-number (substring x 0 2) 16)))))))
          a_url)
    a))

(defun eshell/unhex (hex-string)
  "Convert hexlified HEX-STRING to normal."
  (interactive)
  (let ((a "")
        (i 0)
        (len (length hex-string)))
    (while (< i len)
      (setq a (concat a (format "%c" (string-to-number (substring hex-string i (+ i 2)) 16))))
      (setq i (+ i 2)))
    a))

(defun eshell/hex (string)
  "Convert STRING to hex, a.k.a hexlify."
  (interactive)
  (let ((hexlified "")
        (a_s (string-to-list string)))
    (mapc (lambda (x)
            (setq hexlified (concat hexlified
                                    (format "%02x" x))))
          a_s)
    hexlified))

(defun eshell/h2b (file-name hex)
  "Convert encoded HEX to binary, save as FILE-NAME."
  (interactive)
  (let ((a_h (split-string hex "%u"))
        (coding-system-for-write 'raw-text-unix))
    (with-temp-file file-name
      (mapc (lambda (x)
              (unless (string= x "")
                (insert-byte (string-to-number (substring x 2 4) 16) 1)
                (insert-byte (string-to-number (substring x 0 2) 16) 1)))
            a_h))
    "[*] Done...Check it out..."))

(defun eshell/td ()
  "Change current directory to `today', create it if does not exist."
  (interactive)
  (let ((cur_day (format-time-string "%m%d"))
        (main_dir "d:/xxx/2010/"))
    (setq target_dir (concat main_dir cur_day))
    (condition-case nil
        (make-directory target_dir)
      (error nil))
    (cd target_dir)))

(defmacro m-w32-shell-execute (program arg)
  "Execute `w32-shell-execute' to call PROGRAM with ARG."
  `(interactive)
  `(w32-shell-execute "open" ,program (shell-quote-argument ,arg) 1))

(defun eshell/.hiew (file-name)
  "Start HIEW with parameter set as FILE-NAME."
  (interactive)
  (m-w32-shell-execute program-hiew file-name)
  "")

(defun eshell/.ida (file-name)
  "Start IDA with parameter set as FILE-NAME."
  (m-w32-shell-execute program-ida  file-name)
  "")

(defun eshell/.bintext (file-name)
  "Start BINTEXT with parameter set as FILE-NAME."
  (m-w32-shell-execute program-bintext file-name)
  "")

(defun eshell/.od (file-name)
  "Start OLLYDBG with parameter set as FILE-NAME."
  (m-w32-shell-execute program-ollydbg file-name)
  "")

(defun eshell/.peid (file-name)
  "Start PEID with parameter set as FILE-NAME."
  (m-w32-shell-execute program-peid file-name)
  "")

(defun eshell/.studpe (file-name)
  "Start STUD_PE with parameter set as FILE-NAME."
  (m-w32-shell-execute program-stud-pe file-name)
  "")

(defun eshell/url-hex-filter (string)
  "Remove %XX embedded spaces, etc in a URL."
  (if (string-match "unescape" string)
      (url-unhex-string string)
    (if (string-match "write(" string)
        (replace-in-string string "\\\\" "")
      string)))

(add-hook 'eshell-preoutput-filter-functions
          'eshell/url-hex-filter)
;;; binjo-eshell-util.el ends here
