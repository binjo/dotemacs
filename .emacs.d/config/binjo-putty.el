;;; binjo-putty.el --- session management

;; Copyright 2011
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-putty.el,v 0.0 2011/04/08 17:47:53 joe Exp $
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

;;

;;; History:

;; 2011/04/08, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-putty)

;;; Code:

(eval-when-compile
  (require 'cl))


(defvar binjo-putty-sessions-list '()
  "A list of putty sessions")

(defun binjo-putty-sessions-sentinel (proc event)
  "Sentinel for killing buffer when finished."
  (when (eq "finished" event)
    (kill-buffer (process-buffer proc))))

(defun binjo-putty-sessions-filter (proc string)
  "Get output of proc."
  (dolist (s (split-string string "\n" t))
    (add-to-list 'binjo-putty-sessions-list s)))

(defun binjo-putty-sessions ()
  "Get putty sessions from registry."
  (let* ((binjo-putty-sessions-list '())
         (py-proc
          (start-process "*py-putty*" "*py-putty*" "python"
                         (expand-file-name "~/w32/get_putty_sessions.py"))))
    (set-process-filter py-proc 'binjo-putty-sessions-filter)
    (set-process-sentinel py-proc 'binjo-putty-sessions-sentinel)
    ;; FIXME awkward fix for waiting list being filled
    (while (null binjo-putty-sessions-list)
      (sit-for 0.1))
    binjo-putty-sessions-list))

(defun binjo-start-putty ()
  "Start putty.exe with proper session name."
  (interactive)
  (let* ((s (ido-completing-read "Session "
                                 (binjo-putty-sessions))))
    (w32-shell-execute "open" "putty" (concat "-load " s))))

(provide 'binjo-putty)
;;; binjo-putty.el ends here
