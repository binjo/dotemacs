;;; binjo-main.el --- main for my config file

;; Copyright 2010 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-main.el,v 0.0 2010/02/04 15:48:44 binjo Exp $
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

;; 02/04/2010, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-main)

;;; Code:

(eval-when-compile
  (require 'cl))



;; path and private thing first.
(load "~/.emacs.d/config/binjo-path.el")
(load "~/.emacs.d/config/binjo-private.el")

(setq custom-file "~/.emacs.d/config/binjo-custom.el")

;; load config files
(load "~/.emacs.d/config/binjo-util.el")
(load "~/.emacs.d/config/binjo-custom.el")
(load "~/.emacs.d/config/binjo-program.el")

;; awkward hack...
(eval-after-load "~/.emacs.d/config/binjo-custom.el"
  (loop for x downfrom 40 to 1 do
        (setq tab-stop-list (cons (* x 4) tab-stop-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
    (progn
      (defun w32-minimize-frame ()
        "Minimized the current frame"
        (interactive)
        (w32-send-sys-command #xf020))
      (defun w32-restore-frame ()
        "Restore a minimized frame"
        (interactive)
        (w32-send-sys-command #xf120))
      (defun w32-maximize-frame ()
        "Maximize the current frame"
        (interactive)
        (w32-send-sys-command 61488))
      ;; Maximum Windows Frame
      (add-hook 'window-setup-hook 'w32-maximize-frame t)))

(add-hook 'window-setup-hook 'server-start t)

(put 'erase-buffer 'disabled nil)

(provide 'binjo-main)
;;; binjo-main.el ends here
