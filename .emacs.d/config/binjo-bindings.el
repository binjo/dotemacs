;;; binjo-bindings.el --- Key bindings

;; Copyright 2009 Binjo
;;
;; Author: Binjo <binjo.cn@gmail.com>
;; Version: $Id: binjo-bindings.el,v 0.0 2009/11/30 06:23:03 binjo Exp $
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

;; Provides key bindings.

;;; History:

;; 11/30/2009, init release

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-bindings)

;;; Code:

(eval-when-compile
  (require 'cl))


;; FN

;; viper-mode toggle-viper-mode
(global-set-key (kbd "<f12>") 'toggle-viper-mode)

(add-to-list 'insert-pair-alist '(?[?]))
(add-to-list 'insert-pair-alist '(?<?>))
(add-to-list 'insert-pair-alist '(?{?}))
(add-to-list 'insert-pair-alist '(?/?/))
(add-to-list 'insert-pair-alist '(?\'?\'))
(add-to-list 'insert-pair-alist '(?\"?\"))
(add-to-list 'insert-pair-alist '(?`?`))
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "M-<") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "C-{") 'backward-paragraph)
(global-set-key (kbd "C-/") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-`") 'insert-pair)

;; C-@ is not very comfortable...
(global-set-key (kbd "M-<SPC>") 'set-mark-command)

(global-set-key (kbd "M-4") 'kill-buffer);;yic-kill-current-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'other-window)
(global-set-key (kbd "C-x f") 'find-file-at-point)

(global-set-key (kbd "C-<tab>") 'ibuffer)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-%") 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;;;_ , view-mode
(setq view-mode-hook
      (lambda ()
        (define-key view-mode-map "h" 'backward-char)
        (define-key view-mode-map "l" 'forward-char)
        (define-key view-mode-map "j" 'next-line)
        (define-key view-mode-map "k" 'previous-line)))

;; browse url
(global-set-key (kbd "C-c u b") 'browse-url-at-point)

(global-set-key (kbd "C-c m v") 'visit-tags-table)
(global-set-key (kbd "C-c m e") '(lambda ()
                                   (interactive)
                                   (call-interactively 'eval-region)
                                   (message "eval-region...done")))

(global-set-key (kbd "C-c s r") '(lambda ()
                                   (interactive)
                                   (call-interactively 'sort-lines)
                                   (message "sort-lines...done")))

(global-set-key (kbd "C-c s h") 'eshell)
(global-set-key (kbd "C-c u n") 'untabify)
(global-set-key (kbd "C-c f l") 'flush-lines)
(global-set-key (kbd "C-c e b") 'erase-buffer)

(global-set-key (kbd "C-c r b") '(lambda (b e)
                                   (interactive "r")
                                   (base64-decode-region b e)))

(provide 'binjo-bindings)
;;; binjo-bindings.el ends here
