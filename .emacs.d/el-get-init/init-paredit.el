;;; init-paredit.el --- paredit

;; Copyright 2014 Binjo
;;
;; Author: Binjo
;; Version: $Id: init-paredit.el,v 0.0 2014/07/15 06:04:40 binjo Exp $
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

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-paredit)

;;; Code:

(eval-when-compile
  (require 'cl))



(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(eval-after-load "paredit"
  '(progn
    (define-key paredit-mode-map (kbd "M-<up>"  ) nil)
    (define-key paredit-mode-map (kbd "M-<down>") nil)
    ))

(provide 'init-paredit)
;;; init-paredit.el ends here
