;;; init-company.el --- company

;; Copyright 2014 Binjo
;;
;; Author: Binjo
;; Version: $Id: init-company.el,v 0.0 2014/07/15 05:58:12 binjo Exp $
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
;;   (require 'init-company)

;;; Code:

(eval-when-compile
  (require 'cl))



(setq company-auto-complete t)
(setq company-global-modes t)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 3)
(setq company-show-numbers t)
(setq company-tooltip-limit 30)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)

(add-hook 'after-init-hook 'global-company-mode)

;; (set-face-attribute 'company-tooltip nil :background "black" :foreground "white")
;; (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "DodgerBlue" :foreground "white")
;; (set-face-attribute 'company-preview nil :background "black")
;; (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "white")
;; (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :foreground "green")
;; (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection :foreground "white")
;; (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
;; (set-face-attribute 'company-scrollbar-fg nil :background "gray40")

(provide 'init-company)
;;; init-company.el ends here
