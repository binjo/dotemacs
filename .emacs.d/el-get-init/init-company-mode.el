;;; init-company-mode.el --- company

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



(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-show-numbers nil)
(setq company-tooltip-limit 30)

;; https://github.com/company-mode/company-mode/issues/75
(setq company-selection-wrap-around t)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
;;; init-company.el ends here
