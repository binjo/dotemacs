;;; init-smartparens.el --- smartparens

;; Copyright 2014 binjo
;;
;; Author: binjo@binjo01.local
;; Version: $Id: init-smartparens.el,v 0.0 2014/07/16 13:23:16 binjo Exp $
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
;;   (require 'init-smartparens)

;;; Code:

(eval-when-compile
  (require 'cl))



(require 'smartparens)
(require 'smartparens-config)

(setq sp-autoskip-closing-pair 'always
      sp-show-pair-delay 0
      sp-show-pair-from-inside t)

(smartparens-global-mode t)
(show-smartparens-global-mode)

;; http://stackoverflow.com/a/2665369/152142
(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens)
(defun conditionally-enable-smartparens ()
  "enable smartparens-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(provide 'init-smartparens)
;;; init-smartparens.el ends here
