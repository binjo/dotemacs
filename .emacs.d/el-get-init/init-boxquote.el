;;; init-boxquote.el --- boxquote

;; Copyright 2014 binjo
;;
;; Author: binjo@binjo01.local
;; Version: $Id: init-boxquote.el,v 0.0 2014/07/16 13:33:54 binjo Exp $
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
;;   (require 'init-boxquote)

;;; Code:

(eval-when-compile
  (require 'cl))



(global-set-key (kbd "C-c b r") 'boxquote-region)
(global-set-key (kbd "C-c b t") 'boxquote-title)
(global-set-key (kbd "C-c b f") 'boxquote-describe-function)
(global-set-key (kbd "C-c b v") 'boxquote-describe-variable)
(global-set-key (kbd "C-c b k") 'boxquote-describe-key)
(global-set-key (kbd "C-c b !") 'boxquote-shell-command)
(global-set-key (kbd "C-c b y") 'boxquote-yank)

(provide 'init-boxquote)
;;; init-boxquote.el ends here
