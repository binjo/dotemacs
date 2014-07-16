;;; init-ioccur.el --- ioccur

;; Copyright 2014 Binjo
;;
;; Author: Binjo
;; Version: $Id: init-ioccur.el,v 0.0 2014/07/15 07:12:07 binjo Exp $
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
;;   (require 'init-ioccur)

;;; Code:

(eval-when-compile
  (require 'cl))



(set-face-background 'ioccur-match-overlay-face "SkyBlue")
(set-face-background 'ioccur-title-face "MediumOrchid")

(define-key ioccur-mode-map (kbd "j") 'ioccur-scroll-down)
(define-key ioccur-mode-map (kbd "k") 'ioccur-scroll-up)

(global-set-key (kbd "C-c o c") 'ioccur)

;; (add-to-list 'desktop-globals-to-save 'ioccur-history)

(provide 'init-ioccur)
;;; init-ioccur.el ends here
