;;; binjo-hydra.el --- hydra bindings

;; Copyright 2015 Binjo
;;
;; Author: binjo
;; Version: $Id: binjo-hydra.el,v 0.0 2015/07/27 02:28:48 binjo Exp $
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
;;   (require 'binjo-hydra)

;;; Code:

(eval-when-compile
  (require 'cl))


(require 'hydra)

(defhydra binjo-hydra-top (:color blue :hint nil :columns 3)
  "
Capture^^    ^Agenda^           ^Misc^
---------------------------------------
_t_ todo     _a_ Agenda List    _m_ magit
_r_ remmeber _A_ Agenda         _p_ pt
^^           _s_ search"
  ("t" (org-capture nil "t"))
  ("r" (org-capture nil "c"))
  ("a" org-agenda-list)
  ("A" org-agenda)
  ("s" org-search-view)
  ("m" magit-status)
  ("p" pt-regexp)
  ("q" nil "cancel"))

(global-set-key (kbd "C-.") 'binjo-hydra-top/body)

(provide 'binjo-hydra)
;;; binjo-hydra.el ends here
