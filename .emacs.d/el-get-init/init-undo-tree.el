;;; init-undo-tree.el --- undo-tree

;; Copyright 2014 binjo
;;
;; Author: binjo@binjo01.local
;; Version: $Id: init-undo-tree.el,v 0.0 2014/07/16 13:10:01 binjo Exp $
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
;;   (require 'init-undo-tree)

;;; Code:

(eval-when-compile
  (require 'cl))



(add-hook 'window-setup-hook '(lambda ()
                                (global-undo-tree-mode)))

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
