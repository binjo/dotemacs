;;; init-el-get.el --- el-get

;; Copyright 2010, 2014 Binjo
;;
;; Author: binjo.cn@gmail.com
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

;; 15/07/2014, init

;;; Code:

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Init files
(setq el-get-user-package-directory "~/.emacs.d/el-get-init")

;; El-get from master branch
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Own recipes
(add-to-list 'el-get-recipe-path "~/.emacs.d/config/recipes")

(setq binjo:el-get-packages
      '(el-get
        slothful
        pymacs
        company-mode
        ;; browse-kill-ring
        template
        session
        etags-select
        undo-tree
        js2-mode
        smex
        smartparens
        web-mode
        magit
        ioccur
        boxquote
        cal-china-x
        hydra
        ))

(el-get 'sync binjo:el-get-packages)

(provide 'init-el-get)
