;;; binjo-magit.el --- settings for magit

;; Copyright 2010 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-magit.el,v 0.0 2010/08/23 11:20:18 binjo Exp $
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

;; 23/08/2010, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-magit)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'magit)
(require 'magit-svn)
;; (require 'magithub)


(eval-after-load 'magit
  '(progn
     (setq magit-auto-revert-mode nil)
     (setq magit-last-seen-setup-instructions "1.4.0")
     (setq magit-repo-dirs '("~/.emacs.d/"
                             "d:/repos"))))

(defun magit-clone (url repo dir &optional svnp)
  "Clone remote URL repo into directory DIR.
If SVNP is non-nil, clone it as svn repo."
  (interactive
   (list (read-string "Remote URL: ")
         (read-string "Repo's Name: ")
         (read-directory-name "Parent directory: ")
         current-prefix-arg))
  (let* ((dir (concat (directory-file-name (expand-file-name dir)) "/" repo "/"))
         (magit-args (if svnp
                         `("svn" "clone" ,url ,dir)
                       `("clone" ,url ,dir))))
    (apply 'magit-run-git magit-args)
    (magit-status dir)))

(provide 'binjo-magit)
;;; binjo-magit.el ends here
