;;; binjo-path.el --- path related config

;; Copyright 2010 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-path.el,v 0.0 2010/02/05 10:20:12 binjo Exp $
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

;; 02/05/2010, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-path)

;;; Code:

(eval-when-compile
  (require 'cl))



(when (and (eq system-type 'windows-nt)
           (file-readable-p "~/.emacs.d/config/mypath"))
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/config/mypath")
    (goto-char (point-min))
    (while (not (eobp))
      (add-to-list 'exec-path
                   (buffer-substring
                    (line-beginning-position) (line-end-position)))
      (forward-line))))

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin/"))

(provide 'binjo-path)
;;; binjo-path.el ends here
