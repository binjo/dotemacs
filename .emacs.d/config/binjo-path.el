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



(setq load-path
      (append (list "~/.emacs.d/site-lisp"
                    "~/.emacs.d/config"
                    "~/.emacs.d/site-lisp/wubi"
                    "~/.emacs.d/site-lisp/template/lisp"
                    "~/.emacs.d/site-lisp/ruby-mode"
                    "~/.emacs.d/site-lisp/ejacs"
                    "~/.emacs.d/site-lisp/bbdb-2.35/lisp"
                    "~/.emacs.d/site-lisp/smex"
                    "~/.emacs.d/site-lisp/yasnippet"
                    "~/.emacs.d/site-lisp/emms/lisp"
                    "~/.emacs.d/site-lisp/douban"
                    "~/.emacs.d/site-lisp/emacs_chrome/servers"
                    "~/.emacs.d/site-lisp/twit"
                    "~/.emacs.d/site-lisp/twittering-mode"
                    "~/.emacs.d/site-lisp/magit"
                    "~/.emacs.d/site-lisp/magithub"
                    "~/.emacs.d/site-lisp/org/lisp"
                    "~/.emacs.d/site-lisp/ioccur"
                    "~/.emacs.d/site-lisp/undo-tree"
                    "~/.emacs.d/site-lisp/emacs-jabber"
                    "~/.emacs.d/site-lisp/google-weather-el"
                    "~/.emacs.d/site-lisp/php-mode"
                    )
              load-path))

(when (file-readable-p "~/.emacs.d/config/mypath")
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/config/mypath")
    (goto-char (point-min))
    (while (not (eobp))
      (setq exec-path (append
                       (list (buffer-substring
                              (line-beginning-position) (line-end-position)))
                       exec-path))
      (forward-line))))

(provide 'binjo-path)
;;; binjo-path.el ends here
