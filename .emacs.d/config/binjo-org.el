;;; binjo-org.el --- configs for org-mode

;; Copyright 2009 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-org.el,v 0.0 2009/04/09 08:04:57 binjo Exp $
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

;; Configs for org-mode

;;; History:

;; 04/09/2009, init releease

;; 12/02/2009, refactoring layout, copy some elisp from xwl's xwl-org.el

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-org)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org-install)
(require 'org-google-weather)

(defvar binjo-org-files
  '("works.org" "todo.org" "exploits.org" "remember.org" "archive.org"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'toggle-truncate-lines)

(setq org-directory "~/.emacs.d/org/")

(setq org-agenda-files (mapcar
                        (lambda (f)
                          (concat org-directory f))
                        binjo-org-files))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "LATER(l)" "|" "DONE(d!)" "CANCELLED(c!)")))

(setq org-return-follows-link t
      org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . t)))

(setq org-completion-use-ido t
      org-outline-path-complete-in-steps nil
      org-fast-tag-selection-single-key t)
(setq org-tag-alist '((:startgroup . nil)
                      ("work"      . ?w)
                      ("phish"     . ?p)
                      (:endgroup   . nil)
                      (:startgroup . nil)
                      ("0day"      . ?0)
                      ("M$"        . ?M)
                      ("adobe"     . ?a)
                      ("security"  . ?s)
                      (:endgroup   . nil)
                      ("blog"      . ?b)
                      ("emacs"     . ?e)
                      ("life"      . ?l)
                      ("mail"      . ?m)
                      ("misc"      . ?i)
                      ("paper"     . ?p)
                      ("reading"   . ?r)
                      ("twitter"   . ?t)))

(setq org-agenda-custom-commands
      '(("w" tags-todo "work")
        ("d" tags "adobe")
        ("r" tags "reading")))

;;; org archive
(setq org-archive-location "~/.emacs.d/org/archive.org::From %s")

(eval-after-load 'org
  '(defun org-dblock-write:image (params)
     (let ((file (plist-get params :file)))
       (clear-image-cache file)
       (insert-image (create-image file) ))))

;;; org-remember
(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "notes.org"))
(global-set-key (kbd "C-c r") 'org-remember)
(setq org-remember-templates
  '(("Clipboard" ?c "* %T %^{Description}\n %x"
     "remember.org" "Interesting")
    ("ToDo" ?t "* TODO %T %^{Summary}"
     "todo.org" "Todo")
    ("Exploit" ?e "* %T %^{Description}\n %x"
     "exploits.org" "Exploits")))

;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode/
(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
    (delete-frame)))

(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-remember
;; splits the window.
(add-hook 'remember-mode-hook 'delete-other-windows)

(defun make-remember-frame ()
  "Create a new frame and run org-remember"
  (interactive)
  (make-frame '((name . "*Remember*") (width . 80) (height . 10)))
  (select-frame-by-name "*Remember*")
  (org-remember))

;; org-google-weather
(setq org-google-weather-icon-directory
      "~/w32/GNOME_Weather_Icons_by_DarKobra/48x48/status")

(provide 'binjo-org)
;;; binjo-org.el ends here
