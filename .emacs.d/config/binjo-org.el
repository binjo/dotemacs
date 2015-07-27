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

(require 'org)
(require 'org-protocol)
(ignore-errors (require 'org-contacts))

(defvar binjo-org-files
  '("works.org" "todo.org" "exploits.org" "remember.org" "archive.org" "things.org" "mapp.org"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c k") 'org-capture)

(setq org-log-done t)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'toggle-truncate-lines)

(setq org-directory "~/.emacs.d/org/")

(dolist (f binjo-org-files)
  (when (file-exists-p (concat org-directory f))
    (add-to-list 'org-agenda-files (concat org-directory f))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "LATER(l)" "|" "DONE(d!)" "CANCELLED(c!)")))

(setq org-return-follows-link t
      org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . t)))

(setq org-completion-use-ido t
      org-outline-path-complete-in-steps nil
      org-fast-tag-selection-single-key t
      org-tags-column -80)
(setq org-tag-alist '((:startgroup . nil)
                      ("work"      . ?w)
                      ("phish"     . ?p)
                      (:endgroup   . nil)
                      (:startgroup . nil)
                      ("0day"      . ?0)
                      ("Ms"        . ?M)
                      ("adobe"     . ?a)
                      ("security"  . ?s)
                      (:endgroup   . nil)
                      ("blog"      . ?b)
                      ("emacs"     . ?e)
                      ("life"      . ?l)
                      ("mail"      . ?m)
                      ("misc"      . ?i)
                      ("paper"     . ?P)
                      ("reading"   . ?r)
                      ("twitter"   . ?t)))

;; Agenda
(setq org-agenda-custom-commands
      '(("w" tags-todo "work")
        ("d" tags "adobe")
        ("r" tags "reading"))
      org-agenda-restore-windows-after-quit t)

;;; org archive
(setq org-archive-location "~/.emacs.d/org/archive.org::From %s")

(eval-after-load 'org
  '(defun org-dblock-write:image (params)
     (let ((file (plist-get params :file)))
       (clear-image-cache file)
       (insert-image (create-image file) ))))

;; org-capture
(setq org-capture-templates
      '(("c" "Things from clipboard" entry
         (file+headline "remember.org" "Interesting")
         "* %^{Description}\n  :TIMESTAMP: %T\n  %x"
         :empty-lines 1)
        ("t" "ToDo" entry
         (file+headline "todo.org" "Todo")
         "* TODO %^{Summary}\n  :TIMESTAMP: %T\n"
         :empty-lines 1)
        ("T" "ToDo Things" entry
         (file+headline "things.org" "Things")
         "* TODO %^{Summary}\n  :TIMESTAMP: %T\n"
         :empty-lines 1)
        ("e" "Exploit" entry
         (file+headline "exploits.org" "Exploits")
         "* %^{Description}\n  :TIMESTAMP: %T\n  %x"
         :empty-lines 1)
        ("C" "Contacts" entry
         (file+headline "contacts.org" "Contacts")
         "* %(org-contacts-template-name)\n  :PROPERTIES:\n  :EMAIL: %(org-contacts-template-email)\n  :END:")))

(when (boundp 'binjo-private-org-symc-template)
  (setq org-capture-templates
        (append org-capture-templates binjo-private-org-symc-template)))

(setq org-default-notes-file (concat org-directory "notes.org"))

;; org contacts
(setq org-contacts-files `(,(concat org-directory "contacts.org")))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(setq org-confirm-babel-evaluate nil)
(when (boundp 'binjo-private-org-symc-babel)
  (add-to-list 'org-structure-template-alist binjo-private-org-symc-babel))


(provide 'binjo-org)
;;; binjo-org.el ends here
