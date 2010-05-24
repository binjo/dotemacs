;;; binjo-gnus-notify.el --- Show groups with new mail on the modeline

;; Copyright 2010 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-gnus-notify.el,v 0.0 2010/05/21 10:35:37 binjo Exp $
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

;; Show groups with new mail on the modeline, should be customizable.

;;; History:

;; 2010/05/21, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-gnus-notify)

;;; Code:

(eval-when-compile
  (require 'cl))



(defvar binjo-gnus-unread-group-info nil
  "A list of (group number).")

(defvar binjo-gnus-notify-groups '()
  "A list of groups name.")

(defun binjo-gnus-notify-string ()
  (let ((sum (apply '+ (mapcar 'cdr binjo-gnus-unread-group-info))))
    (if (= 0 sum)
        ""
      (format " M(%d)" sum))))

(defun gnus-mst-notify-shorten-group-name (group)
  "shorten the group name to make it better fit on the modeline"
  (let ((name (if (string-match ":" group)
                  (cadr (split-string group "[:]"))
                group)))
    (mapconcat 'identity
               (mapcar
                #'(lambda (segment)
                    (string (elt segment 0)))
                (split-string name "[\\./]"))
               ".")))

(defun binjo-gnus-update-unread-group-info ()
  "Update unreaded group info."
  (setq binjo-gnus-unread-group-info '())
  (mapc (lambda (g)
          (let* ((gname (gnus-mst-notify-shorten-group-name g))
                 (unread (gnus-group-unread g)))
            (when (and (numberp unread) (> unread 0))
              (add-to-list 'binjo-gnus-unread-group-info `(,gname . ,unread)))))
        binjo-gnus-notify-groups)
  (force-mode-line-update))

(defun binjo-gnus-enable-unread-notify ()
  "Enable gnus unread notify."
  (interactive)
  (add-hook 'gnus-after-getting-new-news-hook
            'binjo-gnus-update-unread-group-info t)
  (add-hook 'gnus-summary-exit-hook
            'binjo-gnus-update-unread-group-info t)
  (add-hook 'gnus-topic-mode-hook
            'binjo-gnus-update-unread-group-info t)
  (add-to-list 'global-mode-string
               '(:eval (binjo-gnus-notify-string)) t))

(defun binjo-gnus-disable-unread-notify ()
  "Disable gnus unread notify."
  (interactive)
  (remove-hook 'gnus-after-getting-new-news-hook
               'binjo-gnus-update-unread-group-info)
  (remove-hook 'gnus-summary-exit-hook
               'binjo-gnus-update-unread-group-info)
  (remove-hook 'gnus-topic-mode-hook
               'binjo-gnus-update-unread-group-info)
  (setq global-mode-string
        (remove '(:eval (binjo-gnus-notify-string))
                global-mode-string)))

(provide 'binjo-gnus-notify)
;;; binjo-gnus-notify.el ends here
