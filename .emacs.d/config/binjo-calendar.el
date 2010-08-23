;;; binjo-calendar.el --- calendar related settings

;; Copyright 2010 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-calendar.el,v 0.0 2010/07/31 17:31:49 binjo Exp $
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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-calendar)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'cal-china-x)

(setq mark-holidays-in-calendar t
      view-calendar-holidays-initially t
      cal-china-x-important-holidays
      `(,@cal-china-x-chinese-holidays
        ,@(mapcar '(lambda (x)
                     (let ((lunar-or-fixed (car x)))
                       (if (= 1 lunar-or-fixed)
                           `(holiday-lunar ,@(cdr x))
                         `(holiday-fixed ,@(cdr x)))))
                  binjo-calendar-important-days))
      )

(setq calendar-holidays
      cal-china-x-important-holidays)

(when (file-readable-p "~/.diary")
  (setq diary-file "~/.diary"
        mark-diary-entries-in-calendar t))

(defun xwl-display-diary ()
  (when (eq (face-at-point) 'diary)
    (save-window-excursion
      (message (nth 1 (car (diary-view-entries)))))))

(add-hook 'calendar-move-hook 'xwl-display-diary)

(define-key calendar-mode-map (kbd "j") 'calendar-forward-week)
(define-key calendar-mode-map (kbd "k") 'calendar-backward-week)
(define-key calendar-mode-map (kbd "l") 'calendar-forward-day)
(define-key calendar-mode-map (kbd "h") 'calendar-backward-day)

(provide 'binjo-calendar)
;;; binjo-calendar.el ends here
