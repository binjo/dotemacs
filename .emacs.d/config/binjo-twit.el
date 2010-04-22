;;; binjo-twit.el --- config for twit.el

;; Copyright 2010 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-twit.el,v 0.0 2010/02/03 15:16:07 binjo Exp $
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

;; Configs for twit.el

;;; History:

;; 02/03/10, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-twit)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'twit-appspot)
(require 'twittering-mode)



;; twit.el related settings
(setq twit-filter-tweets-regex "^$"
      twit-show-user-images    nil
      twit-fill-tweets         nil)

(if binjo-at-company-p
    (setq twit-proxy           "172.25.25.4:808"))

(global-set-key (kbd "C-c t s") 'twit-show-recent-tweets)
(global-set-key (kbd "C-c t d") 'twit-show-direct-tweets)
(global-set-key (kbd "C-c t l") 'twit-follow-recent-tweets)
(global-set-key (kbd "C-c t S") 'twit-stop-following-tweets)
(global-set-key (kbd "C-c t w") 'twit-post)

;; TODO notify in mode-line
(defun binjo-twit-hook-notify-new-tweets ()
  "Called by `twit-new-tweet-hook'."
  (message "New tweets from %s" (cadr twit-last-tweet)))

(add-hook 'twit-new-tweet-hook 'binjo-twit-hook-notify-new-tweets)

;; (if binjo-at-company-p
;;     (progn
;;       (run-with-timer "08:30am" (* 24 60 60) 'twit-follow-recent-tweets)
;;       (run-with-timer "18:00pm" (* 24 60 60) 'twit-stop-following-tweets)))

;; twittering-mode
(setq twittering-username twit-user
      twittering-password twit-pass)

(setq twittering-api-host        binjo-twitter-api-url
      twittering-api-search-host binjo-twitter-search-url
      twittering-use-ssl         nil)

(setq twittering-status-format
      "%i %C{%a %m.%d/%H:%M:%S} %s, from %f%L%r%R:\n%FILL{       %T}\n")

(add-hook 'twittering-mode-hook (lambda ()
                                  (twittering-icon-mode 1)
                                  (twittering-enable-unread-status-notifier)))

(eval-after-load 'twittering-mode
  '(progn
     (define-key twittering-mode-map "c" 'twittering-current-timeline)

     (define-key twittering-mode-map "n" 'twittering-goto-next-status)
     (define-key twittering-mode-map "p" 'twittering-goto-previous-status)
     (define-key twittering-mode-map "N" 'twittering-goto-next-status-of-user)
     (define-key twittering-mode-map "P" 'twittering-goto-previous-status-of-user)
     (define-key twittering-mode-map "q" 'twittering-suspend)

     (global-set-key (kbd "C-c t m") 'twittering-mode)
     (global-set-key (kbd "C-c t i") 'twittering-start)
     (global-set-key (kbd "C-c t o") 'twittering-stop)
     (global-set-key (kbd "C-c t u") 'twittering-update-status-interactive)
     (global-set-key (kbd "C-c t D") 'twittering-direct-messages-timeline)
     ))

;; utils
(defun binjo-twittering-jmp (buf)
  "Switch to twittering buffer with BUF name."
  (interactive
   (list (ido-completing-read "Twittering "
                              (mapcar 'buffer-name (twittering-get-buffer-list)))))
  (switch-to-buffer buf))

(global-set-key (kbd "C-c t t") 'binjo-twittering-jmp)

(provide 'binjo-twit)
;;; binjo-twit.el ends here
