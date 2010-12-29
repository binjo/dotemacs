;;; binjo-twit.el --- config for twittering-mode.el

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

;; Configs for twittering-mode.el

;;; History:

;; 02/03/10, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-twit)

;;; Code:

(eval-when-compile
  (require 'cl))



(binjo-m-global-set-key-dynamic 'twittering-mode
                                ((kbd "C-c t m") . 'twit)
                                ((kbd "C-c t i") . 'twittering-start)
                                ((kbd "C-c t o") . 'twittering-stop)
                                ((kbd "C-c t u") . 'twittering-update-status-interactive)
                                ((kbd "C-c t d") . 'twittering-direct-message)
                                ((kbd "C-c t D") . 'twittering-direct-messages-timeline))

;; utils
(defun binjo-twittering-jmp (buf)
  "Switch to twittering buffer with BUF name."
  (interactive
   (list (ido-completing-read "Twittering "
                              (mapcar 'buffer-name (twittering-get-buffer-list)))))
  (switch-to-buffer buf))

;; twittering-mode
(eval-after-load 'twittering-mode
  '(progn
     ;; redefine service method table
     (setq twittering-service-method-table
           `((twitter (api ,binjo-twitter-api-url)
                      (search ,binjo-twitter-search-url)
                      (web ,binjo-twitter-api-url)
                      (api-prefix "1/")
                      (search-method "search")
                      (status-url twittering-get-status-url-twitter)
                      (search-url twittering-get-search-url-twitter))
             (sina (api "api.t.sina.com.cn")
                   (web "t.sina.com.cn")
                   (oauth-request-token-url-without-scheme "://api.t.sina.com.cn/oauth/request_token")
                   (oauth-authorization-url-base-without-scheme "://api.t.sina.com.cn/oauth/authorize?oauth_token=")
                   (oauth-access-token-url-without-scheme "://api.t.sina.com.cn/oauth/access_token")
                   (oauth-consumer-key ,binjo-private-sina-app-key)
                   (oauth-consumer-secret ,binjo-private-sina-app-secret)
                   (status-url twittering-get-status-url-sina)
                   (search-url twittering-get-search-url-twitter))
             (statusnet
              (status-url twittering-get-status-url-statusnet)
              (search-url twittering-get-search-url-statusnet))))

     (setq twittering-accounts
           `((twitter (username ,twit-user)
                      (password ,twit-pass)
                      (auth     basic))
             (sina (username ,binjo-private-sina-username)
                   (auth oauth))))

     (setq twittering-enabled-services '(twitter sina))

     (setq twittering-oauth-use-ssl   nil
           twittering-use-ssl         nil)

     (setq twittering-status-format
           "%i %C{%a %m.%d/%H:%M:%S} %s, from %f%L%r%R:\n%FOLD[       ]{%t%T}\n"
           twittering-retweet-format "RT @%s: %t")

     (setq twittering-url-show-status nil
           twittering-notify-successful-http-get nil)

     (if binjo-at-company-p
         (twittering-toggle-proxy)
       ;; fuck gfw
       (defadvice start-process (before binjo-ad-proxy-on-appspot activate)
         "Set proxy for appspot.com"
         (let ((name (ad-get-arg 0))
               (call-args (ad-get-args 3)))
           (if (string= name "*twmode-curl*")
               (ad-set-args 3 `(,@call-args "-x" "www.google.cn:80"))))))

     (setq twittering-update-status-function
           'twittering-update-status-from-pop-up-buffer)

     (setq twittering-new-tweets-count-excluding-me t)

     (defadvice twittering-initialize-global-variables-if-necessary
       (before binjo-ad-set-convert-var activate)
       "Set proper convert before calling `twittering-initialize-global-variables-if-necessary',
enable icon mode and unread status notifier."
       (if (string-match "c:" twittering-convert-program 0)
           (setq twittering-convert-program
                 (expand-file-name
                  "convert.exe"
                  (expand-file-name
                   "w32"
                   (file-name-directory (symbol-file 'twit)))))))

     (add-hook 'twittering-mode-hook (lambda ()
                                       (twittering-icon-mode 1)
                                       (twittering-enable-unread-status-notifier)))

     (define-key twittering-mode-map "c" 'twittering-current-timeline)

     (define-key twittering-mode-map "n" 'twittering-goto-next-status)
     (define-key twittering-mode-map "p" 'twittering-goto-previous-status)
     (define-key twittering-mode-map "N" 'twittering-goto-next-status-of-user)
     (define-key twittering-mode-map "P" 'twittering-goto-previous-status-of-user)
     (define-key twittering-mode-map "q" 'twittering-suspend)
     (define-key twittering-mode-map "F" 'twittering-follow)
     (define-key twittering-mode-map "K" 'twittering-unfollow)
     (define-key twittering-mode-map "R" 'twittering-reply-to-user)

     (global-set-key (kbd "C-c t t") 'binjo-twittering-jmp)

     ))

(provide 'binjo-twit)
;;; binjo-twit.el ends here
