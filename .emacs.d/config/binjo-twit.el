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



;; twit.el related settings
(eval-after-load 'twit-appspot
  '(setq twit-filter-tweets-regex "^$"
         twit-show-user-images    nil
         twit-fill-tweets         nil))

(if binjo-at-company-p
    (setq twit-proxy           "172.25.25.4:808"))

(binjo-m-global-set-key-dynamic 'twit-appspot
                                ((kbd "C-c t s") . 'twit-show-recent-tweets)
                                ((kbd "C-c t d") . 'twit-show-direct-tweets)
                                ((kbd "C-c t l") . 'twit-follow-recent-tweets)
                                ((kbd "C-c t S") . 'twit-stop-following-tweets)
                                ((kbd "C-c t w") . 'twit-post))

;; TODO notify in mode-line
(defun binjo-twit-hook-notify-new-tweets ()
  "Called by `twit-new-tweet-hook'."
  (message "New tweets from %s" (cadr twit-last-tweet)))

(eval-after-load 'twit-appspot
  '(add-hook 'twit-new-tweet-hook 'binjo-twit-hook-notify-new-tweets))

;; (if binjo-at-company-p
;;     (progn
;;       (run-with-timer "08:30am" (* 24 60 60) 'twit-follow-recent-tweets)
;;       (run-with-timer "18:00pm" (* 24 60 60) 'twit-stop-following-tweets)))

(binjo-m-global-set-key-dynamic 'twittering-mode
                                ((kbd "C-c t m") . 'twittering-mode)
                                ((kbd "C-c t i") . 'twittering-start)
                                ((kbd "C-c t o") . 'twittering-stop)
                                ((kbd "C-c t u") . 'twittering-update-status-interactive)
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
     (setq twittering-username twit-user
           twittering-password twit-pass)

     (setq twittering-api-host        binjo-twitter-api-url
           twittering-api-search-host binjo-twitter-search-url
           twittering-use-ssl         nil)

     (setq twittering-status-format
           "%i %C{%a %m.%d/%H:%M:%S} %s, from %f%L%r%R:\n%FILL{       %T}\n")

     ;; for modeline notify
     (defconst twittering-logo-image
       (when (image-type-available-p 'xpm)
         '(image :type xpm
                 :ascent center
                 :data
                 "/* XPM */
static char * twitter_xpm[] = {
\"16 16 104 2\",
\"  	c None\",
\". 	c #A3A2A2\",
\"+ 	c #ADACAC\",
\"@ 	c #64CFFC\",
\"# 	c #64D0FC\",
\"$ 	c #69D1FC\",
\"% 	c #6AD1FC\",
\"& 	c #6CD2FC\",
\"* 	c #6DD2FC\",
\"= 	c #6ED3FC\",
\"- 	c #70D3FC\",
\"; 	c #71D3FC\",
\"> 	c #C2C1C1\",
\", 	c #72D3FC\",
\"' 	c #71D4FC\",
\") 	c #72D4FC\",
\"! 	c #73D4FC\",
\"~ 	c #C3C3C3\",
\"{ 	c #75D5FC\",
\"] 	c #76D5FC\",
\"^ 	c #78D6FC\",
\"/ 	c #79D6FC\",
\"( 	c #7BD6FC\",
\"_ 	c #7DD7FC\",
\": 	c #7FD8FC\",
\"< 	c #80D8FC\",
\"[ 	c #80D8FD\",
\"} 	c #81D8FD\",
\"| 	c #C9C8C8\",
\"1 	c #CAC9C9\",
\"2 	c #CCCAC9\",
\"3 	c #85DAFD\",
\"4 	c #89DBFD\",
\"5 	c #8BDCFD\",
\"6 	c #8CDCFD\",
\"7 	c #91DDFD\",
\"8 	c #92DDFD\",
\"9 	c #D3D2D2\",
\"0 	c #D7D7D6\",
\"a 	c #A8E4FC\",
\"b 	c #A5E5FF\",
\"c 	c #A7E5FF\",
\"d 	c #A6E6FF\",
\"e 	c #A7E6FF\",
\"f 	c #ABE6FE\",
\"g 	c #AEE7FD\",
\"h 	c #AFE8FF\",
\"i 	c #E1DFDF\",
\"j 	c #B9EAFD\",
\"k 	c #B9EAFE\",
\"l 	c #B8EBFF\",
\"m 	c #E5E2E1\",
\"n 	c #E6E2E1\",
\"o 	c #C7EFFF\",
\"p 	c #C8F0FF\",
\"q 	c #ECE9E7\",
\"r 	c #ECE9E8\",
\"s 	c #EDECEB\",
\"t 	c #D0F3FF\",
\"u 	c #D3F3FF\",
\"v 	c #EEEDED\",
\"w 	c #D4F3FF\",
\"x 	c #EFEDEC\",
\"y 	c #D6F4FF\",
\"z 	c #D8F6FF\",
\"A 	c #D9F6FF\",
\"B 	c #F3F0EE\",
\"C 	c #F4F0EF\",
\"D 	c #DBF6FF\",
\"E 	c #DEF6FF\",
\"F 	c #DFF6FF\",
\"G 	c #E1F6FF\",
\"H 	c #DFF7FF\",
\"I 	c #DFF8FF\",
\"J 	c #E1F8FF\",
\"K 	c #E7F7FF\",
\"L 	c #F7F4F3\",
\"M 	c #F8F4F3\",
\"N 	c #E4F9FF\",
\"O 	c #EAF8FE\",
\"P 	c #EBF8FE\",
\"Q 	c #F9F6F5\",
\"R 	c #EFFAFE\",
\"S 	c #ECFBFF\",
\"T 	c #FBF8F7\",
\"U 	c #F9F9F9\",
\"V 	c #FAF9F9\",
\"W 	c #FDF9F8\",
\"X 	c #FAFAFA\",
\"Y 	c #F3FCFF\",
\"Z 	c #FCFAFA\",
\"` 	c #FDFAF9\",
\" .	c #F6FCFF\",
\"..	c #FCFBFA\",
\"+.	c #F2FEFF\",
\"@.	c #F8FDFF\",
\"#.	c #FDFCFB\",
\"$.	c #FDFCFC\",
\"%.	c #F9FDFF\",
\"&.	c #FFFCFA\",
\"*.	c #FFFEFC\",
\"=.	c #FFFEFD\",
\"-.	c #FDFFFF\",
\";.	c #FFFFFF\",
\"      0 q r                     \",
\"    v ;.H N ;.~                 \",
\"    =.o ! ( D M                 \",
\"    X b $ % k *.B Q L x         \",
\"    V e & = a G J F E S ;.9     \",
\"    V e * ! { ^ ^ ^ ] _ t `     \",
\"    V e * ! ' = = = = @ b $.    \",
\"    V e * ) / < : : _ 3 z T     \",
\"    V e & = g R P O O Y V .     \",
\"    U d & * j ;.;.;.;.;.>       \",
\"    Z h & = 7 K @. . .%...      \",
\"    W y , - ; [ 6 5 4 8 I `     \",
\"    1 ;.f & = = * * * # c #.    \",
\"      s -.l } ! ! ! ' { p &.    \",
\"        i ;.+.A u w u J ;.|     \",
\"            2 n B B C m +       \"};
"))
       "Image for twitter logo.")

     (defconst twittering-logo
       (if twittering-logo-image
           (apply 'propertize " "
                  `(display ,twittering-logo-image))
         "tw"))

     ;; redefine `twittering-make-unread-status-notifier-string'
     (defun twittering-make-unread-status-notifier-string ()
       "Generate a string that displays unread statuses."
       (setq twittering-unread-status-info
             (remove nil
                     (mapcar (lambda (entry)
                               (when (buffer-live-p (car entry))
                                 entry))
                             twittering-unread-status-info)))
       (let ((sum (apply '+ (mapcar 'cadr twittering-unread-status-info))))
         (if (= 0 sum)
             ""
           (format "%s(%d)" twittering-logo sum))))

     (add-hook 'twittering-mode-hook (lambda ()
                                       (setq twittering-convert-program (executable-find "imconvert"))
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

     (global-set-key (kbd "C-c t t") 'binjo-twittering-jmp)

     ))

(provide 'binjo-twit)
;;; binjo-twit.el ends here
