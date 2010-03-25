;;; binjo-erc.el --- erc config

;; Copyright 2009 (C) Binjo
;;
;; Author: binjo <binjo.cn@gmail.com>
;; Version: $Id: binjo-erc.el,v 0.0 2009/10/20 02:35:07 binjo Exp $
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

;;     most config comes from xwl <william.xwl@gmail.com>
;;           http://xwl.appspot.com/ref/xwl-erc.el

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-erc)


;;; History:
;;

;;; Code:
(require 'erc)

(setq erc-server-history-list '("irc.0x557.net"
                                "irc.freenode.net"
                                "127.0.0.1" ; stunnel to irc.0x557.net
                                "irc.oftc.net"))

(setq erc-mode-line-format "%t %a"
      erc-timestamp-format-right "[%Y-%m-%d %T]"
      erc-fill-column 100
      ;; erc-insert-timestamp-function 'erc-insert-timestamp-left
      )

(setq erc-join-buffer 'buffer
      erc-auto-query 'bury)

;(setq erc-default-port "9940")
(setq  erc-server-coding-system '(utf-8 . utf-8)
       erc-encoding-coding-alist '(("#segfault" . gb2312)
                                   ("#ph4nt0m" . gb2312)))
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#metasploit" "#haskell")
        ("0x557.net" "#segfault")
        ("oftc.net" "#emacs-cn")))

;; trim erc nicks
(setq erc-format-nick-function 'xwl-erc-format-nick)

(defun xwl-erc-format-nick (&optional user channel-data)
  "Like `erc-format-nick' but trim nick to a fixed length. "
  (let ((nick (erc-format-nick user channel-data)))
    (when (> (length nick) 7)
      (setq nick (concat (substring nick 0 4)
                         ".."
                         (substring (substring nick 7) -1))))
    nick))

;; match & track
(require 'erc-match)
(erc-match-mode 1)
(setq erc-current-nick-highlight-type 'nick-or-keyword)
(setq erc-keywords '("gnus" "twitter" "0day"))
(setq erc-pals nil)

;; track
(require 'erc-track)

(setq erc-track-faces-priority-list
      '(erc-query-buffer-face
        erc-current-nick-face
        erc-keyword-face
        erc-pal-face
        erc-default-face
        )
      erc-track-switch-direction 'importance
      erc-track-priority-faces-only 'all
      erc-track-showcount t)

;; ignore
(setq erc-ignore-list nil)
(setq erc-hide-list
      '("JOIN" "PART" "QUIT" "MODE" "NICK"))

;; log
(require 'erc-log)
(erc-log-mode 1)
(setq erc-log-channels-directory "~/.emacs.d/erc/"
      erc-save-buffer-on-part t
      erc-log-file-coding-system 'utf-8
      erc-log-write-after-send t
      erc-log-write-after-insert t)

;; goodies
(require 'erc-goodies)
(erc-readonly-mode 1)
(erc-smiley-mode 1)

;; (erc-autojoin-mode 0)
;; (defun binjo-post-cloak-autojoin (proc parsed)
;;   "Autojoin until NickServ tells cloak has been set"
;;   (with-current-buffer (process-buffer proc)
;;     (when (and (string-match ".*.freenode.net"
;;                              (erc-response.sender parsed))
;;                (string-match ".*NickServ set your hostname to.*"
;;                              (erc-response.contents parsed)))
;;       (erc-autojoin-channels erc-session-server (erc-current-nick))
;;       nil)))
;; (add-hook 'erc-server-NOTICE-functions 'binjo-post-cloak-autojoin)

(defun xwl-erc-cmd-WHOIS (nick)
  "Run /whois easily by key sequences."
  (interactive
   (list
    (ido-completing-read
     "/whois "
     (erc-get-channel-nickname-list))))
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (concat "/whois " nick))
    (erc-send-current-line)
    (goto-char (point-max))))

(defun xwl-erc-cmd-bitlbee-blist ()
  "Run `blist' easily by key sequences."
  (interactive)
  (if (string= "&bitlbee" (buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "blist")
        (erc-send-current-line)
        (goto-char (point-max)))
    (message "not in &bitlbee buffer")))

(defun binjo-erc-switch-buffer ()
  "Move cursor to the last line, then call `ido-switch-buffer'."
  (interactive)
  (unless (< (point) (point-max))
    (forward-line -1))
  (ido-switch-buffer))

(defun binjo-erc-track-switch-buffer (arg)
  "Move cursor to the last line, then call `erc-track-switch-buffer'."
  (interactive "p")
  (when (eq major-mode 'erc-mode)
      (unless (< (point) (point-max))
        (forward-line -1)))
  (erc-track-switch-buffer arg))

(defun xwl-erc-mode-hook ()
  (auto-fill-mode -1)

  (define-key erc-mode-map (kbd "C-c C-w") 'xwl-erc-cmd-WHOIS)
  (define-key erc-mode-map (kbd "C-c C-b") 'xwl-erc-cmd-bitlbee-blist)
  (define-key erc-mode-map (kbd "C-x b") 'binjo-erc-switch-buffer)
  (define-key erc-mode-map (kbd "M-m") 'erc-bol))

;;; C-c C-@ is kind of annoying...
(global-set-key (kbd "C-c C-2") 'binjo-erc-track-switch-buffer)

(add-hook 'erc-mode-hook 'xwl-erc-mode-hook)

(defun binjo-erc-select ()
  "Copy of xwl's xwl-erc-select."
  (interactive)
  ;; freenode
  (erc-select :server "irc.freenode.net"
              :port 6667
              :nick erc-nick
              :password erc-password)
  ;; #emacs-cn
  (erc-select :server "irc.oftc.net"
              :port 6667
              :nick erc-nick
              :password erc-password)
  ;; 0x557
  (erc-select :server "localhost"
              :port 6667
              :nick erc-nick
              :password erc-0x557-password))
(global-set-key (kbd "C-c n e") 'binjo-erc-select)

(provide 'binjo-erc)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; binjo-erc.el ends here
