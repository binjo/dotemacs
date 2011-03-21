;;; binjo-jabber.el --- settings for jabber goes here

;; Copyright 2010 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-jabber.el,v 0.0 2010/11/04 13:37:25 binjo Exp $
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

;; 2010/11/04, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-jabber)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'jabber-autoloads)


;; account
(setq jabber-account-list
      `((,binjo-main-account
         (:network-server . "localhost")
         (:connection-type . network)
         (:port . 5222))))

;; log
(setq jabber-history-enabled t
      jabber-history-muc-enabled t
      jabber-use-global-history nil
      jabber-history-dir "~/.emacs.d/jabber")

(defun jabber ()
  "switch to jabber buffer or issue `jabber-connect'."
  (interactive)
  (if (buffer-live-p (get-buffer "*-jabber-roster-*"))
      (switch-to-buffer "*-jabber-roster-*")
    (jabber-connect-all)))

(provide 'binjo-jabber)
;;; binjo-jabber.el ends here
