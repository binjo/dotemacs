;;; binjo-sql.el --- configs for sql

;; Copyright 2009
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-sql-config.el,v 0.0 2009/07/06 08:11:52 binjo Exp $
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
;;   (require 'binjo-sql)

;;; Code:

(require 'sql)
(eval-when-compile
  (require 'cl))

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(setq sql-input-ring-file-name "~/.emacs.d/sql-history")
;; sql-mysql
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))
(setq sql-connection-alist
      `((webfilter
         (sql-product 'mysql)
         (sql-server ,webfilter-server)
         (sql-user ,webfilter-user)
         (sql-password ,webfilter-password)
         (sql-database ,webfilter-database))
        (webmon
         (sql-product 'mysql)
         (sql-server ,webmon-server)
         (sql-user ,webmon-user)
         (sql-password ,webmon-password)
         (sql-database ,webmon-database))
        (fips
         (sql-product 'mysql)
         (sql-server ,fips-server)
         (sql-user ,fips-user)
         (sql-password ,fips-password)
         (sql-database ,fips-database))
        (avfeedback
         (sql-product 'mysql)
         (sql-server ,avfeedback-server)
         (sql-user ,avfeedback-user)
         (sql-password ,avfeedback-password)
         (sql-database ,avfeedback-database))))

(defun sql-make-smart-buffer-name ()
  "Return a string that can be used to rename a SQLi buffer.

This is used to set `sql-alternate-buffer-name' within
`sql-interactive-mode'."
  (or (and (boundp 'sql-name) sql-name)
      (concat (if (not(string= "" sql-server))
                  (concat
                   (or (and (string-match "[0-9.]+" sql-server) sql-server)
                       (car (split-string sql-server "\\.")))
                   "/"))
              sql-database)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq sql-alternate-buffer-name (sql-make-smart-buffer-name))
            (sql-rename-buffer)))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
           (flet ((sql-get-login (&rest what)))
             (sql-product-interactive sql-product t)))))

(defun sql-webfilter ()
  (interactive)
  (sql-connect-preset 'webfilter))

(defun sql-fips ()
  (interactive)
  (sql-connect-preset 'fips))

(defun sql-av-feedback ()
  (interactive)
  (sql-connect-preset 'avfeedback))

(defun sql-av-webmon ()
  (interactive)
  (sql-connect-preset 'webmon))

(provide 'binjo-sql)
;;; binjo-sql.el ends here
