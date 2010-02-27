(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(canlock-password "31f72e77a9331e61ce98a230d1fbff79785c568a")
 '(column-number-mode t)
 '(default-tab-width 4 t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(parens-require-spaces nil)
 '(safe-local-variable-values (quote ((insert-tabs-mode) (encoding . utf-8) (TeX-master . t))))
 '(message-log-max 2000)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-stop-list nil)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(erc-timestamp-face ((t (:foreground "brown" :weight bold))))
 '(twit-message-face ((default (:height 1.1 :family "新宋体")) (nil nil))))

;; Turn off the annoying default backup behaviour
(if (file-directory-p "~/.emacs.d/backup")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
    (message "Directory does not exist: ~/.emacs.d/backup"))

;; ,----
;; | coding system, xwl
;; `----
(cond ((eq system-type 'windows-nt)
       (prefer-coding-system 'gbk))
      (t
       (prefer-coding-system 'utf-8-emacs)))

;; font set
(setq w32-charset-info-alist
    (cons '("gbk" w32-charset-gb2312 . 936) w32-charset-info-alist))

(setq default-frame-alist
      (append
       '((font . "fontset-gbk")) default-frame-alist))

(create-fontset-from-fontset-spec
  "-outline-新宋体-normal-r-normal-normal-16-*-96-96-c-*-fontset-gbk")
(set-fontset-font
 "fontset-default" nil
 "-outline-新宋体-normal-r-normal-*-16-*-96-96-c-*-iso10646-1" nil 'prepend)
(set-fontset-font
 "fontset-gbk" 'kana
 "-outline-新宋体-normal-r-normal-*-18-*-96-96-c-*-iso10646-1" nil 'prepend)
(set-fontset-font
 "fontset-gbk" 'han
 "-outline-新宋体-normal-r-normal-*-16-*-96-96-c-*-iso10646-1" nil 'prepend)
(set-fontset-font
 "fontset-gbk" 'cjk-misc
 "-outline-新宋体-normal-r-normal-*-16-*-96-96-c-*-iso10646-1" nil 'prepend)
(set-fontset-font
 "fontset-gbk" 'symbol
 "-outline-新宋体-normal-r-normal-*-16-*-96-96-c-*-iso10646-1" nil 'prepend)
(set-frame-font "fontset-gbk")

(setq scroll-margin 3
      scroll-conservatively 10000)

(setq frame-title-format "GNU Emacs@%b")
;; (setq uniquify-buffer-name-style 'forward)
;; (require 'uniquify)
;; (setq frame-title-format
;;       (list '((buffer-file-name " %f" (dired-directory
;;                                        dired-directory
;;                                        (revert-buffer-function " %b"
;;                                                                ("%b - Dir:  " default-directory)))))))
(defun add-mode-line-fullpath ()
  "When editing a file, show the full path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  0 nil))))
(add-hook 'find-file-hook 'add-mode-line-fullpath)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; etags
;;; find-tag-hook
(defun set-view-find-tag-hook ()
  "set view-mode after find-tag"
  (view-mode t))
(add-hook 'find-tag-hook 'set-view-find-tag-hook)

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; Color theme related
(require 'color-theme)
(color-theme-initialize)
(color-theme-bharadwaj)

;; stop calling `vc-working-revision' and `vc-state' to slow down the startup.
(eval-after-load 'vc-hooks
  '(remove-hook 'find-file-hook 'vc-find-file-hook))
(add-hook 'window-setup-hook '(lambda ()
                                (add-hook 'find-file-hook 'vc-find-file-hook)))

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; desktop
;;(load "desktop")
(setq desktop-dirname "~/"
      desktop-buffers-not-to-save "\\.wubi-phrases.*\\|\\.bbdb")
(desktop-read)
(add-hook 'kill-emacs-hook '(lambda ()
                              (desktop-save-mode 1)))
;; ibuffer
(require 'ibuffer)

;; browser-kill-ring
(require 'browse-kill-ring)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
;; ;; tab bar
;; (require 'tabbar)
;; (tabbar-mode)
;; (global-set-key (kbd "") 'tabbar-backward-group)
;; (global-set-key (kbd "") 'tabbar-forward-group)
;; (global-set-key (kbd "") 'tabbar-backward)
;; (global-set-key (kbd "") 'tabbar-forward)

;;; ido
(require 'ido)
(ido-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show line number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'linum)
;; (add-hook 'find-file-hook (lambda () (linum-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wubi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'wubi)
(wubi-load-local-phrases) ; add user's Wubi phrases

(register-input-method
 "chinese-wubi" "Chinese-GB" 'quail-use-package
 "WuBi" "WuBi"
 "wubi")

(setq default-input-method "chinese-wubi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'template)
(template-initialize)

;; saveplace - emacs-fu
(setq save-place-file "~/.emacs.d/saveplace")
(require 'saveplace)
(setq-default save-place t)                   ;; activate it for all buffers

;; custom info
(setq Info-directory-list
      `("~/.emacs.d/info" ,@Info-default-directory-list))

;; Emacs-fu: making buffer names unique
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;;; for GNUS
;; can't put this in .gnus.el, since that's t000 late...
(defun binjo-gnus-group-get-new-news ()
  "Only get news for groups with a level lower than 4.
This is because some levels' updating takes too long time."
  (interactive)
  (gnus-group-get-new-news 3)
  (force-mode-line-update))

;; checking news periodly
(defadvice gnus (around switch-or-start)
  "Start `gnus' or switch to started *Group* buffer."
  (if (gnus-alive-p)
      (switch-to-buffer "*Group*")
    ad-do-it
    ;; idle 5 minutes, then check news every 5 minutes.
    (gnus-demon-add-handler 'binjo-gnus-group-get-new-news 5 5)))

(ad-activate 'gnus)
(global-set-key (kbd "C-x g") 'gnus)

;; grep
(setq grep-command "grep -n -e ")
(global-set-key (kbd "C-c m g") 'grep)

(require 'binjo-bindings)

;; redefines for archive-mode
(require 'binjo-arc)

;; get rid of 'yes/no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; proxy
(if binjo-at-company-p
    (setq url-proxy-services '(("http" . "172.25.25.4:808"))
          url-using-proxy t))

;; Shut off compiler error pop-up warning about
;;    save-excursion defeated by set-buffer
(setq byte-compile-warnings '(not suspicious))