(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode nil)
 '(canlock-password "31f72e77a9331e61ce98a230d1fbff79785c568a")
 '(column-number-mode t)
 '(custom-enabled-themes (quote (jschaf)))
 '(custom-safe-themes (quote ("9bfadc3257f16106c57a3511e85d26db32555528" default)))
 '(default-tab-width 4 t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(message-log-max 2000)
 '(parens-require-spaces nil)
 '(safe-local-variable-values (quote ((insert-tabs-mode) (encoding . utf-8) (TeX-master . t))))
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
 '(highlight-changes ((((min-colors 88) (class color)) (:background "purple1"))))
 )

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
(when (eq system-type 'windows-nt)
  ;; copy from xwl's xwl-window.el
  (let* ((all-fonts
          `(;; (mac . ("Monaco-14" "stheiti*" "hiragino maru gothic pro"))
            ;; (ns  . ("Monaco-14" "Hiragino Sans GB" "Hiragino_Kaku_Gothic_ProN"))
            (w32 . ("��Ȫ��ȿ�΢�׺�-10" "��Ȫ��ȿ�΢�׺�" "NSimSun"
                    ;; "�������б�" "�������б�" "�������б�"
                    ;; "΢���ź�" "΢���ź�"
                    ))
            ;; (x   . ,(if (string= system-name "debian..xwl")
            ;;             '("DejaVu Sans Mono-11" "wenquanyi" "wenquanyi")
            ;;           '("DejaVu LGC Sans Mono-13" "SimSun" "SimSun")))
            ))
         (fonts (cdr (assoc window-system all-fonts)))
         (default-font (nth 0 fonts))
         (cn-font (nth 1 fonts))
         (jp-font (nth 2 fonts))
         (charset-fonts `((chinese-gb2312    . ,cn-font)
                          (chinese-gbk       . ,cn-font)
                          (gb18030           . ,cn-font)
                          )))
    (set-default-font default-font)       ; this will decide font size.
    (mapc (lambda (cf)
            (set-fontset-font (frame-parameter nil 'font)
                              (car cf)
                              (font-spec :family (cdr cf) :size 16)))
          charset-fonts)))

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
(defun binjo-modeline-proper-full-path ()
  (let ((cur-dir default-directory))
    (if (> (length cur-dir) 48)
        (concat (substring cur-dir 0 24) "..."
                (substring (substring cur-dir 24) -21))
      cur-dir)))

(defun add-mode-line-fullpath ()
  "When editing a file, show the full path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (binjo-modeline-proper-full-path))))

(add-hook 'find-file-hook 'add-mode-line-fullpath)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; etags
;;; find-tag-hook
(defun set-view-find-tag-hook ()
  "set view-mode after find-tag"
  (view-mode t))
(add-hook 'find-tag-hook 'set-view-find-tag-hook)

;; etags-select
(binjo-m-global-set-key-dynamic 'etags-select
                                ((kbd "M-?") . 'etags-select-find-tag-at-point)
                                ((kbd "M-.") . 'etags-select-find-tag))

;; Color theme related
;; (add-hook 'window-setup-hook '(lambda ()
;;                                 (require 'color-theme)
;;                                 (color-theme-initialize)
;;                                 (color-theme-bharadwaj)))

;; stop calling `vc-working-revision' and `vc-state' to slow down the startup.
(eval-after-load 'vc-hooks
  (progn
    (setq vc-handled-backends nil)))
(add-hook 'window-setup-hook '(lambda ()
                                (setq vc-handled-backends '(SVN Git Hg))))

;; session
(add-hook 'after-init-hook '(lambda ()
                              (require 'session)
                              (session-initialize)))
;; desktop
;;(load "desktop")
(setq desktop-dirname "~/"
      desktop-buffers-not-to-save "\\.wubi-phrases.*\\|\\.bbdb")
(desktop-read)
(add-hook 'kill-emacs-hook '(lambda ()
                              (desktop-save-mode 1)))
;; ibuffer
;; (require 'ibuffer)

;; browser-kill-ring
(binjo-m-global-set-key-dynamic 'browse-kill-ring
                                ((kbd "C-c k") . 'browse-kill-ring))
(eval-after-load 'browse-kill-ring
  '(browse-kill-ring-default-keybindings))

;;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wubi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-input-method
 "chinese-wubi" "Chinese-GB" 'quail-use-package
 "WuBi" "WuBi"
 "wubi")

(setq default-input-method "chinese-wubi")
(defadvice toggle-input-method (before load-wubi activate)
  (require 'wubi))

(eval-after-load 'wubi
  '(progn
     (setq wubi-phrases-file "~/.wubi-phrases.el")
     (ignore-errors (wubi-load-local-phrases))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'window-setup-hook '(lambda ()
                                (require 'template)
                                (template-initialize)))

;; saveplace - emacs-fu
(add-hook 'window-setup-hook '(lambda ()
                                (require 'saveplace)
                                (setq save-place-file "~/.emacs.d/saveplace")
                                (setq-default save-place t)))

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
    ;; for gnus notify
    (require 'binjo-gnus-notify)
    (binjo-gnus-enable-unread-notify)
    ad-do-it
    ;; idle 2 minutes, then check news every 3 minutes.
    (gnus-demon-add-handler 'binjo-gnus-group-get-new-news 3 2)))

(ad-activate 'gnus)
(global-set-key (kbd "C-x g") 'gnus)

;; grep
(binjo-m-global-set-key-dynamic 'grep
                                ((kbd "C-c m g") . 'grep)
                                ((kbd "C-c m G") . 'grep-find))
(eval-after-load 'grep
  '(progn
     (grep-apply-setting 'grep-command "grep -r -nH -i -e ")
     (grep-apply-setting 'grep-find-command
                         (cons (concat
                                (shell-quote-argument
                                 (expand-file-name "find.exe"
                                                   (file-name-directory (executable-find "git"))))
                                " . -type f -exec grep -r -nH -i -e  {} NUL \";\"") 71))
     (grep-apply-setting 'grep-use-null-device nil)))

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

;; ;; epa, gpg related
;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; (setq mml2015-encrypt-to-self t
;;       mml2015-cache-passphrase t
;;       mml2015-passphrase-cache-expiry 60000
;;       mml2015-verbose t)

(eval-after-load 'gnus
  '(progn
     (setq mm-verify-option 'known
           mm-decrypt-option 'known)
     (setq gnus-buttonized-mime-types '("multipart/signed" "multipart/encrypted"))))

;; ,----
;; | Track changes for some buffer
;; `----
(defadvice switch-to-buffer (before
                             highlight-changes-for-some-buffer
                             activate)
  (cond ((memq major-mode (list ;; 'erc-mode
                           'twittering-mode))
         (let ((buffer-read-only nil)
               (inhibit-read-only t))
           (highlight-changes-mode -1)
           (highlight-changes-mode 1)
           (goto-char (point-min))))
        ((memq major-mode (list 'erc-mode))
         (when (memq (current-buffer) (erc-buffer-list))
           (goto-char (point-max))
           (forward-line -1)))))

;; c, c++ settings
(defcustom binjo-cpp-style
  '((c-tab-always-indent . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((substatement-open after)
                               (brace-list-open)))
    (c-cleanup-list . (comment-close-slash compact-empty-funcall))
    (c-offsets-alist . ((substatement-open . 0)
                        (innamespace . 0)
                        (case-label . +)
                        (access-label . -)
                        (inline-open . 0)
                        (block-open . 0)))
    (setq comment-start "/*"
          comment-end "*/")
    (setq indent-tabs-mode nil))
  "my cpp coding style.")
(c-add-style "binjo-cpp-style" binjo-cpp-style)

(defun binjo-set-c-c++-style ()
  (c-set-style "binjo-cpp-style"))

(add-hook 'c++-mode-hook 'binjo-set-c-c++-style)
(add-hook 'c-mode-hook 'binjo-set-c-c++-style)
