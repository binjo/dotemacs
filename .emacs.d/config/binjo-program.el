;;; Python
(autoload 'python-mode "python-mode" "Python editing mode." t)
(eval-after-load 'python
  '(progn
     (setq auto-mode-alist
           (cons '("\\.py$" . python-mode) auto-mode-alist))
     (setq interpreter-mode-alist
           (cons '("python" . python-mode) interpreter-mode-alist))

      (global-font-lock-mode t)
     (setq font-lock-maximum-decoration t)

     (add-hook 'python-mode-hook #'(lambda ()
                                     (local-set-key "\C-x\t"
                                                    #'(lambda ()
                                                        (interactive)
                                                        (insert "    ")))))))

;; python in eshell
(defun n-eshell-exec-python (command args)
  "to make eshell understand python scripts."
  (if (string-match "^.+\.py[ \t]*" command)
      (progn
         (setq args (cons command args))
;;         (setq args (cons "-S" args))
         (throw 'eshell-replace-command
                (eshell-parse-command "python" args))
        )
    )
  )
(eval-after-load 'eshell
  '(add-hook 'eshell-named-command-hook 'n-eshell-exec-python))

;; settings for gud of pdb
(eval-after-load 'gud
  '(setq gud-pdb-command-name "python -i -m pdb"))
(global-set-key (kbd "C-c p d") 'pdb)

;;; Ruby
;; Based on http://infolab.stanford.edu/~manku/dotemacs.html
(autoload 'ruby-mode "ruby-mode"
    "Mode for editing ruby source files")
(eval-after-load 'ruby-mode
  '(progn
     (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
     (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
     (autoload 'run-ruby "inf-ruby"
       "Run an inferior Ruby process")
     (autoload 'inf-ruby-keys "inf-ruby"
       "Set local key defs for inf-ruby in ruby-mode")
     (add-hook 'ruby-mode-hook
               '(lambda ()
                  (inf-ruby-keys)))
     ;; If you have Emacs 19.2x or older, use rubydb2x
     (autoload 'rubydb "rubydb3x" "Ruby debugger" t)
     ;; uncomment the next line if you want syntax highlighting
     (add-hook 'ruby-mode-hook 'turn-on-font-lock)))

;; Php
(autoload 'php-mode "php-mode" "Php editing mode." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(eval-after-load 'php-mode
  '(add-to-list 'interpreter-mode-alist '("php" . php-mode)))

;; Js related setting
(autoload 'js2-mode "js2-mode" nil t)
(eval-after-load 'js2-mode
  '(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
(binjo-m-global-set-key-dynamic 'js-console
                                ((kbd "C-c j s") . 'js-console))

(binjo-m-global-set-key-dynamic 'js-comint
                                ((kbd "C-c j c") . 'run-js)
                                ((kbd "C-c j r") . 'js-send-region-and-go))
(eval-after-load 'js-comint
  '(setq inferior-js-program-command "java -jar d:\\Datas\\source\\rhino1_7R2\\js.jar"))

;;; c# highlighting
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(eval-after-load 'csharp-mode
  '(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)))

;;;; highlight special keywords, copy from xwl's xwl-programming.el
(setq binjo-keyword-highlight-modes
      '(java-mode c-mode c++-mode emacs-lisp-mode scheme-mode
    text-mode outline-mode python-mode perl-mode haskell-mode))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-todo-face)

(modify-face 'font-lock-fixme-face "black" "yellow" nil t nil t nil nil)
(modify-face 'font-lock-todo-face  "black" "yellow" nil t nil nil nil nil)

(defun xwl-highlight-special-keywords ()
  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t)
             ("\\<\\(TODO\\)"  1 'font-lock-todo-face  t))))
        binjo-keyword-highlight-modes))

(add-hook 'window-setup-hook 'xwl-highlight-special-keywords)

;; asp highlight
(autoload 'asp-mode "asp-mode")
(eval-after-load 'asp-mode
  '(add-to-list 'auto-mode-alist '("\\.aspx?" . asp-mode)))

(ignore-errors
  (progn

(require 'mmm-auto)
(require 'mmm-sample)
(setq mmm-global-mode 'maybe)
(setq mmm-classes-alist
      (append '((text-html :submode html-mode
                           :front "<html>"
                           :front-offset (beginning-of-line 0)
                           :back "</html>"
                           :back-offset (end-of-line 1)
                           :face mmm-code-submode-face)
                )
              mmm-classes-alist))

;; (setq mmm-mode-ext-classes-alist
;;       '((message-mode nil text-html)
;;         (gnus-article-edit-mode nil text-html)
;;         (text-mode nil text-html)
;;         ;; (scheme-mode nil c-in-scheme)
;;         ))

;; (setq mmm-global-classes
;;       (append '(text-html)
;;               mmm-global-classes))

(mmm-add-to-major-mode-preferences 'javascript 'js2-mode)

(defun xwl-mmm-refresh ()
  "Re-apply mmm-mode when buffer contents have been modified."
  (when (and mmm-mode (buffer-modified-p))
    (mmm-apply-all)))

(add-hook 'post-command-hook 'xwl-mmm-refresh)

))

;; haskell
(load "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file.el")
(eval-after-load 'haskell-mode
  '(progn
     (setq haskell-program-name
           (shell-quote-argument "c:/Program Files/Haskell Platform/bin/ghci.exe"))
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
     (add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))))

;; pintool's header
(add-to-list 'auto-mode-alist  '("\\.PH$" . c++-mode))
