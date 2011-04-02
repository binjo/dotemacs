;;; Python

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

(eval-after-load 'python
  '(add-hook 'python-mode-hook (lambda ()
                                 (setq tab-width 4))))

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

;; haskell
(autoload 'haskell-mode "haskell-mode")
(add-to-list 'auto-mode-alist        '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(eval-after-load 'haskell-mode
  '(progn
     (load "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file.el")
     (setq haskell-program-name
           (shell-quote-argument "c:/Program Files/Haskell Platform/bin/ghci.exe"))
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))

;; pintool's header
(add-to-list 'auto-mode-alist  '("\\.PH$" . c++-mode))

(binjo-m-global-set-key-dynamic 'cdb-gud
  ((kbd "C-c d b") . 'cdb))
