;; macro
(defmacro binjo-m-global-set-key-dynamic (package key func)
  "Call `global-set-key' to set FUNC with KEY, and `require' PACKAGE dynamically."
  `(global-set-key (kbd ,key) '(lambda ()
                                 (interactive)
                                 (require ,package)
                                 (call-interactively ,func))))
;; eshell
(eval-after-load 'eshell
  '(progn
     (require 'binjo-eshell-util)))

(defun binjo-unify-line (begin end)
  "wipe out the same line of specified region"
  (interactive "r")
  (goto-char begin)
  (let ((line "")
        (unified ()))
    (save-excursion
      (while (< (line-end-position) end)
        (setq line (downcase (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))))
        (unless (assoc-string line unified)
          (if (null unified)
              (setq unified (list (cons line 1)))
            (setq unified (append  (list (cons line 1)) unified))))
        (forward-line))
      (pop-to-buffer (get-buffer-create "*unified-line*"))
      (erase-buffer)
      (mapc (lambda (x)
              (insert (car x))
              (insert "\n")) (reverse unified)))))
(global-set-key (kbd "C-x r u") 'binjo-unify-line)

(defun binjo-copy-line (&optional arg)
  "copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (clipboard-kill-ring-save (line-beginning-position)
                  (line-end-position (+ 0 arg)))
  (message "%d line%scopied" arg (if (= 1 arg) " " "s ")))
(global-set-key (kbd "C-c h") 'binjo-copy-line)

;;; erc
(binjo-m-global-set-key-dynamic 'binjo-erc "C-c n e" 'binjo-erc-select)

;; auctex
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auctex")
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (eval-after-load "tex"
;;   '(progn
;;      (setq TeX-output-view-style
;;            (cons '("^pdf$" "." "start \"title\" %o") TeX-output-view-style)
;;            ;(cons '("^pdf$" "." "cmdproxy /C %o ") TeX-output-view-style)
;;       )))

;; sql-config
(binjo-m-global-set-key-dynamic 'binjo-sql "C-c s w" 'sql-webfilter)
(binjo-m-global-set-key-dynamic 'binjo-sql "C-c s f" 'sql-fips)
(binjo-m-global-set-key-dynamic 'binjo-sql "C-c s a" 'sql-av-feedback)

;; org-mode
(require 'binjo-org)

;; twit.el
(require 'binjo-twit)

;; smex
(binjo-m-global-set-key-dynamic 'smex "M-x" 'smex)
(binjo-m-global-set-key-dynamic 'smex "M-X" 'smex-major-mode-commands)
(binjo-m-global-set-key-dynamic 'smex "C-c M-x" 'smex-update-and-run)
(eval-after-load 'smex
  '(smex-initialize))
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; boxquote
(binjo-m-global-set-key-dynamic 'boxquote "C-c b r" 'boxquote-region)
(binjo-m-global-set-key-dynamic 'boxquote "C-c b t" 'boxquote-title)
(binjo-m-global-set-key-dynamic 'boxquote "C-c b f" 'boxquote-describe-function)
(binjo-m-global-set-key-dynamic 'boxquote "C-c b v" 'boxquote-describe-variable)
(binjo-m-global-set-key-dynamic 'boxquote "C-c b k" 'boxquote-describe-key)
(binjo-m-global-set-key-dynamic 'boxquote "C-c b !" 'boxquote-shell-command)
(binjo-m-global-set-key-dynamic 'boxquote "C-c b y" 'boxquote-yank)

;; YASnippet
(require 'yasnippet-bundle)

;;; anchiva
(binjo-m-global-set-key-dynamic 'anchiva "C-c v u" 'anchiva-submit-sample)
(binjo-m-global-set-key-dynamic 'anchiva "C-c v t" 'anchiva-check-mwcs-test)
(binjo-m-global-set-key-dynamic 'anchiva "C-c v m" 'anchiva-submit-sig-mwcs)
(binjo-m-global-set-key-dynamic 'anchiva "C-c v b" 'anchiva-browse-all-urls)

;; babel, use online api to translate
(binjo-m-global-set-key-dynamic 'babel "C-c t r" 'babel-region)
(binjo-m-global-set-key-dynamic 'babel "C-c t b" 'babel)
(eval-after-load 'babel
  '(setq babel-preferred-to-language "Chinese (Simplified)"))

;; emms
(require 'binjo-emms)

;; douban
(require 'douban-emacs)
(binjo-m-global-set-key-dynamic 'douban-emacs "C-c d n" 'douban-create-note)

(if binjo-at-company-p
    (progn
      (require 'edit-server)
      (edit-server-start)))

(binjo-m-global-set-key-dynamic 'magit "C-c g m" 'magit-status)

(require 'kmacro-ring-list)
(binjo-m-global-set-key-dynamic 'kmacro-ring-list "C-c m r" 'kmacro-ring-list)

(setq last-kbd-macro
   [?\C-e ?: ?3 ?: ?* ?: ?\C-\M-% ?\[ ?- ?| ?\\ ?  ?| ?\C-q ?\C-j ?\] return return ?! ?\C-x ?\C-s ?\C-a ?\C-k ?\C-z])
