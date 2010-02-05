;; eshell
(require 'binjo-eshell-util)

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
(require 'binjo-erc)

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
;;            ;»ò(cons '("^pdf$" "." "cmdproxy /C %o ") TeX-output-view-style)
;;       )))

;; sql-config
(require 'binjo-sql)
(global-set-key (kbd "C-c s w") 'sql-webfilter)
(global-set-key (kbd "C-c s f") 'sql-fips)
(global-set-key (kbd "C-c s a") 'sql-av-feedback)

;; org-mode
(require 'binjo-org)

;; twit.el
(require 'binjo-twit)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; boxquote
(require 'boxquote)
(global-set-key (kbd "C-c b r") 'boxquote-region)
(global-set-key (kbd "C-c b t") 'boxquote-title)
(global-set-key (kbd "C-c b f") 'boxquote-describe-function)
(global-set-key (kbd "C-c b v") 'boxquote-describe-variable)
(global-set-key (kbd "C-c b k") 'boxquote-describe-key)
(global-set-key (kbd "C-c b !") 'boxquote-shell-command)
(global-set-key (kbd "C-c b y") 'boxquote-yank)

;; YASnippet
(require 'yasnippet-bundle)

;;; anchiva
(require 'anchiva)
(global-set-key (kbd "C-c v u") 'anchiva-submit-sample)
(global-set-key (kbd "C-c v t") 'anchiva-check-mwcs-test)
(global-set-key (kbd "C-c v m") 'anchiva-submit-sig-mwcs)
(global-set-key (kbd "C-c v b") 'anchiva-browse-all-urls)

;; babel, use online api to translate
(require 'babel)
(setq babel-preferred-to-language "Chinese (Simplified)")
(global-set-key (kbd "C-c t r") 'babel-region)
(global-set-key (kbd "C-c t b") 'babel)

;; emms
(require 'binjo-emms)

;; douban
(require 'douban-emacs)
(global-set-key (kbd "C-c d n") 'douban-create-note)

(require 'edit-server)
(edit-server-start)

(setq last-kbd-macro
   [?\C-e ?: ?3 ?: ?* ?: ?\C-\M-% ?\[ ?- ?| ?\\ ?  ?| ?\C-q ?\C-j ?\] return return ?! ?\C-x ?\C-s ?\C-a ?\C-k ?\C-z])
