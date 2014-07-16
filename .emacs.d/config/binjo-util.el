;; macro
(defmacro binjo-m-global-set-key-dynamic (package &rest key-funcs)
  "Call `global-set-key' to set key and functions with KEY-FUNCS,
and `require' PACKAGE dynamically."
  (declare (indent 1))
  (cons 'progn
        (mapcar (lambda (key-func)
                `(global-set-key ,(car key-func) '(lambda ()
                                                    (interactive)
                                                    (require ,package)
                                                    (call-interactively ,(cdr key-func)))))
              key-funcs)))

;; eshell
(eval-after-load 'esh-mode
  '(progn
     (require 'binjo-eshell-util)))

(defun binjo-unify-line (begin end)
  "wipe out the same line of specified region"
  (interactive "r")
  (goto-char begin)
  (let ((line "")
        (unified ()))
    (save-excursion
      (while (not (eobp))
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
(global-set-key (kbd "C-c r u") 'binjo-unify-line)

(defun binjo-copy-line (&optional arg)
  "copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (clipboard-kill-ring-save (line-beginning-position)
                  (line-end-position (+ 0 arg)))
  (message "%d line%scopied" arg (if (= 1 arg) " " "s ")))
(global-set-key (kbd "C-c h") 'binjo-copy-line)

;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;; erc
(binjo-m-global-set-key-dynamic 'binjo-erc
                                ((kbd "C-c n e") . 'binjo-erc-select))

;; ;; sql-config
;; (binjo-m-global-set-key-dynamic 'binjo-sql
;;                                 ((kbd "C-c s w") . 'sql-webfilter)
;;                                 ((kbd "C-c s f") . 'sql-fips)
;;                                 ((kbd "C-c s a") . 'sql-av-feedback)
;;                                 ((kbd "C-c s m") . 'sql-av-webmon))

;; org-mode
(add-hook 'window-setup-hook '(lambda ()
                                (require 'binjo-org)))

;; boxquote
(binjo-m-global-set-key-dynamic 'boxquote
                                ((kbd "C-c b r") . 'boxquote-region)
                                ((kbd "C-c b t") . 'boxquote-title)
                                ((kbd "C-c b f") . 'boxquote-describe-function)
                                ((kbd "C-c b v") . 'boxquote-describe-variable)
                                ((kbd "C-c b k") . 'boxquote-describe-key)
                                ((kbd "C-c b !") . 'boxquote-shell-command)
                                ((kbd "C-c b y") . 'boxquote-yank))

;; YASnippet
;; (add-hook 'window-setup-hook '(lambda ()
;;                                 (require 'yasnippet)
;;                                 (setq yas/snippet-dirs "~/.emacs.d/snippets")
;;                                 (yas/initialize)
;;                                 (set-default 'yas/dont-activate
;;                                              #'(lambda ()
;;                                                  (and yas/root-directory
;;                                                       (null (yas/get-snippet-tables)))))))

;; babel, use online api to translate
(binjo-m-global-set-key-dynamic 'babel
                                ((kbd "C-c t r") . 'babel-region)
                                ((kbd "C-c t b") . 'babel))
(eval-after-load 'babel
  '(setq babel-preferred-to-language "Chinese (Simplified)"))

;; (binjo-m-global-set-key-dynamic 'kmacro-ring-list
;;                                 ((kbd "C-c m r") . 'kmacro-ring-list))

(binjo-m-global-set-key-dynamic 'binjo-calendar
  ((kbd "C-x C") . (lambda ()
                     (interactive)
                     (let ((cal "*Calendar*"))
                       (if (get-buffer cal)
                           (progn
                             (split-window-vertically -9)
                             (other-window 1))
                         (let ((split-width-threshold 9999))
                           (calendar)))
                       (switch-to-buffer cal)
                       (calendar-cursor-holidays)))))

;; ;; jabber
;; (binjo-m-global-set-key-dynamic 'binjo-jabber
;;   ((kbd "C-c j j") . 'jabber)
;;   ;; C-x C-j C-l is tedious
;;   ((kbd "C-c j x") . 'jabber-activity-switch-to))

;; putty
(when (eq system-type 'windows-nt)
  (binjo-m-global-set-key-dynamic 'binjo-putty
    ((kbd "C-c p t") . 'binjo-start-putty)
    ((kbd "C-c p c") . 'binjo-start-pscp)))

;; keep history of `async-shell-command'
;; TODO new file?
(defvar binjo-async-shell-cmd-history '()
  "Histtory yof `async-shell-command'.")

(defun binjo-async-shell-cmd ()
  "Save cmd history, prompt via `ido-completing-read'."
  (interactive)
  (let ((cmd (ido-completing-read "cmd "
                                  binjo-async-shell-cmd-history)))
    (add-to-list 'binjo-async-shell-cmd-history cmd)
    ;; FIXME what about the optional params?
    (async-shell-command cmd)))

(global-set-key (kbd "M-&") 'binjo-async-shell-cmd)

;; ;; anything - helm
;; (binjo-m-global-set-key-dynamic 'helm-config
;;   ((kbd "C-c i") . 'helm-mini))

(provide 'binjo-util)
