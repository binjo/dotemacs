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
(global-set-key (kbd "C-x r u") 'binjo-unify-line)

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

;; sql-config
(binjo-m-global-set-key-dynamic 'binjo-sql
                                ((kbd "C-c s w") . 'sql-webfilter)
                                ((kbd "C-c s f") . 'sql-fips)
                                ((kbd "C-c s a") . 'sql-av-feedback)
                                ((kbd "C-c s m") . 'sql-av-webmon))

;; org-mode
(add-hook 'window-setup-hook '(lambda ()
                                (require 'binjo-org)))

;; twit.el
(add-hook 'window-setup-hook '(lambda ()
                                (require 'binjo-twit)))

;; smex
(binjo-m-global-set-key-dynamic 'smex
                                ((kbd "M-x") . 'smex)
                                ((kbd "M-X") . 'smex-major-mode-commands)
                                ((kbd "C-c M-x") . 'smex-update-and-run))
(eval-after-load 'smex
  '(smex-initialize))
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
(add-hook 'window-setup-hook '(lambda ()
                                (require 'yasnippet)
                                (yas/initialize)
                                (yas/load-directory "~/.emacs.d/snippets")))

;;; anchiva
(binjo-m-global-set-key-dynamic 'anchiva
                                ((kbd "C-c v u") . 'anchiva-submit-sample)
                                ((kbd "C-c v t") . 'anchiva-check-mwcs-test)
                                ((kbd "C-c v m") . 'anchiva-submit-sig-mwcs)
                                ((kbd "C-c v b") . 'anchiva-browse-all-urls)
                                ((kbd "C-c v s") . 'anchiva-pop-sig-buffer))

;; babel, use online api to translate
(binjo-m-global-set-key-dynamic 'babel
                                ((kbd "C-c t r") . 'babel-region)
                                ((kbd "C-c t b") . 'babel))
(eval-after-load 'babel
  '(setq babel-preferred-to-language "Chinese (Simplified)"))

;; emms
(add-hook 'window-setup-hook '(lambda ()
                                (require 'binjo-emms)))

;; douban
(binjo-m-global-set-key-dynamic 'douban-emacs
                                ((kbd "C-c d n") . 'douban-create-note))

(if binjo-at-company-p
    (progn
      (require 'edit-server)
      (edit-server-start)))

(binjo-m-global-set-key-dynamic 'binjo-magit
                                ((kbd "C-c g m") . 'magit-status)
                                ((kbd "C-c m c") . 'magit-clone))

(binjo-m-global-set-key-dynamic 'kmacro-ring-list
                                ((kbd "C-c m r") . 'kmacro-ring-list))

(binjo-m-global-set-key-dynamic 'binjo-calendar
  ((kbd "<f11>") . (lambda ()
                     (interactive)
                     (let ((cal "*Calendar*"))
                       (if (get-buffer cal)
                           (progn
                             (split-window-vertically -9)
                             (other-window 1))
                         ;; ���ô�ֱ�ָ
                         (let ((split-width-threshold 9999))
                           (calendar)))
                       (switch-to-buffer cal)
                       (calendar-cursor-holidays)))))

;; ioccur
(binjo-m-global-set-key-dynamic 'ioccur
  ((kbd "C-c o c") . 'ioccur))
(eval-after-load 'ioccur
  '(progn
     (set-face-background 'ioccur-match-overlay-face "SkyBlue")
     (set-face-background 'ioccur-title-face "MediumOrchid")

     (define-key ioccur-mode-map (kbd "j") 'ioccur-scroll-down)
     (define-key ioccur-mode-map (kbd "k") 'ioccur-scroll-up)

     (add-to-list 'desktop-globals-to-save 'ioccur-history)))