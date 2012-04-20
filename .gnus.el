;; gnus
(ignore-errors
  (require 'gnus-load))

(require 'gnushush)
(setq gnushush-fqdn "unknown")

(eval-after-load "smtpmail"
  '(defun smtpmail-fqdn ()
     "unknown"))

;; stop annoying header update of article
(eval-after-load "gnus-art"
  (progn
    (setq gnus-article-update-date-headers nil)))

(setq user-full-name "Binjo"
      user-mail-address binjo-main-account)

(setq gnus-select-method '(nntp "nntp.aioe.org"
                                (nntp-open-connection-function network-only)))

(setq gnus-secondary-select-methods
      `((nntp "news.gmane.org"
              (nntp-address "news.gmane.org")
              (nnir-search-engine gmane)
              (nntp-open-connection-function network-only))

        ,@(mapcar '(lambda (x)
                     (let ((imap-lable (plist-get x :imap-labl))
                           (imap-address (plist-get x :imap-addr))
                           (imap-port (plist-get x :imap-port))
                           (imap-stream (plist-get x :imap-strm)))
                       `(nnimap ,imap-lable
                                (nnimap-address ,imap-address)
                                (nnimap-server-port ,imap-port)
                                (nnimap-stream ,imap-stream))))
                  binjo-private-gnus-imap-settings)))


;;;; Send/Fetch Mails/Messages

(setq mail-archive-file-name "~/.emacs.d/outgoing")

(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)
(setq mail-user-agent 'gnus-user-agent)

(defun binjo-sendmail-with-account (account pass server port)
  "Set proper variable with ACCOUNT name.

PASS smtp password.
SERVER smtp server.
PORT smtp service."
  (interactive)
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port)
  (setq smtpmail-auth-credentials `((,smtpmail-smtp-server
                                     ,smtpmail-smtp-service
                                     ,account
                                     ,pass)))
  (message "Will send mail with account : %s" account))

;;; default send with gmail account
(binjo-sendmail-with-account binjo-main-account
                             nil
                             "127.0.0.1"
                             4659)

;; (setq pop3-leave-mail-on-server t)
(setq mail-source-delete-incoming t)
(setq nnimap-split-inbox '("INBOX"))
;; to let gnus show group name like "[Gmail]" in server buffer
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)")

(defun binjo-mk-message-cite-style (&optional below-p)
  "Message citation style, if `BELOW-P' set as below."
  (interactive)
  `((message-cite-function  'message-cite-original-without-signature)
    (message-citation-line-function  'message-insert-formatted-citation-line)
    ,(if below-p
         `(message-cite-reply-position 'below)
       `(message-cite-reply-position 'above))
    (message-yank-prefix  ">")
    (message-yank-cited-prefix  ">")
    (message-yank-empty-prefix  ">")
    (message-citation-line-format "On %m/%d/%Y, at %R, %N wrote:\n")))

;; posting-style
(setq gnus-posting-styles
      `((".*"
         (name user-full-name)
         (address user-mail-address)
         (signature "Life is like a prison, can you break it?")
         (organization "Wh0 CaR3s")
         (eval
          ;; default cite style - bottom post
          (set (make-local-variable 'message-cite-style)
               (binjo-mk-message-cite-style t))))

        ("^cn\\.bbs\\.comp"
         (eval (set (make-local-variable 'mm-coding-system-priorities)
                    '(gb2312 utf-8))))

        ,@(mapcar '(lambda (x)
                     (let ((filter-name (plist-get x :imap-filter))
                           (my-name (plist-get x :imap-name))
                           (my-addr (plist-get x :imap-email))
                           (my-smtp-addr (plist-get x :imap-smtp-addr))
                           (my-smtp-port (plist-get x :imap-smtp-port))
                           (my-sig (plist-get x :imap-signature))
                           (my-org (plist-get x :imap-org)))
                       `(,filter-name
                         (name ,my-name)
                         (address ,my-addr)
                         (From (format "\"%s\" <%s>" ,my-name ,my-addr))
                         ,@(if my-sig
                               `((signature ,my-sig)))
                         ,@(if my-org
                               `((organization ,my-org)))
                         (eval
                          (progn
                            (binjo-sendmail-with-account ,my-addr
                                                         nil
                                                         ,my-smtp-addr
                                                         ,my-smtp-port)
                            ,@(if (plist-get x :imap-top-post-p)
                                  `((set (make-local-variable 'message-cite-style)
                                         (binjo-mk-message-cite-style)))))))))
                  binjo-private-gnus-imap-settings)
        ))

;; auto fill
(add-hook 'message-mode-hook
   (lambda ()
     (setq fill-column 80)
     (turn-on-auto-fill)))

;; framework
(gnus-add-configuration '(article
                          (vertical 1.0
                                    (summary .4 point)
                                    (article 1.0))))

;; http://www.emacswiki.org/emacs/TomRauchenwald
;; eye candy
(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %8{│%}                       %7{│%}%) %6{□%}  %S\n"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "■ "
      gnus-sum-thread-tree-false-root "□ "
      gnus-sum-thread-tree-single-indent "▣ "
      gnus-sum-thread-tree-leaf-with-other "├─▶ "
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-single-leaf "└─▶ ")

(setq gnus-summary-line-format "%8{%4L│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%8{│%}%9{%-23,23&user-date;%}%7{%*│%} %6{%B%} %s\n")

;; score
(setq gnus-use-adaptive-scoring t
      gnus-save-score t)

(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'gnus-startup-hook
          '(lambda ()
             (setq gnus-visible-headers
                   (concat "^User-Agent:\\|^Content-Type:\\|"
                           "Content-Transfer-Encoding:\\|"
                           "^X-mailer:\\|^X-Newsreader:\\|^X-Sender:\\|^Archived-At:\\|"
                           gnus-visible-headers))
             ;; banner alist for hiding
             (dolist (banner-item binjo-private-gnus-banner-alist)
               (let ((banner-name (intern (plist-get banner-item :name)))
                     (banner-regx (plist-get banner-item :regexp))
                     (banner-bner (plist-get banner-item :banner)))
                 (add-to-list 'gnus-parameter-banner-alist `(,banner-regx . ,banner-name))
                 (add-to-list 'gnus-article-banner-alist `(,banner-name . ,banner-bner))))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(defvar gnus-default-adaptive-score-alist
    '((gnus-kill-file-mark (from -10))
        (gnus-unread-mark)
        (gnus-read-mark (from 10) (subject 30))
        (gnus-catchup-mark (subject -10))
        (gnus-killed-mark (from -1) (subject -30))
        (gnus-del-mark (from -2) (subject -15))
        (gnus-ticked-mark (from 10))
        (gnus-dormant-mark (from 5))))

(setq  gnus-score-find-score-files-function
       '(gnus-score-find-hierarchical gnus-score-find-bnews)
       gnus-use-adaptive-scoring t)

;; (setq nnmail-split-methods 'nnmail-split-fancy)

;; sent copy
(setq gnus-message-archive-group
      '((if (message-news-p)
            "nnml:mail.sent.news"
          "nnml:mail.sent.mail")))

;; expire
;; (setq nnmail-expiry-wait-function
;;       (lambda (group)
;;         (cond ((string-match "avlab" group) 'never)
;;               ((string-match "mail\\.misc" group) 'never)
;;               ((string-match "staff" group) 'never)
;;               (t 7))))

;;;
(setq gnus-confirm-mail-reply-to-news t
      message-kill-buffer-on-exit t)

;; sort
(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)
        ))

;; Chinese related settings
(setq gnus-summary-show-article-charset-alist
      '((1 . cn-gb-2312) (2 . big5) (3 . gbk) (4 . utf-8)))

(setq gnus-default-charset 'cn-gb-2312
      gnus-group-name-charset-group-alist '((".*" . cn-gb-2312))
      gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown iso-8859-1 ISO-8859-15 x-gbk GB18030 gbk DEFAULT_CHARSET))

(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; save/copy some articles?
;;
;; - `B c': copy article to some group
;; - `*': put it in the cache, and use `Y c' to show it later
(setq gnus-use-cache 'passive)

;;; fuck mm-inline-wash-with-stdin
(when (eq system-type 'windows-nt)
  (progn
    (defadvice mm-inline-wash-with-stdin (around binjo-adv-set-lynx-param
                                                 (post-func cmd &rest args))
      "Set proper parameters before calling lynx."
      (if (string= "lynx" cmd)
          (setq args `("-cfg=c:/Lynx/lynx.cfg" "-lss=c:/Lynx/lynx.lss" ,@args)))
      ad-do-it)
    (eval-after-load "mm-view"
      '(ad-activate 'mm-inline-wash-with-stdin))))

;;;; Faces ;;;;
;;; http://www.stanford.edu/~rgm/comp/dotgnus.html

;; Group level faces.

(defface my-gnus-face-0 '((t (:foreground "aquamarine3" :weight bold)))
  "Gnus face for gnus-face-0 (default bold). Used for topics.")

(defface my-gnus-face-1 '((t (:foreground "aquamarine3" :weight normal)))
  "Gnus face for gnus-face-1.  Used for from.")

(defface my-gnus-face-2 '((t (:foreground "aquamarine1" :weight normal)))
  "Gnus face for gnus-face-2.  Used for date.")

(defface my-gnus-face-3 '((t (:foreground "turquoise" :weight normal)))
  "Gnus face for gnus-face-3.  Used for subject.")

(defface my-gnus-group-face-1 '((t (:foreground "LightBlue")))
  "Gnus face for group buffer highlighting.")

(defface my-gnus-group-face-2 '((t (:foreground "cyan")))
  "Gnus face for group buffer highlighting.")

(defface my-gnus-group-face-3 '((t (:foreground "MidnightBlue")))
  "Gnus face for group buffer highlighting.")

(defface my-gnus-group-face-4 '((t (:foreground "RoyalBlue1")))
  "Gnus face for group buffer highlighting.")

(defface my-gnus-group-face-5 '((t (:foreground "MediumSeaGreen")))
  "Gnus face for group buffer highlighting.")

(defface my-gnus-article-button-face
  '((t (:foreground "yellow" :weight bold)))
  "Gnus face for article buttons (urls, etc).")

;; Annoyingly, affects from: headers, etc.
;; Could adjust `gnus-header-button-alist' to remove most entries.
;;;(setq gnus-article-button-face 'my-gnus-article-button-face)


(defun my-gnus-article-prepare-hook-fn ()
  "Function added to `gnus-article-prepare-hook'."
  (require 'gnus-cite)                  ; NB why?
  ;; "X wrote:" font.
  (face-spec-set 'gnus-cite-attribution-face
                 '((t (:slant italic :family "helv"
                              :foreground "SkyBlue"))))
  (face-spec-set 'gnus-cite-face-1
                 '((((type x)) (:foreground "light blue"))
                   (t (:foreground "turquoise")))))

(add-hook 'gnus-article-prepare-hook 'my-gnus-article-prepare-hook-fn)


(defun my-gnus-article-mode-hook-fn ()
  "Function added to `gnus-article-mode-hook'."
  ;; Misc faces.
  (face-spec-set 'gnus-emphasis-bold-italic
                 '((t (:slant italic :family "helv" :weight bold))))

  (face-spec-set 'gnus-emphasis-italic
                 '((t (:slant italic :family "helv"))))

  (face-spec-set 'gnus-emphasis-strikethru '((t (:strike-through t))))

  (face-spec-set 'gnus-emphasis-underline-bold-italic
                 '((t (:slant italic :family "helv" :weight bold
                              :underline t))))

  (face-spec-set 'gnus-emphasis-underline-italic
                 '((t (:slant italic :family "helv" :underline t))))

  ;; Article header faces.
  (face-spec-set 'gnus-header-name-face '((t (:foreground "SeaGreen"))))

  (face-spec-set 'gnus-header-from-face '((t (:foreground "spring green"))))

  (face-spec-set 'gnus-header-newsgroups-face '((t (:foreground "yellow"))))

  (face-spec-set 'gnus-header-subject-face '((t (:foreground "SeaGreen3"))))

  (face-spec-set 'gnus-header-content-face '((t (:foreground "forest green"))))

  (face-spec-set 'gnus-signature-face '((t (:foreground "pink")))))

(add-hook 'gnus-article-mode-hook 'my-gnus-article-mode-hook-fn)


;; Summary faces.
(face-spec-set 'gnus-summary-high-unread-face
               '((t (:foreground "DarkSlateGray1" :weight bold))))

(face-spec-set 'gnus-summary-normal-unread-face
               '((t (:foreground "DarkSlateGray1")))) ; default wheat

(face-spec-set 'gnus-summary-low-unread-face
               '((t (:slant italic :family "helv"))))

(face-spec-set 'gnus-summary-high-read-face
               '((t (:foreground "PaleGreen" :weight bold))))

(face-spec-set 'gnus-summary-normal-read-face
               '((t (:foreground "PaleGreen"))))

;; Eg those marked down by scoring, so should be inconspicuous.
(face-spec-set 'gnus-summary-low-read-face
               '((t (:foreground "slate gray"))))

(face-spec-set 'gnus-summary-normal-ticked-face
               '((t (:foreground "Pink"))))

(face-spec-set 'gnus-summary-low-ticked-face
               '((t (:foreground "SkyBlue"))))

(face-spec-set 'gnus-summary-normal-ancient-face
               '((t (:foreground "SkyBlue"))))

(face-spec-set 'gnus-summary-low-ancient-face
               '((t (:foreground "SkyBlue"))))

(face-spec-set 'gnus-summary-high-ancient-face
               '((t (:foreground "SkyBlue"))))


;; Message faces.
(face-spec-set 'message-header-name-face
               '((t (:foreground "SeaGreen"))))

(face-spec-set 'message-header-to-face
               '((t (:foreground "yellow" :weight bold))))

(face-spec-set 'message-header-cc-face
               '((t (:foreground "yellow"))))

(face-spec-set 'message-header-subject-face
               '((t (:foreground "lawngreen"))))

(face-spec-set 'message-header-other-face
               '((t (:foreground "yellow"))))

(face-spec-set 'message-separator-face
               '((t (:foreground "pink"))))

(face-spec-set 'message-header-newsgroups-face
               '((t (:foreground "yellow" :weight bold))))

(face-spec-set 'message-cited-text-face
               '((t (:foreground "DarkSlateGray1" :weight bold))))
