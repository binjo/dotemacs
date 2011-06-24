;; gnus
(when (eq 0
        (string-match "No" (gnus-version)))
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
(setq gnus-secondary-select-methods `((nntp "news.gmane.org"
                                            (nntp-address "news.gmane.org")
                                            (nnir-search-engine gmane)
                                            (nntp-open-connection-function network-only))

                                      (nnimap ,binjo-imap-label1
                                              (nnimap-address "127.0.0.1")
                                              (nnimap-server-port 9939)
                                              (nnimap-stream network)
                                              )

                                      (nnimap ,binjo-imap-label2
                                              (nnimap-address "127.0.0.1")
                                              (nnimap-server-port 9940)
                                              (nnimap-stream network)
                                              )

                                      (nnimap ,binjo-imap-label3
                                              (nnimap-address "127.0.0.1")
                                              (nnimap-server-port 9557)
                                              (nnimap-stream network)
                                              )

                                      (nnimap ,binjo-imap-label4
                                              (nnimap-address ,binjo-comp-server)
                                              (nnimap-server-port 143)
                                              (nnimap-stream network)
                                              )
))


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

        (,binjo-label1-filter
         (eval
          (binjo-sendmail-with-account ,binjo-main-account
                                       nil
                                       "127.0.0.1"
                                       4659)))

        (,binjo-label2-filter
         (name ,binjo-fake-name)
         (address ,binjo-fake-account)
         (From (format "\"%s\" <%s>" ,binjo-fake-name ,binjo-fake-account))
         (eval
          (binjo-sendmail-with-account ,binjo-fake-account
                                       nil
                                       "127.0.0.1"
                                       4659)))

        (,binjo-label3-filter
         (name "Binjo")
         (address ,binjo-0x557-account)
         (From (format "\"%s\" <%s>" "Binjo" ,binjo-0x557-account))
         (eval
          (binjo-sendmail-with-account ,binjo-0x557-account
                                       nil
                                       "127.0.0.1"
                                       4659)))

        (,binjo-label4-filter
         (name ,binjo-comp-name)
         (address ,binjo-comp-account)
         (From (format "\"%s\" <%s>" ,binjo-comp-name ,binjo-comp-account))
         (signature ,binjo-private-mail-sig)
         (organization "Anchiva System Inc.")
         (eval
          (progn
            (binjo-sendmail-with-account ,binjo-comp-account
                                         ,binjo-comp-pass
                                         ,binjo-comp-server
                                         25)
            ;; top post
            (set (make-local-variable 'message-cite-style)
                 (binjo-mk-message-cite-style)))))))

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

(setq gnus-summary-line-format ":%U%R %B %s %-60=| %5L |%-10,8f |%&user-date; \n")

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
                           gnus-visible-headers))))

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

;; (setq nnimap-split-fancy
;;       `(|
;;         (from ,(regexp-opt binjo-private-avlab-group)
;;               "avlab")
;;         (from ,(regexp-opt binjo-private-staff-group)
;;               "staff")
;;         (from ,(regexp-opt
;;                 '("bugzilla-daemon@"))
;;               "bugzilla")
;;         (to ,binjo-private-avlab-account
;;             (| ("from" ,(regexp-opt binjo-private-not-spam-group)
;;                 "avlab")
;;                "spam"))
;;         (to ,binjo-private-psm-group
;;             "psm")
;;         "misc"))

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
      message-kill-buffer-on-exit t
      message-elide-ellipsis "[...]\n"
      )

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
(defadvice mm-inline-wash-with-stdin (around binjo-adv-set-lynx-param
                                             (post-func cmd &rest args))
  "Set proper parameters before calling lynx."
  (if (string= "lynx" cmd)
      (setq args `("-cfg=c:/Lynx/lynx.cfg" "-lss=c:/Lynx/lynx.lss" ,@args)))
  ad-do-it)
(eval-after-load "mm-view"
  '(ad-activate 'mm-inline-wash-with-stdin))
