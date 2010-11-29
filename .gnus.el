;; gnus
(when (eq 0
        (string-match "No" (gnus-version)))
    (require 'gnus-load))

(require 'gnushush)
(setq gnushush-fqdn "unknown")

(eval-after-load "smtpmail"
  '(defun smtpmail-fqdn ()
     "unknown"))

(setq user-full-name "Binjo"
      user-mail-address binjo-main-account)

(setq gnus-select-method '(nntp "news.cn99.com"))
(setq gnus-secondary-select-methods `((nntp "news.gmane.org")
;;                                      (nnfolder "")
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
                                      (nnml ,binjo-comp-server)
))

(setq mail-sources
      `((pop :server ,binjo-comp-server
             :user ,binjo-comp-user
             :password ,binjo-comp-pass)

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

(setq pop3-leave-mail-on-server t)
(setq mail-source-delete-incoming t)
(setq nnimap-split-inbox '("INBOX"))
;; to let gnus show group name like "[Gmail]" in server buffer
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)")

;; posting-style
(setq gnus-posting-styles
      `((".*"
         (name user-full-name)
         (address user-mail-address)
         (signature "Life is like a prison, can you break it?")
         (organization "Wh0 CaR3s"))

        ("^cn\\.bbs\\.comp"
         (eval (setq mm-coding-system-priorities
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
        ("nnml.*"
         (name ,binjo-comp-name)
         (address ,binjo-comp-account)
         (From (format "\"%s\" <%s>" ,binjo-comp-name ,binjo-comp-account))
         (signature ,binjo-private-mail-sig)
         (organization "Anchiva System Inc.")
         (eval
          (binjo-sendmail-with-account ,binjo-comp-account
                                       ,binjo-comp-pass
                                       ,binjo-comp-server
                                       25)))))

;; auto fill
(add-hook 'message-mode-hook
   (lambda ()
     (setq fill-column 80)
     (turn-on-auto-fill)))

;; don't quote the signature
(setq message-cite-function 'message-cite-original-without-signature)

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
       '(gnus-score-find-hierarchical gnus-score-find-bnews bbdb/gnus-score)
       gnus-use-adaptive-scoring t)

;; split
(setq nnmail-split-fancy-match-partial-words t)

(setq nnmail-split-fancy
      `(|
        (from ,(regexp-opt binjo-private-avlab-group)
              "mail.avlab")
        (from ,(regexp-opt binjo-private-staff-group)
              "mail.staff")
        (from ,(regexp-opt
                '("bugzilla-daemon@"))
              "mail.bugzilla")
        (to ,binjo-private-avlab-account
            (| ("from" ,(regexp-opt binjo-private-not-spam-group)
                "mail.avlab")
               "mail.spam"))
        "mail.misc"))

(setq nnmail-split-methods 'nnmail-split-fancy)

;; sent copy
(setq gnus-message-archive-group
      '((if (message-news-p)
            "nnml:mail.sent.news"
          "nnml:mail.sent.mail")))

;; expire
(setq nnmail-expiry-wait-function
      (lambda (group)
        (cond ((string-match "avlab" group) 'never)
              ((string-match "mail\\.misc" group) 'never)
              ((string-match "staff" group) 'never)
              (t 7))))
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

;; bbdb
(require 'bbdb)
(bbdb-initialize 'gnus 'message)

(setq bbdb-north-american-phone-numbers-p nil)

(setq bbdb-user-mail-names
      (regexp-opt `(,binjo-main-account
                    ,binjo-fake-account
                    ,binjo-comp-account)))
(setq bbdb-complete-name-allow-cycling t         ;; cycle
      bbdb-use-pop-up nil                        ;; No popup-buffers
      bbdb-dwim-net-address-allow-redundancy t)  ;; include name
(eval-after-load "message"
  '(define-key message-mode-map (kbd "<backtab>" ) 'bbdb-complete-name))

;;; fuck pop3-movemail
;;; FIXME or get the latest index from "~/Mail/active"  ??
(defun binjo-pop3-latest-index (dir-name)
  "Get latest index to be fetched from DIR-NAME."
  (let ((n 1)
        file-name)
    (mapc (lambda (sub-dir-name)
            (unless (or (string= "." sub-dir-name)
                        (string= ".." sub-dir-name))
              (setq file-name (concat dir-name sub-dir-name "/.overview"))
              (if (file-exists-p file-name)
                  (with-temp-buffer
                    (insert-file-literally file-name)
                    (goto-char (point-max))
                    (forward-line -1)
                    (setq n (+ n (string-to-number
                                  (car (split-string
                                        (thing-at-point 'line))))))))))
          (directory-files dir-name))
    n))

;; TODO stick to the latest pop3-movemail, 20101015
(eval-after-load "pop3"
  '(defun pop3-movemail (&optional crashbox)
     "Transfer contents of a maildrop to the specified CRASHBOX.

A dirty hack to let `pop3-movemail' fetch newest mail if '.overview' exists."
     (or crashbox (setq crashbox (expand-file-name "~/.crashbox")))
     (let* ((process (pop3-open-server pop3-mailhost pop3-port))
            (crashbuf (get-buffer-create " *pop3-retr*"))
            (n 1)
            message-count
            (pop3-password pop3-password))
       (if (file-exists-p "~/Mail/mail/")
           (setq n (binjo-pop3-latest-index "~/Mail/mail/")))
       ;; for debugging only
       (if pop3-debug (switch-to-buffer (process-buffer process)))
       ;; query for password
       (if (and pop3-password-required (not pop3-password))
           (setq pop3-password
                 (read-passwd (format "Password for %s: " pop3-maildrop))))
       (cond ((equal 'apop pop3-authentication-scheme)
              (pop3-apop process pop3-maildrop))
             ((equal 'pass pop3-authentication-scheme)
              (pop3-user process pop3-maildrop)
              (pop3-pass process))
             (t (error "Invalid POP3 authentication scheme")))
       (setq message-count (car (pop3-stat process)))
       (unwind-protect
           (while (<= n message-count)
             (message "Retrieving message %d of %d from %s..."
                      n message-count pop3-mailhost)
             (pop3-retr process n crashbuf)
             (save-excursion
               (set-buffer crashbuf)
               (let ((coding-system-for-write 'binary))
                 (write-region (point-min) (point-max) crashbox t 'nomesg))
               (set-buffer (process-buffer process))
               (while (> (buffer-size) 5000)
                 (goto-char (point-min))
                 (forward-line 50)
                 (delete-region (point-min) (point))))
             (unless pop3-leave-mail-on-server
               (pop3-dele process n))
             (setq n (+ 1 n))
             (pop3-accept-process-output process))
         (when (and pop3-leave-mail-on-server
                    (> n 1))
;;            (message "pop3.el doesn't support UIDL.  Setting `pop3-leave-mail-on-server'
;; to %s might not give the result you'd expect." pop3-leave-mail-on-server)
           (sit-for 1))
         (pop3-quit process))
       (kill-buffer crashbuf))
     t)
)

;;; fuck mm-inline-wash-with-stdin
(defadvice mm-inline-wash-with-stdin (around binjo-adv-set-lynx-param
                                             (post-func cmd &rest args))
  "Set proper parameters before calling lynx."
  (if (string= "lynx" cmd)
      (setq args `("-cfg=c:/Lynx/lynx.cfg" "-lss=c:/Lynx/lynx.lss" ,@args)))
  ad-do-it)
(eval-after-load "mm-view"
  '(ad-activate 'mm-inline-wash-with-stdin))
