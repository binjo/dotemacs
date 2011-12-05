;;;;  -*- mode: emacs-lisp; coding: utf-8-unix -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;;;
;;;; Author: Paul Sexton <eeeickythump@gmail.com>
;;;; Version: 0.1
;;;; Repository at http://bitbucket.org/eeeickythump/slothful/
;;;;
;;;;
;;;; Synopsis
;;;; ========
;;;;
;;;; Extension for GNU Emacs that allows truly lazy loading of files.
;;;;
;;;; Can be used in two ways:
;;;;
;;;; 1. `M-x slothful-find-file' -- works like `find-file' but instead of
;;;;    reading the file contents into a buffer at the time it is called,
;;;;    creates an empty buffer pointing to the specified file. The contents
;;;;    of the file will not be loaded into the buffer until *required*,
;;;;    i.e., until the file is displayed.
;;;; 2. When the global variable `finding-files-slothfully-p' is bound to
;;;;    a non-nil value, all calls to the low level function
;;;;    `find-file-noselect' will instead call its slothful equivalent.
;;;;    In other words, all files that would normally have been read into
;;;;    buffers will instead have slothful buffers created, as described
;;;;    above.
;;;;
;;;; The end user should use `M-x slothful-find-file' to slothfully
;;;; load one or many files into background buffers (the command accepts
;;;; wildcards.)
;;;;
;;;; Emacs Lisp programmers should rebind `finding-files-slothfully-p'
;;;; using a `let' statement -- all files read into buffers within the
;;;; scope of the statement will be read slothfully.
;;;;
;;;; This code comes with absolutely no warranty of any kind.


(defvar slothful-buffer-p nil
  "NEVER globally set this to a non-nil value.")


(defvar finding-files-slothfully-p nil
  "Intended to be dynamically rebound when we want to make find-file work
in a slothful manner.")


(defun slothful-buffer-p (&optional buf)
  "Returns true if the buffer object BUF is a slothful buffer, i.e. the
contents of its file have not actually been loaded yet."
  (buffer-local-value 'slothful-buffer-p (or buf (current-buffer))))


(defun slothful-find-file (filename &optional wildcards)
  "Slothful version of FIND-FILE. Instead of actually reading the
contents of FILENAME into a buffer, an empty `slothful' buffer is
created which is associated with FILENAME. This buffer is created
in the background. The buffer will only be filled with the contents
of FILENAME when you actually switch to the buffer.

If WILCARDS is non-nil, do wildcard processing and create buffers for all the
matching files."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (if (and (called-interactively-p)
           (string-match "[[*?]" filename))
      (setq wildcards t))
  (slothful-find-file-noselect filename nil nil wildcards))


(defun slothful-find-file-noselect (filename
                                    &optional nowarn rawfile wildcards)
  "Slothful version of FIND-FILE-NOSELECT. If there is already a buffer
that is visiting FILENAME, return that. Otherwise, create a background buffer
that is visiting FILENAME, but do not actually read the contents of
FILENAME into the buffer. Return the new buffer object.

If WILCARDS is non-nil, do wildcard processing and create buffers for all the
matching files.

NOWARN and RAWFILE arguments are accepted for compatibility with
`find-file-noselect', but are ignored."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (cond
   ((and wildcards
         find-file-wildcards
         (not (string-match "\\`/:" filename))
         (string-match "[[*?]" filename))
    (let ((files (condition-case nil
                     (file-expand-wildcards filename t)
                   (error (list filename))))
          (find-file-wildcards nil))
      (if (null files)
          (slothful-find-file-noselect filename)
        (mapcar 'slothful-find-file-noselect files))))
   (t
    (let ((buf (find-buffer-visiting filename)))
      (unless buf
        (setq buf (create-file-buffer filename))
        (with-current-buffer buf
          (set-visited-file-name filename t)
          (set-buffer-modified-p nil)
          (set (make-local-variable 'slothful-buffer-p) t)))
      buf))))


(defadvice switch-to-buffer (before load-slothful-before-buffer-displayed
                                    (buffer-or-name
                                     &optional norecord force-same-window))
  "Load slothful buffers before they are displayed."
  (when buffer-or-name
    (let ((buf (get-buffer buffer-or-name)))
      (when (slothful-buffer-p buf)
        (with-current-buffer buf
          (unless (file-exists-p buffer-file-name)
            (error "The file `%s' associated with slothful buffer `%s' %s"
                   buffer-file-name (buffer-name) "does not exist."))
          (setq slothful-buffer-p nil)
          (message "Reading contents of slothful buffer `%s' from disk..."
                   (buffer-name buf))
          (revert-buffer nil t))))))

(ad-activate 'switch-to-buffer)


(defadvice find-file-noselect (around maybe-find-file-slothfully
                                      (filename &optional nowarn rawfile
                                                wildcards))
  "If finding-files-slothfully-p is true, divert this call from
find-file-noselect to slothful-find-file, so that an empty 'slothful'
buffer is created. Otherwise, proceed as normal."
  (if (and finding-files-slothfully-p
           (null wildcards))
      (slothful-find-file-noselect filename nowarn rawfile wildcards)
    ;; Else continue
    ad-do-it))

(ad-activate 'find-file-noselect)

(provide 'slothful)


;; This seems not to be needed?
;;
;; (defadvice display-buffer (before load-slothful-before-buffer-displayed
;;                                   (buffer-or-name
;;                                    &optional not-this-window frame))
;;   "Load slothful buffers before they are displayed."
;;   (let ((buf (get-buffer buffer-or-name)))
;;     (when (slothful-buffer-p buf)
;;       (with-current-buffer buf
;;         (setq slothful-buffer-p nil)
;;         (revert-buffer nil t)))))
;;
;;(ad-activate 'display-buffer)
