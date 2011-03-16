;;; kmacro-ring-list.el --- List keyboard macros in a buffer

;; Copyright (C) 2010  Leo

;; Author: Leo
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In the course of editing text in Emacs a handful of keyboard macros
;; (kmacros) may be gathered for some repetitive but often trivial
;; tasks. Incidentally they are not significant enough to deserve a
;; name and it is best to leave them in kmacro ring. However for the
;; time being, the facility provided by Emacs is not intuitive even
;; for a small number of kmacros. This little package hopefully will
;; make such a task straightforward by displaying the list of kmacros
;; nicely in a buffer as well as offering key bindings to manipulate
;; them. The most useful function, in my view, is hitting RET to set
;; the kmacro at point to be the current one. The main entry function
;; is `kmacro-ring-list' and you may bind it to a key as in this
;; example:
;;
;;    (global-set-key (kbd "<f5>") 'kmacro-ring-list)
;;
;; Comments and bug fixes are welcome.

;;; Code:

(require 'ewoc)
(require 'kmacro)
(require 'pp)
(eval-when-compile (require 'cl))

(defface kmacro-ring-list-macro-face
  '((((background light))
     :background "wheat")
    (((background dark))
     :background "gray30"))
  "Face for displaying keyboard macros."
  :group 'kmacro)

(defvar kmacro-ring-list-ewoc nil
  "Internal variable that holds the ewoc structure associated
with kmacro ring.")
(make-variable-buffer-local 'kmacro-ring-list-ewoc)

;;;; -------- utility --------
;;; macros to begin with % to reduce the chance of conflict
(defmacro* %with-ewoc-node ((var ewoc) &body body)
  "Bind VAR to the EWOC node at point. When VAR is non-nil, execute BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (ewoc-locate ,ewoc)))
     (when ,var ,@body)))

(defun kmacro-ring-list-head-p ()
  "Return t if point is at the head of the kmacro ring."
  (%with-ewoc-node (node kmacro-ring-list-ewoc)
    (eql node (ewoc-nth kmacro-ring-list-ewoc 0))))

(defmacro* kmacro-ring-list-ensure-head (&body body)
  "Execute BODY only if at the head of kmacro ring."
  (declare (indent 0) (debug t))
  `(if (kmacro-ring-list-head-p)
       (progn ,@body)
     (message "Command only allowed at the head of the kmacro ring.")))

;;;; -------- navigation --------
(defsubst kmacro-ring-list-prev (arg)
  (interactive "p")
  (ewoc-goto-prev kmacro-ring-list-ewoc arg))

(defsubst kmacro-ring-list-next (arg)
  (interactive "p")
  (ewoc-goto-next kmacro-ring-list-ewoc arg))

(defun kmacro-ring-list-quit ()
  (interactive)
  (kill-buffer (ewoc-buffer kmacro-ring-list-ewoc))
  (unless (one-window-p) (delete-window)))

(defun kmacro-ring-list-help ()
  (interactive)
  (describe-function 'kmacro-ring-list))

;;;; -------- actions --------
;;; c.f. `kmacro-cycle-ring-next'
;;; FIXME what's the meaning of repeat-prefix?
(defun kmacro-ring-list-set-current ()
  "Set the kmacro under point current."
  (interactive)
  (if (kmacro-ring-list-head-p)
      (message "Macro at point is already the head.")
    (%with-ewoc-node (node kmacro-ring-list-ewoc)
      (unless (kmacro-ring-empty-p)
        (kmacro-push-ring)
        (let* ((data (ewoc-data node))
               (keys (kmacro-get-repeat-prefix)))
          (kmacro-split-ring-element data)
          (setq kmacro-ring (delete data kmacro-ring))
          (kmacro-display last-kbd-macro t)
          (when keys
            (kmacro-repeat-on-last-key keys)))))
    (kmacro-ring-list)))

(defun kmacro-ring-list-preview ()
  "Open a buffer and insert some random text; then execute the
macro at point 3 times if no error occurs."
  (interactive)
  (%with-ewoc-node (node kmacro-ring-list-ewoc)
    (let ((buf (get-buffer-create "*Keyboard Macro Preview*")))
      ;; FIXME: why `with-current-buffer' doesn't work here?
      (with-selected-window (display-buffer buf)
        (highlight-changes-mode -1)     ; turn off
        (erase-buffer)
        (local-set-key "\C-c\C-k" 'kill-this-buffer)
        (insert "Type `C-c C-k' to kill this buffer.\n\n")
        (loop repeat 8
              for i from 1
              with pt
              do (spook)
              ;; save the position halfway
              (when (= i 4) (setq pt (point)))
              finally (goto-char pt))
        (highlight-changes-mode 1)      ; turn on
        ;; calling the macro three times or until there's an error
        (dotimes (k 3)
          (condition-case condition
              (execute-kbd-macro (car (ewoc-data node)))
            (error (message "%s" condition)
                   (return)))
          (sit-for 0.25))))))

(defun kmacro-ring-list-incf-counter (arg)
  "Increase the counter by ARG."
  (interactive "p")
  (%with-ewoc-node (node kmacro-ring-list-ewoc)
    (incf (cadr (ewoc-data node)) arg)
    (when (kmacro-ring-list-head-p)
      (incf kmacro-counter arg))
    (ewoc-invalidate kmacro-ring-list-ewoc node)))

(defun kmacro-ring-list-decf-counter (arg)
  "Decrease the counter by ARG."
  (interactive "p")
  (kmacro-ring-list-incf-counter (- arg)))

(defun kmacro-ring-list-set-counter (arg)
  "Set the counter to ARG."
  (interactive "nSet the counter to: ")
  (%with-ewoc-node (node kmacro-ring-list-ewoc)
    (setf (cadr (ewoc-data node)) arg)
    (when (kmacro-ring-list-head-p)
      (setf kmacro-counter arg))
    (ewoc-invalidate kmacro-ring-list-ewoc node)))

(defun kmacro-ring-list-set-format (fmt)
  "Set the counter format to FMT."
  (interactive "sSet counter format to: ")
  (unless (string= fmt "")
    (%with-ewoc-node (node kmacro-ring-list-ewoc)
      (setf (car (last (ewoc-data node))) fmt)
      (when (kmacro-ring-list-head-p)
        (setf kmacro-counter-format fmt))
      (ewoc-invalidate kmacro-ring-list-ewoc node))))

(defun kmacro-ring-list-edit-head (arg)
  "Edit the keyboard macro at the head of the kmacro ring."
  (interactive "P")
  (kmacro-ring-list-ensure-head
    (edit-kbd-macro
     "\r" arg
     ;; this hook runs at the beginning and end of the function
     ;; `edmacro-finish-edit'
     (lambda ()
       ;; run `kmacro-ring-list' only after exit the edmacro buffer.
       (unless (eq major-mode 'edmacro-mode)
         (kmacro-ring-list))))))

(defun kmacro-ring-list-save-head (name)
  "Save the macro at the head of the kmacro ring."
  (interactive "SName for this macro: ")
  (kmacro-ring-list-ensure-head
    (kmacro-name-last-macro name)
    (with-temp-buffer
      (insert-kbd-macro name)
      (pp-display-expression (pp-last-sexp) "*Named keyboard macros*"))))

(defun kmacro-ring-list-delete-head (arg)
  "Delete the keyboard macro at the head of the kmacro ring."
  (interactive "P")
  (kmacro-ring-list-ensure-head
    (when (yes-or-no-p "Delete the macro? ")
      (kmacro-delete-ring-head arg)
      (kmacro-ring-list))))

;;;; -------- key bindings --------
(defvar kmacro-ring-list-map (make-sparse-keymap)
  "Keymap for `kmacro-ring-list'.")
(define-key kmacro-ring-list-map "C" 'kmacro-ring-list-set-counter)
(define-key kmacro-ring-list-map "F" 'kmacro-ring-list-set-format)
(define-key kmacro-ring-list-map "d" 'kmacro-ring-list-delete-head)
(define-key kmacro-ring-list-map "e" 'kmacro-ring-list-edit-head)
(define-key kmacro-ring-list-map "s" 'kmacro-ring-list-save-head)
(define-key kmacro-ring-list-map "g" 'kmacro-ring-list)
(define-key kmacro-ring-list-map "n" 'kmacro-ring-list-next)
(define-key kmacro-ring-list-map "p" 'kmacro-ring-list-prev)
(define-key kmacro-ring-list-map "q" 'kmacro-ring-list-quit)
(define-key kmacro-ring-list-map "v" 'kmacro-ring-list-preview)
(define-key kmacro-ring-list-map "=" 'kmacro-ring-list-incf-counter)
(define-key kmacro-ring-list-map "+" 'kmacro-ring-list-incf-counter)
(define-key kmacro-ring-list-map "-" 'kmacro-ring-list-decf-counter)
(define-key kmacro-ring-list-map "?" 'kmacro-ring-list-help)
(define-key kmacro-ring-list-map (kbd "RET") 'kmacro-ring-list-set-current)

(defun kmacro-ring-list-pp (entry)
  (insert (propertize (format-kbd-macro (car entry))
                      'face 'kmacro-ring-list-macro-face) "\n")
  (insert "Counter: " (number-to-string (cadr entry)) "\n")
  (insert "Format: " (caddr entry) "\n")
  (insert "----------------\n"))

(defsubst kmacro-ring-list-length ()
  "Return the length of the kmacro ring."
  (+ (length kmacro-ring)
     (if last-kbd-macro 1 0)))

;;;###autoload
(defun kmacro-ring-list ()
  "Display a list of keyboard macros.

\\{kmacro-ring-list-map}"
  (interactive)
  (pop-to-buffer (get-buffer-create "*kmacro-ring-list*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (kill-all-local-variables)
  (let ((header (format "%d macro(s) on the ring\n================\n\n"
                        (kmacro-ring-list-length))))
   (setq kmacro-ring-list-ewoc
         (ewoc-create 'kmacro-ring-list-pp header nil 'nosep)))
  ;; enter the head of the kmacro ring, which is NOT stored in
  ;; kmacro-ring
  (when last-kbd-macro
    (ewoc-enter-last kmacro-ring-list-ewoc
                     ;; this is not the same as `kmacro-ring-head'
                     (list last-kbd-macro
                           kmacro-counter
                           kmacro-counter-format)))
  (dolist (entry kmacro-ring)
    (ewoc-enter-last kmacro-ring-list-ewoc entry))
  (use-local-map kmacro-ring-list-map)
  (setq buffer-read-only t)
  ;; place the point at the head
  (kmacro-ring-list-next 0))

(provide 'kmacro-ring-list)
;;; kmacro-ring-list.el ends here
