;;; binjo-arc.el --- redefuns for arc-mode

;; Copyright 2009 Binjo
;;
;; Author: Binjo <binjo.cn@gmail.com>
;; Version: $Id: binjo-arc.el,v 0.0 2009/12/04 08:43:02 binjo Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Redefine some functions to let `archive-rar-summarize' work properly.

;;; History:

;; 12/04/2009, init release

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-arc)

;;; Code:

(eval-when-compile
  (require 'cl))


(eval-after-load "arc-mode"
  '(defun archive-rar-summarize (&optional file)
     "Substitute 'unrar-free' with '7z' to get summarize."
     ;; File is used internally for `archive-rar-exe-summarize'.
     (unless file (setq file buffer-file-name))
     (let* ((copy (file-local-copy file))
            (maxname 10)
            (maxsize 5)
            (files ()))
       (with-temp-buffer
         (call-process "7z" nil t nil "l" (or file copy))
         (if copy (delete-file copy))
         (goto-char (point-min))
         (re-search-forward "^-+.*\n")
         (while (looking-at (concat  "\\([0-9\\-]+\\)" ;Date
                                     " +\\([0-9:]+\\)" ;Time
                                     " +\\([AD\\.]+\\)" ;Attr
                                     " +\\([0-9]+\\)"   ;Size
                                     " +\\([0-9]+\\)"   ;Compressed
                                     " +\\([^ \n]+\\)\n")) ;Name
           (goto-char (match-end 0))
           (let ((name (match-string 6))
                 (size (match-string 4)))
             (if (> (length name) maxname) (setq maxname (length name)))
             (if (> (length size) maxsize) (setq maxsize (length size)))
             (push (vector name name nil nil
                           ;; Size, Compressed.
                           size (match-string 5)
                           ;; Date, Time.
                           (match-string 1) (match-string 2))
                   files))))
       (setq files (nreverse files))
       (goto-char (point-min))
       (let* ((format (format " %%s %%s  %%%ds %%%ds  %%s" maxsize maxsize))
              (sep (format format "----------" "--------" (make-string maxsize ?-)
                           (make-string maxsize ?-) ""))
              (column (length sep)))
         (insert (format format "  Date  " "  Time  " "  Size " " Comp " "  Filename") "\n")
         (insert sep (make-string maxname ?-) "\n")
         (archive-summarize-files (mapcar (lambda (desc)
                                            (let ((text
                                                   (format format
                                                           (aref desc 6)
                                                           (aref desc 7)
                                                           (aref desc 4)
                                                           (aref desc 5)
                                                           (aref desc 1))))
                                              (vector text
                                                      column
                                                      (length text))))
                                          files))
         (insert sep (make-string maxname ?-) "\n")
         (apply 'vector files)))))

(eval-after-load "arc-mode"
  '(defun archive-rar-extract (archive name)
     ;; use 7z.exe to extract
     (if (file-name-absolute-p name)
         ;; The code below assumes the name is relative and may do undesirable
         ;; things otherwise.
         (error "Can't extract files with non-relative names")
       (let ((dest (make-temp-file "arc-rar" 'dir))
             (nondir-name (file-name-nondirectory name))) ; FIXME name with directory
         (unwind-protect
             (progn
               (call-process "7z" nil nil nil
                             "e" archive name (format "-o%s" dest))
               (insert-file-contents-literally (expand-file-name nondir-name dest)))
           (delete-file (expand-file-name nondir-name dest))
           (while (file-name-directory nondir-name)
             (setq name (directory-file-name (file-name-directory nondir-name)))
             (delete-directory (expand-file-name nondir-name dest)))
           (delete-directory dest))))))

(provide 'binjo-arc)
;;; binjo-arc.el ends here
