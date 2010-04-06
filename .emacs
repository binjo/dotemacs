;;; Binjo @ Anchiva - refactor @ 2009-03-18 wed. 15:00
;;;                            @ 2010-02-04 wed. 18:00
(when (eq system-type 'windows-nt)
  (setenv "HOME" "d:/repos/emacs/")
  ;; `abbreviated-home-dir' will be set properly by `abbreviate-file-name'.
  (setq abbreviated-home-dir nil))

(load "~/.emacs.d/config/binjo-main.el")
