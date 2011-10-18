;;; Binjo @ Anchiva - refactor @ 2009-03-18 wed. 15:00
;;;                            @ 2010-02-04 wed. 18:00
(cond ((eq system-type 'windows-nt)
       (setenv "HOME" "d:/repos/emacs/"))
      ((eq system-type 'gnu/linux)
       (setenv "HOME" "/home/binjo/repos/emacs/"))
      ((eq system-type 'darwin)
       (setenv "HOME" (concat "/Users/" user-login-name "/repos/dotemacs/")))
      (t
       (error "not implemented...")))

;; `abbreviated-home-dir' will be set properly by `abbreviate-file-name'.
(setq abbreviated-home-dir nil)

(load "~/.emacs.d/config/binjo-main.el")
