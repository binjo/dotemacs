;;; binjo-emms.el --- configs 4 emms

;; Copyright 2009 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: binjo-emms.el,v 0.0 2009/12/11 16:38:20 binjo Exp $
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

;; My config settings for emms

;;; History:

;; 12/11/2009, init

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'binjo-emms)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'emms-setup)
(emms-standard)


(setq emms-player-mpg321-command-name "mpg123"
      emms-player-mplayer-command-name "mplayer"
      emms-player-mplayer-parameters
      (append emms-player-mplayer-parameters
              ;; '( "-font" "Simsun") ;"/usr/share/fonts/truetype/microsoft/simsun.ttf")
              ;; "-idx"
              ;; '("-subcp" "cp936")
              )
      emms-player-list '(emms-player-mplayer
                         emms-player-mplayer-playlist
                         emms-player-ogg123
                         emms-player-mpg321
                         ))

(setq emms-info-mp3info-coding-system 'gbk
      emms-cache-file-coding-system 'utf-8
      emms-i18n-default-coding-system '(gbk . gbk)
      )

(add-to-list 'file-coding-system-alist '("/[mM]usic/.*" gbk . gbk))

(setq emms-playlist-buffer-name "*EMMS Playlist*"
      emms-source-file-default-directory "d:/Datas/music/"
      emms-playing-time-style 'bar
      emms-info-asynchronously nil)

(setq emms-playlist-mode-window-width (* 0.3 (frame-width)))

(add-hook 'emms-playlist-limit-hook
          'emms-playlist-sort-by-natural-order)

;; Update track info in playlist buffer when finished.

(add-hook 'emms-player-finished-hook
          (lambda ()
            (with-current-emms-playlist
              (save-excursion
                (emms-playlist-mode-center-current)
                (emms-playlist-update-track)))))

;; Mode Line Format
(setq emms-mode-line-format "[ %s ]"
      emms-lyrics-display-format "%s"
      emms-playing-time-display-format "%s")

(require 'emms-mode-line-icon)


;;; Track Show Format (for playlist buffer)

(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%a %H:%M")
	(604800                           . "%a %H:%M") ; this week
	((emms-last-played-seconds-month) . "%d")
	((emms-last-played-seconds-year)  . "%m/%d")
	(t                                . "%Y/%m/%d")))

(eval-after-load 'emms
  '(progn
     (setq xwl-emms-playlist-last-track nil)
     (setq xwl-emms-playlist-last-indent "\\")

     (defun xwl-emms-track-description-function (track)
       "Return a description of the current track."
       (let* ((name (emms-track-name track))
              (type (emms-track-type track))
              (short-name (file-name-nondirectory name))
              (play-count (or (emms-track-get track 'play-count) 0))
              (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
              (empty "..."))
         (prog1
             (case (emms-track-type track)
               ((file url)
                (let* ((artist (or (emms-track-get track 'info-artist) empty))
                       (year (emms-track-get track 'info-year))
                       (playing-time (or (emms-track-get track 'info-playing-time) 0))
                       (min (/ playing-time 60))
                       (sec (% playing-time 60))
                       (album (or (emms-track-get track 'info-album) empty))
                       (tracknumber (emms-track-get track 'info-tracknumber))
                       (short-name (file-name-sans-extension
                                    (file-name-nondirectory name)))
                       (title (or (emms-track-get track 'info-title) short-name))

                       ;; last track
                       (ltrack xwl-emms-playlist-last-track)
                       (lartist (or (and ltrack (emms-track-get ltrack 'info-artist))
                                    empty))
                       (lalbum (or (and ltrack (emms-track-get ltrack 'info-album))
                                   empty))

                       (same-album-p (and (not (string= lalbum empty))
                                          (string= album lalbum))))
                  (format "%10s  %3d   %-20s%-60s%-35s%-15s%s"
                          (emms-last-played-format-date last-played)
                          play-count
                          ;;                           (if (and (not (string= lartist empty))
                          ;;                                    (string= artist lartist))
                          ;;                               empty
                          ;;                             artist)
                          artist

                          ;; Combine indention, tracknumber, title.
                          ;; (format "%s%s%-40s"
                          (concat
                           (if same-album-p ; indention by album
                               (setq xwl-emms-playlist-last-indent
                                     (concat " " xwl-emms-playlist-last-indent))
                             (setq xwl-emms-playlist-last-indent "\\")
                             "")
                           (if (and tracknumber ; tracknumber
                                    (not (zerop (string-to-number tracknumber))))
                               (format "%02d." (string-to-number tracknumber))
                             "")
                           title        ; title
                           )

                          ;; album
                          (cond ((string= album empty) empty)
                                ;; (same-album-p "  ")
                                (t (concat "《" album "》")))

                          (or year empty)
                          (if (or (> min 0)  (> sec 0))
                              (format "%02d:%02d" min sec)
                            empty))))
               ((url)
                (concat (symbol-name type)
                        ":"
                        (decode-coding-string
                         (encode-coding-string name 'utf-8)
                         'gbk)))
               (t
                (format "%-3d%s"
                        play-count
                        (concat (symbol-name type) ":" name))))
           (setq xwl-emms-playlist-last-track track))))

     (setq emms-track-description-function
           'xwl-emms-track-description-function)
     ))

;; Redefine this to use name only
(defun xwl-emms-mode-line-playlist-current ()
  "Format the currently playing song."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (name (emms-track-name track))
         (artist (emms-track-get track 'info-artist))
         (title (emms-track-get track 'info-title)))
    (format "[ %s ]"
            (cond ((and artist title)
                   (concat artist " - " title))
                  (title
                   title)
                  ((eq type 'file)
                   (file-name-sans-extension (file-name-nondirectory name)))
                  (t
                   (concat (symbol-name type) ":" name))))))

(setq emms-mode-line-mode-line-function
      'xwl-emms-mode-line-playlist-current)

(setq emms-mode-line-titlebar-function nil)
;;      'xwl-emms-mode-line-playlist-current)


;;; Key Bindings

(defun xwl-emms-playlist-mode-hook ()
  (toggle-truncate-lines 1))

(add-hook 'emms-playlist-mode-hook 'xwl-emms-playlist-mode-hook)

(define-key emms-playlist-mode-map (kbd "x") 'emms-start)
(define-key emms-playlist-mode-map (kbd "v") 'emms-stop)
(define-key emms-playlist-mode-map (kbd "h") 'emms-shuffle)
(define-key emms-playlist-mode-map (kbd "o") 'emms-show)
(define-key emms-playlist-mode-map (kbd "F") 'emms-playlist-show-current-line)
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "r") 'emms-toggle-repeat-track)
(define-key emms-playlist-mode-map (kbd "R") 'emms-toggle-repeat-playlist)
(define-key emms-playlist-mode-map (kbd "q") 'next-buffer) ; 'delete-window)
(define-key emms-playlist-mode-map (kbd "<left>")  (lambda () (interactive) (emms-seek -10)))
(define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
(define-key emms-playlist-mode-map (kbd "<down>")  (lambda () (interactive) (emms-seek -60)))
(define-key emms-playlist-mode-map (kbd "<up>")    (lambda () (interactive) (emms-seek +60)))

(define-key emms-playlist-mode-map (kbd "N") 'emms-next)
(define-key emms-playlist-mode-map (kbd "P") 'emms-previous)
(define-key emms-playlist-mode-map (kbd "E") 'emms-tag-editor-edit)

;; emms dired
(define-key emms-playlist-mode-map (kbd "n") 'next-line)
(define-key emms-playlist-mode-map (kbd "p") 'previous-line)
(define-key emms-playlist-mode-map (kbd "d") 'emms-playlist-mode-delete-selected-track)
;; (define-key emms-playlist-mode-map (kbd "D") 'emms-playlist-mode-delete)
;; (define-key emms-playlist-mode-map (kbd "m") 'emms-playlist-mode-mark)
;; (define-key emms-playlist-mode-map (kbd "u") 'emms-playlist-mode-unmark)

(global-set-key (kbd "C-c e x")   'emms-start)
(global-set-key (kbd "C-c e v")   'emms-stop)
(global-set-key (kbd "C-c e n")   'emms-next)
(global-set-key (kbd "C-c e p")   'emms-previous)
(global-set-key (kbd "C-c e o")   'emms-show)
(global-set-key (kbd "C-c e h")   'emms-shuffle)
(global-set-key (kbd "C-c e SPC") 'emms-pause)
(global-set-key (kbd "C-c e f")   'emms-no-next)
(global-set-key (kbd "C-c e F")   'emms-no-next-and-sleep)
(global-set-key (kbd "C-c e a")   'emms-add-directory-tree)
(global-set-key (kbd "C-c e d")   'emms-playlist-mode-delete-selected-track)
(global-set-key (kbd "C-c e r")   'emms-toggle-repeat-track)
(global-set-key (kbd "C-c e R")   'emms-toggle-repeat-playlist)
(global-set-key (kbd "C-c e l")   'emms-lyrics-visit-lyric)

(global-set-key (kbd "C-c e s")   'emms-lastfm-radio-similar-artists)
(global-set-key (kbd "C-c e k")   'emms-lastfm-radio-skip)

(provide 'binjo-emms)
;;; binjo-emms.el ends here
