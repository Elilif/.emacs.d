;; init-music.el --- Initialize music configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Calendar configuration.
;;

;;; Code:

(use-package emms
  :ensure t
  :defer t
  :commands (emms)
  :init
  (setq emms-score-max-score 10)
  (defvar eli-filter-score 0)
  :custom
  ;; (emms-playlist-buffer-name "*Emms*")
  (emms-source-file-default-directory "~/Music/")
  (emms-lyrics-display-on-minibuffer nil)
  (emms-lyrics-display-on-modeline nil)
  (emms-player-list '(emms-player-mpv))
  (emms-browser-covers 'emms-browser-cache-thumbnail)
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-history-load)
  (emms-mode-line-disable)
  ;; covers
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-browser-thumbnail-small-size 64)
  (setq emms-browser-thumbnail-medium-size 128)

  ;; filters
  (emms-browser-make-filter "all" 'ignore)
  (emms-browser-make-filter
   "by-scores"
   (lambda (track)
     (not (>= (funcall 'emms-score-get-score (emms-track-get track 'name)) eli-filter-score))))

  (defun eli/emms-filter (score)
    (interactive "nSet score for filter: ")
    (setq eli-filter-score score)
    (emms-browser-show-by-scores)
    )
  
  
  )

(use-package lyrics-fetcher
  :ensure t
  :after emms
  :config
  (setq lyrics-fetcher-genius-access-token "wxV8JATH2c0ktcikcnRL-GSrFx7jY-UEouWXNmmQC9_irv7co8mNIJRw6hjn0og7"))



(provide 'init-music)
