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
  (emms-playlist-buffer-name "*Emms*")
  (emms-source-file-default-directory "~/Music/")
  (emms-lyrics-dir "~/Music/lyrics")
  (emms-lyrics-display-on-minibuffer t)
  (emms-lyrics-display-on-modeline nil)
  (emms-player-list '(emms-player-mpv))
  ;; covers
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-history-load)
  (emms-mode-line-disable)

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

  ;; check and set scores in browser buffer
  (defun emms-browser-show-file-on-line ()
    "Show score for track at point in emms-browser buffer."
    (interactive)
    (message "track/tolerance score: %d/%d"
	     (emms-score-get-score
	      (emms-track-get (nth 0 (emms-browser-tracks-at-point))
			      'name))
	     emms-score-min-score))

  (defun emms-browser-set-score (score)
    "Set score for track at point in emms-browser buffer."
    (interactive "nSet score for this track: ")
    (let ((filename (emms-track-get (nth 0 (emms-browser-tracks-at-point))
				    'name)))
      (emms-score-change-score
       (- score (emms-score-get-score filename))
       filename))))

(use-package lyrics-fetcher
  :ensure t
  :after emms
  :bind (:map lyrics-fetcher-view-mode-map
	      ("RET" . lyrics-fetcher-neteasecloud-lyrics-jump))
  :config
  (lyrics-fetcher-use-backend 'neteasecloud)

  ;; jump to specific lyrics
  (defun lyrics-fetcher-neteasecloud-lyrics-jump ()
    (interactive)
    (if (derived-mode-p 'lyrics-fetcher-view-mode)
	(let* ((timestamp (save-excursion
			    (beginning-of-line)
			    (thing-at-point 'sexp ':no-properties)))
	       (minutes (string-to-number
			 (progn
			   (string-match "\\([[:digit:]]\\{2\\}\\):\\([[:digit:]]\\{2\\}\\)" timestamp)
			   (match-string 1 timestamp))))
	       (seconds (string-to-number
			 (match-string 2 timestamp))))
	  (if timestamp
	      (emms-seek-to (+ (* 60 minutes) seconds))
	    (message "No timestamp found!")))
    (message "This function must be called in lyrics-fetcher-view-mode!"))))

(provide 'init-music)
