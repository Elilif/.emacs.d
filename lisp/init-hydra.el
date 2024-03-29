;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

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


(use-package hydra
  :ensure t
  :defer t
  )


(use-package pretty-hydra
  :ensure t
  :defer t
  :bind
  (("C-c o" . hydra-org/body)
   ("C-c w" . jp-window/body)
   ("C-c b" . hydra-bibtex/body)
   ("C-c s" . hydra-search/body)
   ("C-c p" . hydra-player/body)
   ("C-c e" . hydra-edit/body)
   ("C-c m" . hydra-misc/body)
   ("C-c d" . hydra-develop/body)
   ("C-c q" . hydra-emacs/body)
   ("C-c r" . hydra-roam/body)
   ("C-c i" . hydra-insert/body)
   ("C-c [" . hydra-skan-user-buffers-prev/body)
   ("C-c ]" . hydra-skan-user-buffers-next/body)
   ("C-c n" . hydra-org-noter/body)
   ("C-c a" . hydra-org-agenda/body)
   )
  :config
  (pretty-hydra-define hydra-search
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil) ))
    ("Dicts"
     (("t" Eli/te-search "search TE")
      ("d" Eli/dict-search "search Dicts"))
     "Deft"
     (("n" notdeft "notdeft"))
     "git grep"
     (("g" consult-git-grep))
     "Google"
     (("s" my-search-with-chrome))
     ))
  (pretty-hydra-define hydra-edit
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil) ))
    ("multiple cursors"
     (("l" mc/edit-lines "edit-lines")
      ("n" mc/mark-next-like-this "mark next")
      ("p" mc/mark-previous-like-this "mark previous")
      ("a" mc/mark-all-like-this-dwim "makk all")
      ("s" set-rectangular-region-anchor "set mc")
      ("in" mc/insert-numbers "insert numbers"))
     "iedit"
     (("e" iedit-mode "iedit mode"))
     ))

  (pretty-hydra-define hydra-roam
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil) ))
    ("Roam"
     (("l" org-roam-buffer-toggle "toggle roam buffer")
      ("f" org-roam-node-find "find roam node")
      ("n" org-id-get-create "create roam id")
      ("i" org-roam-node-insert "insert roam node")
      ("s" helm-org-roam-files-headings "search headings")
      ("w" org-roam-refile "refile roam node"))
     "Roam"
     (("c" org-roam-dailies-capture-today "roam capture")
      ("ra" org-roam-ref-add "add refs")
      ("rd" org-roam-ref-remove "remove a ref")
      ("t" org-roam-tag-add "add tags")
      ("v" org-roam-tag-remove "remove a tag")
      ("h" yuchen/helm-org-run-marked-heading-id-link "insert a headline"))
     "Roam"
     (("dd" org-roam-dailies-goto-today "today")
      ("df" org-roam-dailies-goto-date "goto date")
      ("dc" org-roam-dailies-capture-today "goto today")
      ("aa" org-roam-alias-add "add alias")
      ("ar" org-roam-alias-remove "remove alias")
      ("u" org-roam-ui-mode))
     )
    )

  (pretty-hydra-define hydra-emacs
    (:color amaranth :exit t :quit-key "q" :idle 2
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil) ))
    ("basic"
     (("E" eval-buffer)
      ("f" (find-file "~/.emacs.d/lisp") "config files")
      ("R" restart-emacs)
      )
     "Bookmark"
     (("bs" bookmark-set "set bookmark")
      ("bj" consult-bookmark "jump bookmark")
      ("bd" bookmark-delete "delete bookmark"))
     "edit"
     (("ed" mc/mark-all-dwim "mark all dwim")
      ("ee" mc/edit-lines "elit lines"))
     "treemacs"
     (("t" treemacs "treemacs"))
     ))

  (pretty-hydra-define hydra-org
    (:color amaranth :exit t :quit-key "q"
	    :pre
	    (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil) ))
    ("Basic"
     (("a" org-archive-subtree "archive subtree")
      ;; ("c" org-capture "org capture")
      ("h" org-mode "org mode")
      ("pp" org-pomodoro "sart a pomodoro")
      ("pt" org-pomodoro-extend-last-clock "extend pomodoro")
      ("f" my/consult-org-file "agenda filter")
      )
     "Org link"
     (("li" grab-x-link-chromium-insert-link "insert web link")
      ("lo" grab-x-link-chromium-insert-org-link "insert org link")
      ("ld" org-download-clipboard "org download clipboard"))
     "Org-clock"
     (("i" org-clock-in)
      ("c" org-mru-clock-in)
      ("o" org-clock-out)
      
      ("ke" org-clock-modify-effort-estimate "modify effort estimates")
      ("kk" org-clock-cancel)

      ("g" org-mru-clock-goto)
      ("d" org-clock-display)
      ("kr" org-clock-report)
      ("?" (org-info "Clocking commands")))
     "Org-timer"
     (("tr" org-timer-start)
      ("tn" org-timer-set-timer)
      ("tp" org-timer-pause-or-continue "Pause/Continue")
      ("ts" org-timer-stop)
      ("tm" org-timer)
      ("tt" org-timer-item)
      ("tz" (org-info "Timers") "info timers")
      )
     "Writing"
     (("ww" writeroom-mode "write room")
      ("wm" writeroom-toggle-mode-lint "toggle modeline")
      ("wg" writegood-mode "write good")
      ("wl" writegood-grade-level "grade level")
      ("we" writegood-reading-ease "reading-ease"))
     "blog"
     (("bp" org-publish)
      ("bg" eli/push-to-gitpage)
      ("bt" org-timeline-export-to-html "export timeline"))
     ))

  (pretty-hydra-define jp-window
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t)  )
            :post (progn (setq which-key-inhibit nil) ))
    ("Actions"
     (("TAB" other-window "switch")
      ("m" ace-delete-window "delete")
      ("x" ace-delete-other-windows "maximize")
      ("s" ace-swap-window "swap")
      ("a" ace-select-window "select"))

     ;; "Resize"
     ;; (("h" move-border-left "←")
     ;;  ("j" move-border-down "↓")
     ;;  ("k" move-border-up "↑")
     ;;  ("l" move-border-right "→")
     ;;  ("n" balance-windows "balance")
     ;;  ("f" toggle-frame-fullscreen "toggle fullscreen"))

     "Split"
     (("h" split-window-right "horizontally")
      ("v" split-window-below "vertically")
      )

     "Zoom"
     (("=" text-scale-increase "in")
      ("-" text-scale-decrease "out")
      ("[" shrink-window-horizontally "h-shrink window")
      ("]" enlarge-window-horizontally "h-shrink window")
      ("b" balacne-windows "balacne windows")
      )))

  ;; Hydra for org agenda (graciously taken from Spacemacs)
  ;; (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
  ;;                                  :post (setq which-key-inhibit nil)
  ;;                                  :hint none))
  (pretty-hydra-define hydra-org-agenda
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t)  )
	    :post (progn (setq which-key-inhibit nil) ))
    ("Entry"
     (("a" org-agenda)
      ("g" (org-agenda nil "g") "GTD")
      ("hA" org-agenda-archive-default "archive default")
      ("hk" org-agenda-kill "kill")
      ("hp" org-agenda-priority "priority")
      ("hr" org-agenda-refile "refile")
      ("h:" org-agenda-set-tags "set tage")
      ("ht" org-agenda-todo "todo"))
     "Visit entry"
     (("o"   link-hint-open-link "open link" :exit t)
      ("<tab>" org-agenda-goto "goto" :exit t)
      ("TAB" org-agenda-goto "goto" :exit t)
      ("SPC" org-agenda-show-and-scroll-up "show and scroll up")
      ("RET" org-agenda-switch-to "switch to" :exit t))
     "Date"
     (("dt" org-agenda-date-prompt "date-promt")
      ("dd" org-agenda-deadline "deadline")
      ("+" org-agenda-do-date-later "do date later")
      ("-" org-agenda-do-date-earlier "do date earlier")
      ("ds" org-agenda-schedule "schedule"))
     "View"
     (("vd" org-agenda-day-view "day view")
      ("vw" org-agenda-week-view "week view")
      ("vt" org-agenda-fortnight-view "fortnight view")
      ("vm" org-agenda-month-view "month view")
      ("vy" org-agenda-year-view "year view")
      ("vn" org-agenda-later "later")
      ("vp" org-agenda-earlier "earlier")
      ("vr" org-agenda-reset-view "reset view"))
     "Toggle mode"
     (("ta" org-agenda-archives-mode "archives mode")
      ("tA" (org-agenda-archives-mode 'files))
      ("tr" org-agenda-clockreport-mode "clockreport mode")
      ("tf" org-agenda-follow-mode "follow mode")
      ("tl" org-agenda-log-mode "log mode")
      ("td" org-agenda-toggle-diary "toggle diary"))
     "Filter"
     (("fc" org-agenda-filter-by-category "by category")
      ("fx" org-agenda-filter-by-regexp "by regexp")
      ("ft" org-agenda-filter-by-tag "by tag")
      ("fr" org-agenda-filter-by-tag-refine "by rag refine")
      ("fh" org-agenda-filter-by-top-headline "by top headline")
      ("fd" org-agenda-filter-remove-all "remove all"))
     "Clock"
     (("cq" org-agenda-clock-cancel "cancel")
      ("cj" org-agenda-clock-goto "goto" :exit t)
      ("ci" org-agenda-clock-in "clock in" :exit t)
      ("co" org-agenda-clock-out "clock out"))
     "Other"
     (("q" nil :exit t)
      ("sd" org-agenda-goto-date "goto date")
      ("." org-agenda-goto-today "goto today")
      ("rr" org-agenda-redo "redo"))))

  (pretty-hydra-define hydra-bibtex
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil) ))
    ("Reference"
     (("o" citar-open "citar open")
      ("s" ex/search-pdf-contents "search pdf")
      )
     "Calibre"
     (("c" calibredb "calibre")
      ("b" eli/update-calibre-bibtex "get bibtex")
      )
     "Roam Bibtex"
     (("a" orb-note-actions "orb note actions")
      ("l" orb-insert-link "insert orb link")
      )
     "Bibtex"
     (("r" org-bibtex-read "org bibtex read")
      ("w" org-bibtex-write "org bibtex write"))
     )
    )
  (pretty-hydra-define hydra-org-noter
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t)  )
	    :post (progn (setq which-key-inhibit nil) ))
    ("Noter"
     (("n" org-noter "noter")
      ("c" org-noter-create-skeleton "create skeleton")
      )
     "Noter Pdftools"
     (("pc" org-noter-pdftools-create-skeleton "create skeleton"))
     "PDF Annotation"
     (("l" pdf-annot-list-annotations "list annotations")
      )
     ))
  (pretty-hydra-define hydra-misc
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t)  )
	    :post (progn (setq which-key-inhibit nil) ))
    ("Elfeed"
     (("e" elfeed "elfeed"))
     "Mu4e"
     (("m" mu4e "Mails"))
     "Wttrin"
     (("w" wttrin "Weather"))
     ))

  (pretty-hydra-define hydra-skan-user-buffers-next
    (:body-pre (next-buffer)
	       :hint nil
	       :quit-key "q"
	       :pre (progn (setq which-key-inhibit t)  )
	       :post (progn (setq which-key-inhibit nil) ))
    ("skan user buffers"
     (("\]" next-buffer)
      ("\[" previous-buffer)
      ("k" kill-this-buffer)
      ("q" nil))))

  (pretty-hydra-define hydra-skan-user-buffers-prev
    (:body-pre (previous-buffer)
	       :hint nil
	       :quit-key "q"
	       :pre (progn (setq which-key-inhibit t)  )
	       :post (progn (setq which-key-inhibit nil) ))
    ("skan user buffers"
     (("\]" next-buffer)
      ("\[" previous-buffer)
      ("k" kill-this-buffer)
      ("q" nil))))

  (pretty-hydra-define hydra-insert
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil)
			 ))
    ("Input"
     (("s" eli/input-switch "switch input"))
     "Yank"
     (("p" consult-yank-pop "Clipboard"))
     "Emoji"
     (("i" emoji-insert)
      ("f" emoji-search)))
    )
  (pretty-hydra-define hydra-player
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil)
			 ))
    ("Playlists"
     (("e" emms)
      ;; ("g" emms-play-directory "open dir")
      ("v" emms-playlist-mode-go "go to current")
      ("m" emms-metaplaylist-mode-go "metaplaylist")
      ;; ("t" emms-play-directory-tree "play directory")
      )
     "Controls"
     (("n" emms-next "next" :exit nil)
      ("p" emms-previous "previous" :exit nil)
      ("x" emms-shuffle "shuffle")
      ("i" emms-show "song info")
      ("-" emms-volume-lower "lower volume" :exit nil)
      )
     "Controls"
     (("SPC" emms-pause "pause")
      ("r" emms-toggle-repeat-track "repeat")
      ("," emms-seek-backward "backward" :exit nil)
      ("." emms-seek-forward "forward" :exit nil)
      ;; ("d" emms-play-dired "play the list")
      ("=" emms-volume-raise "raise volume" :exit nil)
      )
     "lyrics"
     (("l" lyrics-fetcher-show-lyrics "lyrics")
      ("c" lyrics-fetcher-lyrics-catchup "lyrics catchup")
      ("t" emms-lyrics-toggle-display-on-minibuffer "toggle lyrics")
      )
     "Browser"
     (("b" emms-browser "browser")
      ("o" emms-browser-show-file-on-line "show score")
      ("d" emms-browser-set-score "set score")
      )
     "Score"
     (("ss" emms-score-set-playing "set score for playing track")
      ("sd" emms-score-show-playing "show score of playing track")
      ("st" emms-score-set-tolerance "tolerance")
      ("f"  eli/emms-filter "filter"))
     ))
  (pretty-hydra-define hydra-develop
    (:color amaranth :exit t :quit-key "q"
	    :pre (progn (setq which-key-inhibit t))
	    :post (progn (setq which-key-inhibit nil)))
    ("LSP"
     (("p" lsp-ui-peek-find-definitions "peed")
      ("i" lsp-ui-imenu "imenu"))
     "Debug"
     (("r" quickrun "quickrun")
      ("s" quickrun-shell "quickrun shell")
      ("d" gdb "gdb"))
     )
    ))


(provide 'init-hydra)
;;; init-hydra.el ends here.
