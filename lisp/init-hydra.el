(use-package hydra
  :ensure t
  )


(use-package pretty-hydra
  :ensure t
  :bind
  (("C-c o" . hydra-org/body)
   ("C-c w" . jp-window/body)
   ("C-c b" . hydra-bibtex/body)
   ("C-c m" . hydra-misc/body)
   ("C-c e" . hydra-eaf/body)
   ("C-c d" . hydra-emacs/body)
   ("C-c r" . hydra-roam/body)
   ("C-c i" . hydra-insert/body)
   ("C-c [" . hydra-skan-user-buffers-prev/body)
   ("C-c ]" . hydra-skan-user-buffers-next/body)
   ("C-c n" . hydra-org-noter/body)
   :map org-agenda-mode-map
   ("C-c a" . hydra-org-agenda/body)
   ))

(pretty-hydra-define hydra-eaf
  (:color amaranth :exit t :quit-key "q" :idle 2
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil)))
  ("Eaf"
   (("o" eaf-open "eaf open")
    ("b" eaf-open-browser "open browser")
    ("h" eaf-open-browser-with-history "open history"))
   )
  )
(pretty-hydra-define hydra-roam
  (:color amaranth :exit t :quit-key "q" :idle 2
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("Roam"
   (("l" org-roam-buffer-toggle "toggle roam buffer")
    ("f" org-roam-node-find "find roam node")
    ("n" org-id-get-create "create roam id")
    ("i" org-roam-node-insert "insert roam node")
    ("s" helm-org-in-buffer-headings "search headings")
    ("w" org-roam-refile "refile roam node"))
   "Roam"
   (("c" org-roam-dailies-capture-today "roam capture")
    ("ra" org-roam-ref-add "add refs")
    ("rd" org-roam-ref-remove "remove a ref")
    ("t" org-roam-tag-add "add tags")
    ("v" org-roam-tag-remove "remove a tag")
    ("h" yuchen/helm-org-run-marked-heading-id-link "insert a headline"))
   "Roam"
   (("dd" org-roam-dailies-find-today "today")
    ("df" org-roam-dailies-find-date "find date")
    ("dc" org-roam-dailies-capture-today "find today")
    ("aa" org-roam-alias-add "add alias")
    ("ar" org-roam-alias-remove "remove alias"))
   )
  )

(pretty-hydra-define hydra-emacs
  (:color amaranth :exit t :quit-key "q" :idle 2
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("basic"
   (("E" eval-buffer)
    ("f" (find-file "~/.emacs.d/lisp") "config files")
    )
   "Search"
   (("g" consult-git-grep))
   "Bookmark"
   (("bs" bookmark-set "set bookmark")
    ("bj" consult-bookmark "jump bookmark")
    ("bd" bookmark-delete "delete bookmark"))
   "edit"
   (("ed" mc/mark-all-dwim "mark all dwim")
    ("ee" mc/edit-lines "elit lines"))
   "treemacs"
   (("t" treemacs "treemacs"))
   "Deft"
   (("d" eli/deft-search-for "search"))
   ))

(pretty-hydra-define hydra-org
  (:color amaranth :exit t :quit-key "q"
	  :pre
	  (progn (setq which-key-inhibit t)
	  )
	  :post (progn (setq which-key-inhibit nil) ))
  ("Basic"
   (("a" org-archive-subtree "archive subtree")
    ;; ("c" org-capture "org capture")
    ("h" org-mode "org mode")
    ("pp" org-pomodoro "sart a pomodoro")
    ("pt" org-pomodoro-extend-last-clock "extend pomodoro")
    )
   "Org link"
   (("li" grab-x-link-chromium-insert-link "insert web link")
    ("lo" grab-x-link-chromium-insert-org-link "insert org link")
    ("ld" org-download-clipboard "store link"))
   "Org-clock"
   (("i" org-clock-in)
    ("c" org-clock-in-last)
    ("o" org-clock-out)
    
    ("ke" org-clock-modify-effort-estimate "modify effort estimates")
    ("kk" org-clock-cancel)

    ("g" org-clock-goto)
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
   "query"
   (("et" org-ql-sparse-tree "org-ql sparse tree")
    ("es" org-ql-search "org-ql search")
    ("ev" org-ql-view "org-ql view"))
   "blog"
   (("bp" org-publish)
    ("bg" eli/push-to-gitpage)
    ("bt" org-timeline-export-to-html "export timeline"))
   ))

(pretty-hydra-define jp-window
  (:color amaranth :exit nil :quit-key "q"
	  :pre (progn (setq which-key-inhibit t)  )
          :post (progn (setq which-key-inhibit nil) ))
  ("Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
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
   (("b" split-window-right "horizontally")
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
   (("hA" org-agenda-archive-default "archive default")
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
    ("gd" org-agenda-goto-date "goto date")
    ("." org-agenda-goto-today "goto today")
    ("gr" org-agenda-redo "redo"))))

(pretty-hydra-define hydra-bibtex
  (:color amaranth :exit t :quit-key "q"
	  :pre (progn (setq which-key-inhibit t))
	  :post (progn (setq which-key-inhibit nil) ))
  ("Reference"
   (("i" org-ref-insert-link "insert ref link")
    ("g" org-ref-insert-bibliography-link "insert bibliography")
    ("s" org-ref-insert-bibliographystyle-link "insert bibliographystyle"))
   "Calibre"
   (("b" calibredb "calibre")
    )
   "Roam Bibtex"
   (("a" orb-note-actions "orb note actions")
    ("l" orb-insert-link "insert orb link")
    )
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
  ("Emoji"
   (("i" emojify-insert-emoji "insert emoji")
    ("s" emojify-apropos-emoji "search emoji"))
   "Yank"
   (("p" consult-yank-pop "Clipboard")))
  )
(provide 'init-hydra)
