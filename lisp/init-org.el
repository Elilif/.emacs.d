;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

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



(use-package org
  ;; :load-path "~/.emacs.d/private/org-mode/lisp"
  :ensure t
  :defer t
  :hook ((org-mode . org-indent-mode)
	 (org-mode . auto-fill-mode)
	 (org-mode . column-number-mode)
	 (org-mode . prettify-symbols-mode))
  :bind (("\C-c c" . 'org-capture)
	 ;; ("\C-c a" . 'org-agenda)
	 (:map minibuffer-local-map
	       ("C-c C-l" . 'org-insert-link))
	 (:map org-mode-map
	       ("C-<tab>" . eli/org-expand-all))
	 )
  :config

  (defun eli/insert-open-checkbox ()
    (interactive)
    (let* ((headline (save-excursion
		       (org-back-to-heading)
		       (org-element-at-point)))
	   (text-begin (org-element-property :contents-begin headline))
	   (text-end (org-element-property :contents-end headline))
	   (content (buffer-substring text-begin text-end)))
      (org-element-map (org-element--parse-elements text-begin
						    text-end 'first-section nil 'object nil (list 'org-data nil))
	  'item
	(lambda (item) (if (eq (org-element-property :checkbox item) 'off)
			   (progn
			     (beginning-of-line)
			     (insert (org-element-interpret-data item))))))))

  (setq org-adapt-indentation t)
  ;; prettify symbols
  (setq org-pretty-entities nil)

  ;; improving emphasis marker
  (defun sp-texmathp (id action _context)
    (texmathp))
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "* "
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_ " :unless '(sp-point-after-word-p))
    (sp-local-pair "/" "/ " :unless '(sp-point-after-word-p sp-org-point-after-left-square-bracket-p sp-texmathp) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~ " :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "= " :unless '(sp-point-after-word-p sp-texmathp) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))

  (use-package org-inlinetask
    :defer 5)
  (use-package org-mu4e
    :defer t)
  (setq org-clock-sound "~/.emacs.d/private/bellring.wav")
  (setq org-src-fontify-natively t)
  (setq org-agenda-span 'day)
  (setq org-use-fast-todo-selection 'expert)
  (setq org-agenda-window-setup 'only-window)
  ;; Change task state to STARTED when clocking in
  (setq org-log-into-drawer t)
  (setq org-startup-folded t)
  (setq org-hide-block-startup t)
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (define-key org-mode-map (kbd "C-'") 'nil)
  (define-key org-mode-map (kbd "C-'") 'avy-goto-char)
  (define-key org-mode-map (kbd "C-c \[") 'hydra-skan-user-buffers-prev/body)

  ;; renumbering and sorting footnotes automatically after each deletion or insertion
  (setq org-footnote-auto-adjust t)

  ;; hide drawers
  ;; ((eq org-cycle-subtree-status 'subtree)
  ;;  (org-show-subtree)
  ;;  (org-unlogged-message "ALL")
  ;;  (setq org-cycle-subtree-status 'all))
  ;; add above codes before ((or children-skipped in org-cycle-internal-local
  (defun org-cycle-hide-drawers (state)
    "Re-hide all drawers after a visibility state change."
    (when (and (derived-mode-p 'org-mode)
               (not (memq state '(overview folded contents))))
      (save-excursion
	(let* ((globalp (memq state '(contents all)))
               (beg (if globalp
			(point-min)
                      (point)))
               (end (if globalp
			(point-max)
                      (if (eq state 'children)
			  (save-excursion
                            (outline-next-heading)
                            (point))
			(org-end-of-subtree t)))))
          (goto-char beg)
          (while (re-search-forward org-drawer-regexp end t)
            (save-excursion
              (beginning-of-line 1)
              (when (looking-at org-drawer-regexp)
		(let* ((start (1- (match-beginning 0)))
                       (limit
			(save-excursion
                          (outline-next-heading)
                          (point)))
                       (msg (format
                             (concat
                              "org-cycle-hide-drawers:  "
                              "`:END:`"
                              " line missing at position %s")
                             (1+ start))))
                  (if (re-search-forward "^[ \t]*:END:" limit t)
                      (outline-flag-region start (point-at-eol) t)
                    (user-error msg))))))))))

  (defun eli/org-expand-all ()
    (interactive)
    (org-show-subtree)
    (org-unlogged-message "ALL")
    (setq org-cycle-subtree-status 'all))

  (defun eli/clock-in-to-nest (kw)
    (if (org-get-todo-state)
	"STARTED"))

  (defun eli/get-tag-counts ()
    (interactive)
    (let ((all-tags '()))
      (org-map-entries
       (lambda ()
	 (let ((tag-string (car (last (org-heading-components)))))
	   (when tag-string
	     (setq all-tags
		   (append all-tags (split-string tag-string ":" t)))))) "+LEVEL=1")
      (list (completing-read "Select a tag:" all-tags))))
  (defun eli/entry-rating ()
    (interactive)
    (let* ((eli/temp)
	   (eli/rate))
      (setq eli/temp (org-map-entries (lambda () (string-to-number (if (org-entry-get nil "Rating") (org-entry-get nil "Rating") "0"))) "+Rating>=0" `tree))
      (pop eli/temp)
      (setq eli/rate (if (= (length eli/temp) 0) 0 (/ (apply `+  eli/temp) (length eli/temp))))
      (org-set-property "Rating" (format "%.2f" eli/rate))))
  (defun eli/rating (type)
    (interactive (eli/get-tag-counts))
    (org-map-entries 'eli/entry-rating (concat type "+LEVEL=2/!-DONE-CANCELLED")))
  (global-set-key (kbd "<f8>") 'eli/rating)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-in-switch-to-state `eli/clock-in-to-nest)

  ;; latex
  (setq org-preview-latex-default-process 'dvisvgm)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (setq org-format-latex-options '(:foreground default :background default
                                               :scale 1.5 :html-foreground "Black"
                                               :html-background "Transparent"
                                               :html-scale 1.0
                                               :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  ;; pdf exporting
  (setq org-preview-latex-process-alist
        '((dvisvgm :programs
                   ("xelatex" "dvisvgm")
                   :description "xdv > svg" :message "you need to install the programs: xelatex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
                   (1.7 . 1.5)
                   :latex-compiler
                   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("xelatex" "convert")
                       :description "pdf > png" :message "you need to install the programs: xelatex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  (setq org-latex-listings 'minted)
  (setq org-latex-compiler "xelatex")
  ;; (add-to-list 'org-latex-packages-alist
  ;;            '("UTF8" "ctex" t))
  (add-to-list 'org-latex-packages-alist
	       '("cache=false" "minted" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "xcolor" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "tikz"))
  (setq org-latex-pdf-process
	'("xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
	  "biber %b"
	  "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
	  "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
	  "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
	  ))
  )
(use-package ox-beamer
  :after org
  :defer t)
(use-package org-tempo
  :after org
  :defer t)
(use-package ox-latex
  :after org
  :defer t
  :config
  (add-to-list 'org-latex-classes
	       '("beamer"
		 "\\documentclass[ignorenonframetext,presentation]{beamer}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}"))
	       )
  (add-to-list 'org-latex-classes
	       '("article_cn"
		 "\\documentclass[11pt]{ctexart}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	       ))

(use-package org-mind-map
  :ensure t
  :after org
  :defer t
  :init
  (use-package ox-org
    :after org-mind-map)
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(use-package org-habit
  :after org
  :init
  (setq org-habit-graph-column 1)
  (setq org-habit-preceding-days 10)
  (setq org-habit-following-days 2)
  (setq org-habit-show-habits-only-for-today nil)
  :config
  (defun org-habit-parse-todo (&optional pom)
    "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed
  5: Repeater type as a string

This list represents a \"habit\" for the rest of this module."
    (save-excursion
      (if pom (goto-char pom))
      (cl-assert (org-is-habit-p (point)))
      (let* ((scheduled (org-get-scheduled-time (point)))
	     (scheduled-repeat (org-get-repeat (org-entry-get (point) "SCHEDULED")))
	     (end (org-entry-end-position))
	     (habit-entry (org-no-properties (nth 4 (org-heading-components))))
	     closed-dates deadline dr-days sr-days sr-type)
	(if scheduled
	    (setq scheduled (time-to-days scheduled))
	  (error "Habit %s has no scheduled date" habit-entry))
	(unless scheduled-repeat
	  (error
	   "Habit `%s' has no scheduled repeat period or has an incorrect one"
	   habit-entry))
	(setq sr-days (org-habit-duration-to-days scheduled-repeat)
	      sr-type (progn (string-match "[\\.+]?\\+" scheduled-repeat)
			     (match-string-no-properties 0 scheduled-repeat)))
	(unless (> sr-days 0)
	  (error "Habit %s scheduled repeat period is less than 1d" habit-entry))
	(when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	  (setq dr-days (org-habit-duration-to-days
			 (match-string-no-properties 1 scheduled-repeat)))
	  (if (<= dr-days sr-days)
	      (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
		     habit-entry scheduled-repeat))
	  (setq deadline (+ scheduled (- dr-days sr-days))))
	(org-back-to-heading t)
	(let* ((maxdays 99999)
	       (reversed org-log-states-order-reversed)
	       (search (if reversed 're-search-forward 're-search-backward))
	       (limit (if reversed end (point)))
	       (count 0)
	       (re (format
		    "^[ \t]*-[ \t]+\\(?:State \"%s\".*%s%s\\)"
		    (regexp-opt org-done-keywords)
		    org-ts-regexp-inactive
		    (let ((value (cdr (assq 'done org-log-note-headings))))
		      (if (not value) ""
			(concat "\\|"
				(org-replace-escapes
				 (regexp-quote value)
				 `(("%d" . ,org-ts-regexp-inactive)
				   ("%D" . ,org-ts-regexp)
				   ("%s" . "\"\\S-+\"")
				   ("%S" . "\"\\S-+\"")
				   ("%t" . ,org-ts-regexp-inactive)
				   ("%T" . ,org-ts-regexp)
				   ("%u" . ".*?")
				   ("%U" . ".*?")))))))))
	  (unless reversed (goto-char end))
	  (while (and (< count maxdays) (funcall search re limit t))
	    (push (time-to-days
		   (org-time-string-to-time
		    (or (match-string-no-properties 1)
			(match-string-no-properties 2))))
		  closed-dates)
	    (setq count (1+ count))))
	(list scheduled sr-days deadline dr-days closed-dates sr-type))))

  (defun eli-habit-streaks (habit)
    (interactive)
    (let ((closed-days (nth 4 habit))
	  (counter 1)
	  (sum (length (nth 4 habit)))
	  (streaks 1)
	  (current-streaks 0)
	  (today (time-to-days (current-time)))
	  (max-streaks 1)
	  )
      (while (< counter (length closed-days))
	(if (= (time-convert (time-subtract (nth  counter closed-days) (nth (1- counter) closed-days)) 'integer) 1)
	    (progn (setq streaks (1+ streaks)))
	  (if (> streaks max-streaks)
	      (progn (setq max-streaks streaks)
		     (setq streaks 1)))
	  )
	(setq counter (1+ counter)))
      (setq counter (1- counter))
      (if (= (time-convert (time-subtract today (nth counter closed-days)) 'integer) 1)
	  (progn (setq current-streaks (1+ current-streaks))
		 (while (= (time-convert (time-subtract (nth  counter closed-days) (nth (1- counter) closed-days)) 'integer) 1)
		   (setq current-streaks (1+ current-streaks))
		   (setq counter (1- counter)))
		 )

	)
      (if (> streaks max-streaks)
	  (setq max-streaks streaks))
      (insert " (" (number-to-string current-streaks) "/" (number-to-string max-streaks) "/" (number-to-string sum) ")")
      ))
  (defun org-habit-insert-consistency-graphs (&optional line)
    "Insert consistency graph for any habitual tasks."
    (let ((inhibit-read-only t)
	  (buffer-invisibility-spec '(org-link))
	  (moment (org-time-subtract nil
				     (* 3600 org-extend-today-until))))
      (save-excursion
	(goto-char (if line (point-at-bol) (point-min)))
	(while (not (eobp))
	  (let ((habit (get-text-property (point) 'org-habit-p)))
	    (when habit
	      (move-to-column org-habit-graph-column t)
	      (delete-char (min (+ 1 org-habit-preceding-days
				   org-habit-following-days)
				(- (line-end-position) (point))))
	      (insert-before-markers
	       (org-habit-build-graph
		habit
		(time-subtract moment (days-to-time org-habit-preceding-days))
		moment
		(time-add moment (days-to-time org-habit-following-days))))
	      (end-of-line)
	      (eli-habit-streaks habit)
	      ))
	  (forward-line)))))
  )

;; org todo keaywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t/!)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "NEXT(n!/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))

;; disable company-mode in org-mode
;; (defun eli/org-mode-hook ()
;;   (set (make-local-variable 'company-dabbrev-char-regexp)
;;        "^[\\.0-9a-z-_'/]")
;;   (make-local-variable 'company-backends)
;;   (flycheck-mode -1)
;;   )
;; (add-hook 'org-mode-hook 'eli/org-mode-hook)

;; org-protocol
(use-package org-protocol
  :defer 5
  ;; :hook (after-init . (server-start))
  :config
  (server-start))


;; org reverse tree
(use-package org-reverse-datetree
  :ensure t
  :defer t)

;;org capture
(setq org-agenda-dir "~/Dropbox/org")
(setq org-directory "~/Dropbox/org")

(setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
(setq org-agenda-file-projects (expand-file-name "projects.org" org-agenda-dir))
(setq org-agenda-file-habit (expand-file-name "habits.org" org-agenda-dir))
(setq org-agenda-file-notes (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippets (expand-file-name "snippets.org" org-agenda-dir))
(setq org-agenda-file-te (expand-file-name "words.org" org-agenda-dir))
(setq org-agenda-file-lists (expand-file-name "lists.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-dir))
(defun eli/capture-report-date-file ()
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s-%s.org"
                              (format-time-string "%Y-%m-%d")
                              name) "~/Dropbox/org/blog")))

;; add a property to create id
(defun eli/org-capture-maybe-create-id ()
  (when (org-capture-get :create-id)
    (org-id-get-create)))
(add-hook 'org-capture-prepare-finalize-hook #'eli/org-capture-maybe-create-id)

;; from: https://stackoverflow.com/questions/21073859/is-there-a-way-with-org-capture-templates-to-not-insert-a-line-if-initial-conten
(defun v-i-or-nothing ()
  (let ((v-i (plist-get org-store-link-plist :initial)))
    (if (equal v-i "")
        ""
      (concat "\n#+begin_quote\n" v-i "\n#+end_quote\n"))))

(defun v-a-or-nothing ()
  (let ((v-a (plist-get org-store-link-plist :annotation)))
    (if (equal v-a "")
        ""
      (concat "- reference :: " v-a))))

;; better fill region in capture
(defun eli/fill-region ()
  (save-excursion
    (push-mark)
    (push-mark (point-max) nil t)
    (goto-char (minibuffer-prompt-end))
    (org-fill-paragraph nil t)))
(add-hook 'org-capture-prepare-finalize-hook 'eli/fill-region)

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file org-agenda-file-inbox)
         "* TODO %?\n\n%i\n%U"
         :empty-lines 0)
        ("p" "Project" entry (file org-agenda-file-projects)
         "* PROJECT %?"
         :empty-lines 0)
        ("h" "Habit" entry (file org-agenda-file-habit)
         "* TODO %?\nSCHEDULED: <%(org-read-date nil nil \"+0d\") .+1d>\n:PROPERTIES:\n:STYLE:    habit\n:END:\n\n%U"
         :empty-lines 0)
        ("n" "Notes" entry (file+headline org-agenda-file-inbox "Notes")
         "* %?\n%(v-i-or-nothing)\n%(v-a-or-nothing)\n%U"
         :empty-lines 0
	 :prepend t)
        ("j" "Journals" entry (file+olp+datetree org-agenda-file-journal)
         "* %<%H:%M> %?"
         :empty-lines 0
	 :prepend t
	 ;; :clock-in t
	 ;; :clock-resume t
	 )
	("e" "Events" entry (file "~/Elilif.github.io/2021-07-20-Eli's timeline.org")
	 "* %<%Y-%m-%d>\n%?"
	 :prepend t)
	("B" "Blogs" plain (file eli/capture-report-date-file)
	 "#+TITLE: %?\n#+DATE: %<%Y-%m-%d>\n#+STARTUP: showall\n#+OPTIONS: toc:nil H:2 num:2\n"
	 )
	("T" "Time Report" plain (file+function "~/Dropbox/org/Clock_Report.org"  org-reverse-datetree-goto-date-in-file)
	 "#+BEGIN: clocktable :scope agenda-with-archives :maxlevel 6 :block %<%Y-%m-%d> :fileskip0 t :indent t :link t\n#+END:"
	 :empty-lines 0
	 :jump-to-captured t)
        ;; ("d" "Digests" entry (file+olp+datetree org-agenda-file-notes)
        ;;  "* %a \n%i \n%U"
        ;;  :empty-lines 0)
	("w" "Words" entry (file org-agenda-file-te)
	 "* TODO %u [/]\nSCHEDULED: <%(org-read-date nil nil \"+1d\") .+1d>\n%?"
	 :jump-to-captured t)
	("b" "Book" entry (file+headline org-agenda-file-lists "Books")
	 "* TODO %?\n  %^{Title}p %^{Isbn}p %^{Types}p %^{Authors}p %^{Translator}p %^{Publisher}p %^{Nation}p %^{Lang}p %^{Rating}p")
	("m" "Movies and Musicals" entry (file+headline org-agenda-file-lists "Movies and Musicals")
	 "* TODO %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p %^{Actors}p %^{Types}p %^{Time}p %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p")
	("s" "Series" entry (file+headline org-agenda-file-lists "Series")
	 "* TODO %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p %^{Actors}p %^{Types}p %^{Time}p %^{Episodes}p %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p")
	("c" "Animes" entry (file+headline org-agenda-file-lists "Animes")
	 "* TODO %?\n %^{Title}p %^{URL}p %^{Episodes}p %^{Release}p %^{Director}p %^{Authors}p %^{Publisher}p %^{Rating}p")
	("r" "NOTE" entry (file "~/Dropbox/org/roam/inbox.org")
	 "* %?\n%(v-i-or-nothing)\n%(v-a-or-nothing)"
	 :create-id t)
        ))


(advice-add 'org-time-stamp :around
            (lambda (fn &rest args)
              (apply fn args)
              (when (string-match "\\\\\\\([\\.\\+\\-].*\\)" org-read-date-final-answer)
                (save-excursion
                  (backward-char)
                  (insert " "
                          (string-trim-right
                           (match-string 1 org-read-date-final-answer)))))
              ))

(advice-add 'org--deadline-or-schedule :around
            (lambda (fn &rest args)
              (apply fn args)
              (when (string-match "\\\\\\\([\\.\\+\\-].*\\)" org-read-date-final-answer)
                (save-excursion
                  (next-line)
                  (move-end-of-line 1)
                  (backward-char)
                  (insert " "
                          (string-trim-right
                           (match-string 1 org-read-date-final-answer)))))
              ))

;;; time report
;;; from: https://emacs.stackexchange.com/questions/31683/schedule-org-task-for-last-day-of-every-month
;;; ORG-MODE:  * My Task
;;;              SCHEDULED: <%%(diary-last-day-of-month date)>
;;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
;;; See also:  (setq org-agenda-include-diary t)
;;; (diary-last-day-of-month '(2 28 2017))
(defun diary-last-day-of-month (date)
"Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
            (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

;; appt
(use-package appt
  :after org
  :defer t
  :hook (org-agenda-finalize . eli/org-agenda-to-appt)
  :config
  ;; 每小时同步一次appt,并且现在就开始同步
  (run-at-time nil 3600 'org-agenda-to-appt t)
  ;; 更新agenda时，同步appt
  (defun eli/org-agenda-to-appt ()
    "call org-agenda-to-appt with refresh."
    (org-agenda-to-appt t))
  ;; 激活提醒
  (appt-activate 1)
  ;; 提前半小时提醒
  (setq appt-message-warning-time 30)
  (setq appt-display-interval 5)
  (use-package notifications
    :config
    (defun appt-disp-window-and-notification (min-to-appt current-time appt-msg)
      (if (atom min-to-appt)
	  (notifications-notify :timeout (* appt-display-interval 60000) ;一直持续到下一次提醒
				:title (format "%s分钟内有新的任务" min-to-appt)
				:body appt-msg)
	(dolist (i (number-sequence 0 (1- (length min-to-appt))))
	  (notifications-notify :timeout (* appt-display-interval 60000) ;一直持续到下一次提醒
				:title (format "%s分钟内有新的任务" (nth i min-to-appt))
				:body (nth i appt-msg))))
    )
    ;; (appt-disp-window min-to-appt current-time appt-msg)
    ) ;同时也调用原有的提醒函数
  (setq appt-display-format 'window) ;; 只有这样才能使用自定义的通知函数
  (setq appt-disp-window-function #'appt-disp-window-and-notification)
  )

;; org-refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;; custom org agenda view
(setq org-agenda-log-mode-items '(clock))
(setq org-agenda-log-mode-add-notes nil)

(setq org-agenda-custom-commands
      '(("g" "GTD"
         ((agenda "" nil)
          (tags-todo  "/+TODO"
                 ((org-agenda-overriding-header "Inbox")
                  (org-tags-match-list-sublevels t)
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
          (tags-todo "/+NEXT"
                     ((org-agenda-overriding-header "Next")))
          (tags-todo "/+STARTED"
                     ((org-agenda-overriding-header "Started")))
          (tags-todo "/+PROJECT"
                     ((org-agenda-overriding-header "Projects")))
          (tags-todo "/+WAITING"
                     ((org-agenda-overriding-header "Waiting")))
          (tags-todo "/+SOMEDAY"
                     ((org-agenda-overriding-header "Someday/Maybe")))
          ))))

;; a TODO entry automatically change to DONE when all children are done
(defun eli/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'eli/org-summary-todo)

;; rime
(use-package rime
  :ensure t
  :defer t
  :init
  (defun +rime-predicate-punctuation-line-begin-p ()
    "Enter half-width punctuation at the beginning of the line.
  Detect whether the current cursor is at the beginning of a
  line and the character last inputted is symbol.
  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (<= (point) (save-excursion (back-to-indentation) (point))))
  :hook
  ;; ('kill-emacs . (lambda ()
  ;;                  (when (fboundp 'rime-lib-sync-user-data)
  ;;                    (ignore-errors (rime-sync)))))
  (org-mode . toggle-input-method)
  :custom
  ((default-input-method "rime")
   (rime-user-data-dir "~/.emacs.d/rime")
   (rime-disable-predicates '(rime-predicate-prog-in-code-p
                              rime-predicate-space-after-ascii-p
                              rime-predicate-after-ascii-char-p
                              +rime-predicate-punctuation-line-begin-p
                              rime-predicate-org-in-src-block-p
                              rime-predicate-space-after-cc-p
			      rime-predicate-current-uppercase-letter-p
			      rime-predicate-hydra-p
                              ))
   ;; (rime-inline-predicates '(rime-predicate-space-after-cc-p)))
   )
  :config
  (setq eli/prefer-English t)
  (defun eli/input-switch ()
    (interactive)
    (if (not eli/prefer-English)
	(progn
	  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                              rime-predicate-space-after-ascii-p
                              rime-predicate-after-ascii-char-p
                              +rime-predicate-punctuation-line-begin-p
                              rime-predicate-org-in-src-block-p
                              rime-predicate-space-after-cc-p
                              rime-predicate-current-uppercase-letter-p
                              rime-predicate-hydra-p ))
	  (setq eli/prefer-English t))
      (progn
       (setq rime-disable-predicates
	    (seq-difference rime-disable-predicates '(rime-predicate-space-after-ascii-p
				       +rime-predicate-punctuation-line-begin-p)))
       (setq eli/prefer-English nil)
       )))
  (defun +rime-convert-string-at-point (&optional return-cregexp)
    "将光标前的字符串转换为中文."
    (interactive "P")
    (rime-force-enable)
    (let ((string (if mark-active
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (point) (max (line-beginning-position) (- (point) 80)))))
          code
          length)
      (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
             (setq code (replace-regexp-in-string
			 "^[-']" ""
			 (match-string 0 string)))
             (setq length (length code))
             (setq code (replace-regexp-in-string " +" "" code))
             (if mark-active
		 (delete-region (region-beginning) (region-end))
               (when (> length 0)
		 (delete-char (- 0 length))))
             (when (> length 0)
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))))
            (t (message "`+rime-convert-string-at-point' did nothing.")))))
  
  (setq default-input-method "rime"
	rime-show-candidate 'nil)
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-inline-ascii-trigger 'shift-l)
  (global-set-key (kbd "C-s-k") 'rime-inline-ascii)
  (global-set-key (kbd "C-s-j") '+rime-convert-string-at-point)
  )

(use-package org-superstar
  :ensure t
  :defer t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("☰" "○" "✸" "✤" "◆" "✜" "▶"))
  )

;;roam
(use-package org-roam
  :ensure t
  :defer t
  :init
  ;; :hook (after-init . org-roam-db-autosync-mode)
  :config
  (setq org-roam-database-connector 'sqlite-builtin)
  ;;  dynamically add roam files with TODO entry into agenda files
  (defvar dynamic-agenda-files nil
    "dynamic generate agenda files list when changing org state")

  (defun update-dynamic-agenda-hook ()
    (let ((done (or (not org-state) ;; nil when no TODO list
                    (member org-state org-done-keywords)))
          (file (buffer-file-name))
          (agenda (funcall (ad-get-orig-definition 'org-agenda-files)) ))
      (unless (member file agenda)
	(if done
            (save-excursion
              (goto-char (point-min))
              ;; Delete file from dynamic files when all TODO entry changed to DONE
              (unless (search-forward-regexp org-not-done-heading-regexp nil t)
		(customize-save-variable
		 'dynamic-agenda-files
		 (cl-delete-if (lambda (k) (string= k file))
                               dynamic-agenda-files))))
          ;; Add this file to dynamic agenda files
          (unless (member file dynamic-agenda-files)
            (customize-save-variable 'dynamic-agenda-files
                                     (add-to-list 'dynamic-agenda-files file)))))))

  (defun dynamic-agenda-files-advice (orig-val)
    (cl-union orig-val dynamic-agenda-files :test #'equal))

  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)
  (add-to-list 'org-after-todo-state-change-hook 'update-dynamic-agenda-hook t)

  (setq org-roam-directory "~/Dropbox/org/roam/")
  (setq org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id 'create-if-interactive)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer)))
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section
	      ;; #'org-roam-unlinked-references-section
	      ))
  (setq org-roam-completion-everywhere t)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
	(concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
           "* %?"
           :if-new (file+datetree "~/Dropbox/org/roam/daily/dailies.org" day))))
  (setq org-roam-capture-templates '(("f" "file" plain "%?"
                                      :if-new (file+head "main/%<%Y%m%d%H%M%S>.org"
							 "#+TITLE: ${title}\n")
                                      :unnarrowed t)
				     ("b" "bibliography reference" plain
				      (file "~/.emacs.d/private/orb-capture-template.org")
				      :if-new (file+head "references/${citekey}.org" "#+title: ${title}\n")
				      )
				     ("r" "reference" plain "%? \n %(v-i-or-nothing) \n\n%(v-a-or-nothing)"
				      :if-new
				      (file+head "references/%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
				      :unnarrowed t)))

  (org-roam-db-autosync-mode)
  ;; (require 'org-roam-protocol)
  (use-package org-roam-protocol
    :after org-roam)
  )


;;----------------------------------------------------------------------------
;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-client-id "1055740533302-i0kv050tfr5sv6k5j39f1r7fa2smlcqg.apps.googleusercontent.com"
;; 	org-gcal-client-secret "4OkssNTRTN8b5Atrc0unbcWg"
;; 	org-gcal-fetch-file-alist '(("eli.q.qian@gmail.com" .  "~/Dropbox/org/test.org")
;;                                     ;; ("another-mail@gmail.com" .  "~/task.org")
;; 				    )))

(use-package helm-org
  :ensure t
  :after org
  :defer t
  :config
  (defun yuchen/helm-org-run-marked-heading-id-link ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action
       'yuchen/helm-org-marked-heading-id-link)))

  (defun yuchen/helm-org-marked-heading-id-link (marker)
    (let* ((victims (with-helm-buffer (helm-marked-candidates)))
           (buffer (marker-buffer marker))
           (filename (buffer-file-name buffer))
           (rfloc (list nil filename nil marker)))
      (when (and (= 1 (length victims))
                 (equal (helm-get-selection) (car victims)))
        ;; No candidates are marked; we are refiling the entry at point
        ;; to the selected heading
        (setq victims (list marker)))
      (when (and victims buffer filename rfloc)
        (cl-loop for victim in victims
                 ;; do (org-with-point-at victim
                 ;;      (org-refile nil nil rfloc))

                 do (with-current-buffer (marker-buffer victim)
		      (let ((heading-id (save-excursion (goto-char (marker-position victim))
							(org-id-get-create)
							))
			    (heading-name
			     (save-excursion
			       (goto-char (marker-position victim))
			       (org-entry-get nil "ITEM"))
			     )
			    )
			(with-helm-current-buffer
			  (org-insert-link
			   nil (concat "id:" heading-id) heading-name)
			  (insert " ")
			  )))
		 ))))
  (add-to-list 'helm-org-headings-actions '("Insert id link(s) C-c v" . yuchen/helm-org-marked-heading-id-link) t)
  (define-key helm-org-headings-map (kbd "C-c v") 'yuchen/helm-org-run-marked-heading-id-link)
  )

(use-package helm-org-rifle
  :ensure t
  :after org
  :defer t
  :config)

(use-package writeroom-mode
  :ensure t
  :defer t)

(use-package writegood-mode
  :ensure t
  :defer t)

(use-package notdeft
  :load-path "~/.emacs.d/private/notdeft"
  :defer t
  :commands (notdeft)
  :config
  (setq notdeft-allow-org-property-drawers t)
  (setq notdeft-xapian-max-results 0)
  (setq notdeft-xapian-program "/home/eli/.emacs.d/private/notdeft/xapian/notdeft-xapian")
  (setq notdeft-directories '("~/Dropbox/org/roam"))
  )

;; uncompatible with emacs 28
;; (use-package org-ql
;;   :ensure t
;;   :after org)

;; (use-package easy-hugo
;;   :ensure t
;;   :config
;;   (setq easy-hugo-basedir "~/Elilif.github.io")
;;   (setq easy-hugo-url "https://elilif.github.io")
;;   (setq easy-hugo-default-ext ".org"))

(use-package ox-timeline
  :ensure t
  :defer t
  :config
  (setq org-timeline-source-url "dist")
  )

(use-package org-pomodoro
  :ensure t
  :defer t
  :config
  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (org-notify "A pomodoro is finished, take a break !!!")
              ))
  (add-hook 'org-pomodoro-short-break-finished-hook
            (lambda ()
              (org-notify "A short break done, ready a new pomodoro !!!")
              ))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda ()
              (org-notify "A long break done, ready a new pomodoro !!!")
              ))
  )
(use-package org-contrib
  :ensure t
  :after org
  :defer 5
  :config
  (require 'org-link-edit))

(use-package ox-pandoc
  :ensure t
  :after ox
  :config
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  (defun org-pandoc-link (link contents info)
    "Transcode a LINK object.

The registered formatter for the 'pandoc backend is used. If none
exists, transcode using the registered formatter for the 'org
export backend. For fuzzy (internal) links, resolve the link
destination in order to determine the appropriate reference
number of the target Table/Figure/Equation etc. CONTENTS is the
description of the link, as a string, or nil. INFO is a plist
holding contextual information."
    (let ((type (org-element-property :type link)))
      (cond
       ;; Try exporting with a registered formatter for 'pandoc
       ((org-export-custom-protocol-maybe link contents 'pandoc))
       ;; Try exporting with a registered formatter for 'org
       ((org-export-custom-protocol-maybe link contents 'org))

       ;; Otherwise, override fuzzy (internal) links that point to
       ;; numbered items such as Tables, Figures, Sections, etc.
       ((string= type "fuzzy")
	(let* ((path (org-element-property :path link))
               (destination (org-export-resolve-fuzzy-link link info))
               (dest-type (when destination (org-element-type destination)))
               (number nil))
          ;; Different link targets require different predicates to the
          ;; `org-export-get-ordinal' function in order to resolve to
          ;; the correct number. NOTE: Should be the same predicate
          ;; function as used to generate the number in the
          ;; caption/label/listing etc.
          (cond
           ((eq dest-type 'paragraph)   ; possible figure
            (setq number (org-export-get-ordinal
                          destination info nil #'org-html-standalone-image-p)))

           ((eq dest-type 'latex-environment)
            (setq number (org-export-get-ordinal
                          destination info nil #'org-pandoc--numbered-equation-p)))

           ((eq dest-type 'has-caption)                           ; captioned items
            (setq number (org-export-get-ordinal
                          destination info nil #'org-pandoc--has-caption-p))
	    ))

          ;; Numbered items have the number listed in the link
          ;; description, , fall back on the text in `contents'
          ;; if there's no resolvable destination
          (cond
           ;; Numbered items have the number listed in the link description
           (number
            (format "[[#%s][%s]]" path
                    (if (atom number) (number-to-string number)
                      (mapconcat #'number-to-string number ".")))
	    )

           ;; Unnumbered headlines have the heading name in the link
           ;; description
           ((eq dest-type 'headline)
            (format "[[#%s][%s]]" path
                    (org-export-data
                     (org-element-property :title destination) info)))

           ;; No resolvable destination, fallback on the text in `contents'
           ((eq destination nil)
            (when (org-string-nw-p contents) contents))

           ;; Valid destination, but without a numbered caption/equation
           ;; and not a heading, fallback to standard org-mode link format
           (t
            (org-element-link-interpreter link contents)))))

       ;; Otherwise, fallback to standard org-mode link format
       ((org-element-link-interpreter link contents)))))
  )

(use-package org-mru-clock
  :ensure t
  :defer t
  :config
  (setq org-mru-clock-how-many 30)
  (setq org-mru-clock-files #'org-agenda-files)
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

(use-package org-clock-convenience
  :ensure t
  :defer t
  :bind (:map org-agenda-mode-map
	      ("M-<up>" . org-clock-convenience-timestamp-up)
	      ("M-<down>" . org-clock-convenience-timestamp-down)
	      ("<f6>" . org-clock-convenience-fill-gap)
	      ("<f7>" . org-clock-convenience-fill-gap-both)))
(provide 'init-org)
