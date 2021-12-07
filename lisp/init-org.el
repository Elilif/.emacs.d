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
	 )
  :config
  (use-package org-inlinetask
    :defer 5)
  (use-package org-mu4e
    :defer t)
  (setq org-clock-sound "~/.emacs.d/private/bellring.wav")
  (setq org-src-fontify-natively t)
  (setq org-agenda-span 'day)
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

  (defun org-cycle-internal-local ()
    "Do the local cycling action."
    (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
      ;; First, determine end of headline (EOH), end of subtree or item
      ;; (EOS), and if item or heading has children (HAS-CHILDREN).
      (save-excursion
	(if (org-at-item-p)
	    (progn
	      (beginning-of-line)
	      (setq struct (org-list-struct))
	      (setq eoh (point-at-eol))
	      (setq eos (org-list-get-item-end-before-blank (point) struct))
	      (setq has-children (org-list-has-child-p (point) struct)))
	  (org-back-to-heading)
	  (setq eoh (save-excursion (outline-end-of-heading) (point)))
	  (setq eos (save-excursion
		      (org-end-of-subtree t t)
		      (unless (eobp) (forward-char -1))
		      (point)))
	  (setq has-children
		(or
		 (save-excursion
		   (let ((level (funcall outline-level)))
		     (outline-next-heading)
		     (and (org-at-heading-p t)
			  (> (funcall outline-level) level))))
		 (and (eq org-cycle-include-plain-lists 'integrate)
		      (save-excursion
			(org-list-search-forward (org-item-beginning-re) eos t))))))
	;; Determine end invisible part of buffer (EOL)
	(beginning-of-line 2)
	(while (and (not (eobp))		;this is like `next-line'
		    (get-char-property (1- (point)) 'invisible))
	  (goto-char (next-single-char-property-change (point) 'invisible))
	  (and (eolp) (beginning-of-line 2)))
	(setq eol (point)))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
	;; Nothing is hidden behind this heading
	(unless (org-before-first-heading-p)
	  (run-hook-with-args 'org-pre-cycle-hook 'empty))
	(org-unlogged-message "EMPTY ENTRY")
	(setq org-cycle-subtree-status nil)
	(save-excursion
	  (goto-char eos)
	  (outline-next-heading)
	  (when (org-invisible-p) (org-flag-heading nil))))
       ((and (or (>= eol eos)
		 (not (string-match "\\S-" (buffer-substring eol eos))))
	     (or has-children
		 (not (setq children-skipped
			    org-cycle-skip-children-state-if-no-children))))
	;; Entire subtree is hidden in one line: children view
	(unless (org-before-first-heading-p)
	  (run-hook-with-args 'org-pre-cycle-hook 'children))
	(if (org-at-item-p)
	    (org-list-set-item-visibility (point-at-bol) struct 'children)
	  (org-show-entry)
	  (org-with-limited-levels (org-show-children))
	  (org-show-set-visibility 'tree)
	  ;; Fold every list in subtree to top-level items.
	  (when (eq org-cycle-include-plain-lists 'integrate)
	    (save-excursion
	      (org-back-to-heading)
	      (while (org-list-search-forward (org-item-beginning-re) eos t)
		(beginning-of-line 1)
		(let* ((struct (org-list-struct))
		       (prevs (org-list-prevs-alist struct))
		       (end (org-list-get-bottom-point struct)))
		  (dolist (e (org-list-get-all-items (point) struct prevs))
		    (org-list-set-item-visibility e struct 'folded))
		  (goto-char (if (< end eos) end eos)))))))
	(org-unlogged-message "CHILDREN")
	(save-excursion
	  (goto-char eos)
	  (outline-next-heading)
	  (when (org-invisible-p) (org-flag-heading nil)))
	(setq org-cycle-subtree-status 'children)
	(unless (org-before-first-heading-p)
	  (run-hook-with-args 'org-cycle-hook 'children)))
       ((eq org-cycle-subtree-status 'subtree)
	(org-show-subtree)
	(org-unlogged-message "ALL")
	(setq org-cycle-subtree-status 'all))
       ((or children-skipped
	    (and (eq last-command this-command)
		 (eq org-cycle-subtree-status 'children)))
	;; We just showed the children, or no children are there,
	;; now show everything.
	(unless (org-before-first-heading-p)
	  (run-hook-with-args 'org-pre-cycle-hook 'subtree))
	(org-flag-region eoh eos nil 'outline)
	(org-unlogged-message
	 (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
	(setq org-cycle-subtree-status 'subtree)
	(unless (org-before-first-heading-p)
	  (run-hook-with-args 'org-cycle-hook 'subtree)))
       (t
	;; Default action: hide the subtree.
	(run-hook-with-args 'org-pre-cycle-hook 'folded)
	(org-flag-region eoh eos t 'outline)
	(org-unlogged-message "FOLDED")
	(setq org-cycle-subtree-status 'folded)
	(unless (org-before-first-heading-p)
	  (run-hook-with-args 'org-cycle-hook 'folded))))))


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
  ;; pdf exporting
  ;; (setq org-latex-pdf-process
  ;;     '("xelatex -interaction nonstopmode -output-directory %o %f"
  ;;       "xelatex -interaction nonstopmode -output-directory %o %f"
  ;;       "xelatex -interaction nonstopmode -output-directory %o %f"
  ;; 		))
  ;; (setq org-latex-pdf-process (list "latexmk -pdf -bibtex %f"))
  (setq org-latex-pdf-process
	'("xelatex -interaction nonstopmode %f"
	  "bibtex %b"
	  "xelatex -interaction nonstopmode %f"
	  "xelatex -interaction nonstopmode %f"
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
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "NEXT(n)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))

;; disable company-mode in org-mode
(defun eli/org-mode-hook ()
  (set (make-local-variable 'company-dabbrev-char-regexp)
       "^[\\.0-9a-z-_'/]")
  (make-local-variable 'company-backends)
  (flycheck-mode -1))
(add-hook 'org-mode-hook 'eli/org-mode-hook)

;; org-protocol
(use-package org-protocol
  :defer 5
  ;; :hook (after-init . (server-start))
  :config
  (server-start))


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

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file org-agenda-file-inbox)
         "* TODO %? \n\n%i \n%U"
         :empty-lines 0)
        ("p" "Project" entry (file org-agenda-file-projects)
         "* PROJECT %? "
         :empty-lines 0)
        ("h" "Habit" entry (file org-agenda-file-habit)
         "* TODO %? \nSCHEDULED: <%(org-read-date nil nil \"+0d\") .+1d>\n  :PROPERTIES:\n  :STYLE:    habit\n  :END:\n\n%U"
         :empty-lines 0)
        ("n" "Notes" entry (file+headline org-agenda-file-inbox "Notes")
         "* %? \n\n%a \n\n%i \n\n%U"
         :empty-lines 0)
        ("j" "Journals" entry (file+olp+datetree org-agenda-file-journal)
         "* %<%H:%M> %? "
         :empty-lines 0
	 :clock-in t
	 :clock-resume t)
	("e" "Events" entry (file "~/Elilif.github.io/2021-07-20-Eli's timeline.org")
	 "* %<%Y-%m-%d>\n%?"
	 :prepend t)
	("B" "Blogs" plain (file eli/capture-report-date-file)
	 "#+TITLE: %?\n#+DATE: %<%Y-%m-%d>\n#+STARTUP: showall\n#+OPTIONS: toc:nil H:2 num:2\n"
	 )
        ("d" "Digests" entry (file+olp+datetree org-agenda-file-notes)
         "* %a \n%i \n%U"
         :empty-lines 0)
	("T" "TE" entry (file org-agenda-file-te)
	 "* TODO %u [/]\nSCHEDULED: <%(org-read-date nil nil \"+1d\") .+1d> \n %?"
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
	 "* %?\n%^{REFERENCE}p%i"
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

;; appt
(use-package appt
  :after org
  :defer t
  :hook ((org-agenda-mode . eli/org-agenda-to-appt)
	 (org-finalize-agenda . eli/org-agenda-to-appt))
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
  (global-set-key (kbd "M-s-k") 'rime-inline-ascii)
  (global-set-key (kbd "M-s-j") '+rime-convert-string-at-point)
  )

(use-package org-superstar
  :ensure t
  :defer t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("☰" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  )

;;roam
(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  ;; :hook (after-init . org-roam-db-autosync-mode)
  :config
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
  (setq org-roam-node-display-template "${file} > ${olp} > ${title:90}${tags:10}")
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
           "* %?"
           :if-new (file+datetree "~/Dropbox/org/roam/daily/dailies.org" day))))
  (setq org-roam-capture-templates '(("f" "file" plain "%?"
                                      :if-new (file+head "${slug}.org"
							 "#+TITLE: ${title}\n")
                                      :unnarrowed t)
				     ("r" "bibliography reference" plain
				      (file "~/.emacs.d/private/orb-capture-template.org")
				      :if-new (file+head "references/${citekey}.org" "#+title: ${title}\n")
				      )
				     ))

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

(provide 'init-org)
