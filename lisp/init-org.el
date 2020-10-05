(use-package org
  :ensure t
  :hook ((org-mode . org-indent-mode)
	 (org-mode . auto-fill-mode))
  :bind(("\C-c c" . 'org-capture)
	("\C-c a" . 'org-agenda))
  :config
  (setq org-src-fontify-natively t)) 

(use-package org-habit
  :init
  (setq org-habit-graph-column 1)
  (setq org-habit-preceding-days 10)
  (setq org-habit-following-days 2)
  (setq org-habit-show-habits-only-for-today nil))

;; org todo keaywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "NEXT(n)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")

;;org capture
(setq org-agenda-dir "~/Dropbox/org")

(setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
(setq org-agenda-file-projects (expand-file-name "projects.org" org-agenda-dir))
(setq org-agenda-file-habit (expand-file-name "habits.org" org-agenda-dir))
(setq org-agenda-file-notes (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippets (expand-file-name "snippets.org" org-agenda-dir))
(setq org-agenda-file-posts (expand-file-name "posts.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-dir))

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file org-agenda-file-inbox)
         "* TODO %? \n\n%i \n%U"
         :empty-lines 1)
        ("p" "Project" entry (file org-agenda-file-projects)
         "* PROJECT %? "
         :empty-lines 1)
        ("h" "Habit" entry (file org-agenda-file-habit)
         "* TODO %? \n  :PROPERTIES:\n  :STYLE:    habit\n  :END:\n\n%U"
         :empty-lines 1)
        ("n" "Notes" entry (file+headline org-agenda-file-inbox "Notes")
         "* %? \n\n%a \n%i \n%U"
         :empty-lines 1)
        ("j" "Journals" entry (file+datetree org-agenda-file-journal)
         "* %? "
         :empty-lines 1)
        ("d" "Digests" entry (file+datetree org-agenda-file-notes)
         "* %a \n%i \n%U"
         :empty-lines 1)
        ("l" "Chrome" entry (file+headline org-agenda-file-inbox "Notes")
         "* %? \n%i \n\n%:annotation \n\n%U"
         :empty-lines 1)
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




(provide 'init-org)
