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

(setq org-agenda-span 'day)
(setq org-agenda-window-setup 'only-window)

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

;; appt
(require 'appt)
;; 每小时同步一次appt,并且现在就开始同步
(run-at-time nil 3600 'org-agenda-to-appt)
;; 更新agenda时，同步appt
(add-hook 'org-agenda-mode-hook  'org-agenda-to-appt)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
;; 激活提醒
(appt-activate 1)
;; 提前半小时提醒
(setq appt-message-warning-time 30)
(setq appt-display-interval 5)
(require 'notifications)

(defun appt-disp-window-and-notification (min-to-appt current-time appt-msg)
  (if (atom min-to-appt)
      (notifications-notify :timeout (* appt-display-interval 60000) ;一直持续到下一次提醒
                            :title (format "%s分钟内有新的任务" min-to-appt)
                            :body appt-msg)
    (dolist (i (number-sequence 0 (1- (length min-to-appt))))
      (notifications-notify :timeout (* appt-display-interval 60000) ;一直持续到下一次提醒
                            :title (format "%s分钟内有新的任务" (nth i min-to-appt))
                            :body (nth i appt-msg))))
  ;; (appt-disp-window min-to-appt current-time appt-msg)
  ) ;同时也调用原有的提醒函数
(setq appt-display-format 'window) ;; 只有这样才能使用自定义的通知函数
(setq appt-disp-window-function #'appt-disp-window-and-notification)

(server-start)
(require 'org-protocol)

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
          (tags  "+INBOX/+TODO|+PROJECT/+TODO"
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
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; org-pdftools
(use-package org-pdftools
  :ensure t
  :hook (pdf-view-mode . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
;;   :ensure t
;;   :after org-noter
;;   :config
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; save last pdf position
(use-package saveplace-pdf-view
  :ensure t
  :config (save-place-mode 1))


;; rime
(defun +rime-predicate-is-back-quote-or-tilde ()
  (or (equal rime--current-input-key ?`)
      (equal rime--current-input-key ?~)))

(use-package rime
  :ensure t
  :hook
  ('kill-emacs . (lambda ()
                   (when (fboundp 'rime-lib-sync-user-data)
                     (ignore-errors (rime-sync)))))
  :custom
  ((default-input-method "rime")
   (rime-user-data-dir "~/.emacs.d/rime")
   (rime-disable-predicates '(rime-predicate-prog-in-code-p
                              rime-predicate-space-after-ascii-p
                              rime-predicate-after-ascii-char-p
                              rime-predicate-punctuation-line-begin-p
                              rime-predicate-org-in-src-block-p
                              rime-predicate-space-after-cc-p
                              ))
   ;; (rime-inline-predicates '(rime-predicate-space-after-cc-p))
   )
  )
(setq default-input-method "rime"
      rime-show-candidate 'posframe)
(setq mode-line-mule-info '((:eval (rime-lighter))))
(setq rime-inline-ascii-trigger 'shift-l)
(define-key rime-active-mode-map (kbd "M-s-k") 'rime-inline-ascii)
(define-key rime-mode-map (kbd "M-s-j") 'rime-force-enable)
(add-hook 'org-mode-hook 'toggle-input-method)

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))
;;roam
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/org-roam")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(require 'org-roam-protocol)
(org-roam-server-mode 1)
(add-to-list 'org-roam-capture-ref-templates
             '("a" "Annotation" plain (function org-roam-capture--get-point)
               "%U ${body}\n"
               :file-name "${slug}"
               :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
               :immediate-finish t
               :unnarrowed t))
(setq org-roam-capture-templates
      '(
        ("d" "default" plain #'org-roam-capture--get-point "%?" :file-name "${slug}" :head "#+roam_tags:
* ${title}" :unnarrowed t)
        ))
(provide 'init-org)
