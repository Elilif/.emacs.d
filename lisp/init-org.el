(use-package org
  :ensure t
  :hook ((org-mode . org-indent-mode)
	 (org-mode . auto-fill-mode)
	 (org-mode . column-number-mode)
	 (org-mode . prettify-symbols-mode))
  :bind(("\C-c c" . 'org-capture)
	("\C-c a" . 'org-agenda))
  :config
  (setq org-src-fontify-natively t)
  (setq org-agenda-span 'day)
  (setq org-agenda-window-setup 'only-window)
  ;; Change task state to STARTED when clocking in
  (setq org-log-into-drawer t)
  (setq org-startup-folded t)
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-babel-evaluate nil)
  (defun eli/clock-in-to-nest (kw)
    (if (org-get-todo-state)
	"STARTED"))

  (defun eli/entry-rating ()
    (let* ((eli/temp)
	   (eli/rate))
      (setq eli/temp (org-map-entries (lambda () (string-to-number (if (org-entry-get nil "Rating") (org-entry-get nil "Rating") "0"))) "+Rating>=0" `tree))
      (pop eli/temp)
      (setq eli/rate (if (= (length eli/temp) 0) 0 (/ (apply `+  eli/temp) (length eli/temp))))
      (org-set-property "Rating" (format "%.2f" eli/rate))))
  (defun eli/rating ()
    (interactive)
    (org-map-entries 'eli/entry-rating "Series+LEVEL=2"))
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
(use-package ox-beamer)
(use-package org-tempo)
(use-package ox-latex
  :config
  (add-to-list 'org-latex-classes
	       '("beamer"
		 "\\documentclass[ignorenonframetext,presentation]{beamer}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}"))
	       ))

(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
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

;; disable company-mode in org-mode
(defun eli/org-mode-hook ()
  (company-mode -1)
  (flycheck-mode -1))
(add-hook 'org-mode-hook 'eli/org-mode-hook)
;;org capture
(setq org-agenda-dir "~/Dropbox/org")

(setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
(setq org-agenda-file-projects (expand-file-name "projects.org" org-agenda-dir))
(setq org-agenda-file-habit (expand-file-name "habits.org" org-agenda-dir))
(setq org-agenda-file-notes (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippets (expand-file-name "snippets.org" org-agenda-dir))
(setq org-agenda-file-te (expand-file-name "TE.org" org-agenda-dir))
(setq org-agenda-file-lists (expand-file-name "lists.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-dir))

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
         "* %? \n\n%a \n%i \n%U"
         :empty-lines 0)
        ("j" "Journals" entry (file+olp+datetree org-agenda-file-journal)
         "* %<%H:%M> %? "
         :empty-lines 0
	 :clock-in t
	 :clock-resume t)
        ("d" "Digests" entry (file+olp+datetree org-agenda-file-notes)
         "* %a \n%i \n%U"
         :empty-lines 0)
        ("l" "Chrome" entry (file+headline org-agenda-file-inbox "Notes")
         "* %? \n%i \n\n%:annotation \n\n%U"
         :empty-lines 0)
	("T" "TE" entry (file org-agenda-file-te)
	 "* TODO %u\nSCHEDULED: <%(org-read-date nil nil \"+1d\") .+1d> \n %?")
	("b" "Book" entry (file+headline org-agenda-file-lists "Books")
	 "* TODO %?\n  %^{Title}p %^{Isbn}p %^{Types}p %^{Authors}p %^{Translator}p %^{Publisher}p %^{Nation}p %^{Lang}p %^{Rating}p")
	("m" "Movies and Musicals" entry (file+headline org-agenda-file-lists "Movies and Musicals")
	 "* TODO %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p %^{Types}p %^{Time}p %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p")
	("s" "Movies and Musicals" entry (file+headline org-agenda-file-lists "Series")
	 "* TODO %?\n %^{Title}p %^{IMDB}p %^{URL}p %^{Director}p %^{Writer}p %^{Actors}p %^{Types}p %^{Time}p %^{Episodes}p %^{Release}p %^{Nation}p %^{Lang}p %^{Rating}p")
	("a" "Animes" entry (file+headline org-agenda-file-lists "Animes")
	 "* TODO %?\n %^{Title}p %^{URL}p %^{Episodes}p %^{Release}p %^{Director}p %^{Authors}p %^{Publisher}p %^{Rating}p")
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
(run-at-time nil 3600 'org-agenda-to-appt t)
;; 更新agenda时，同步appt
(defun eli/org-agenda-to-appt ()
  "call org-agenda-to-appt with refresh."
  (org-agenda-to-appt t))
(add-hook 'org-agenda-mode-hook  'eli/org-agenda-to-appt)
(add-hook 'org-finalize-agenda-hook 'eli/org-agenda-to-appt)
;; 激活提醒
(appt-activate 1)
;; 提前半小时提醒
(setq appt-message-warning-time 30)
(setq appt-display-interval 5)
(use-package notifications
  :defer 2)

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
  :defer t
  :config
  ;; (pdf-tools-install)
  )

(use-package org-noter
  :after org
  :config
  ;; Your org-noter config ........
  (require 'org-noter-pdftools))

(use-package org-pdftools
  :after org
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; save last pdf position
(use-package saveplace-pdf-view
  :after pdf-tools
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
  (org-mode . toggle-input-method)
  :custom
  ((default-input-method "rime")

   (rime-user-data-dir "~/.emacs.d/rime")
   (rime-disable-predicates '(rime-predicate-prog-in-code-p
                              ;; rime-predicate-space-after-ascii-p
                              rime-predicate-after-ascii-char-p
                              rime-predicate-punctuation-line-begin-p
                              rime-predicate-org-in-src-block-p
                              rime-predicate-space-after-cc-p
			      rime-predicate-hydra-p
                              ))
   ;; (rime-inline-predicates '(rime-predicate-space-after-cc-p)))
   )
  :config
  (setq default-input-method "rime"
	rime-show-candidate 'posframe)
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-inline-ascii-trigger 'shift-l)
  (global-set-key (kbd "M-s-k") 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "M-s-j") 'rime-force-enable)
  )

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  )

;;roam
(use-package org-roam
  :quelpa ((org-roam :fetcher github :repo "org-roam/org-roam" :branch "v2") :upgrade nil)
  :init
  (setq org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  (add-to-list 'display-buffer-alist
               '(("\*org-roam\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  (setq org-roam-node-display-template "${file} > ${olp} > ${title:*} ${tags:10}")
  :custom
  ((org-roam-directory "~/Dropbox/org/roam/"))
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "${slug}.org"
							 "#+TITLE: ${title}\n#+DATE: %T\n")
                                      :unnarrowed t)))
  ;; this sets up various file handling hooks so your DB remains up to date
  (org-roam-setup)
  )

;; (use-package org-roam-bibtex
;;   :quelpa ((org-roam-bibtex :fetcher github :repo "org-roam/org-roam-bibtex" branch "org-roam-v2") :upgrade nil)
;;   ;; :load-path "~/.emacs.d/private/org-roam-bibtex"
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :config
;;   (require 'org-ref)
;;   (setq
;;    ;; orb-preformat-keywords
;;    ;; '("citekey" "title" "url" "author-or-editor" "keywords" "file" "year")
;;    ;; orb-process-file-keyword t
;;    orb-file-field-extensions '("pdf")
;;    orb-note-actions-interface 'helm
;;    orb-insert-interface 'helm-bibtex
;;    )
;;   )

;;-----------------------------------------------------------------------------
;; blog
;; create a blog quickly
(defun eli/create-blogs ()
  (interactive)
  (counsel-find-file "~/Dropbox/org/blog"))

;; publishing
(defun eli/push-to-gitpage (&optional UNUSE)
  (interactive)
  (shell-command "~/.emacs.d/private/shell.sh")
  (message "blogs deployed successfully!")
  )

(setq org-html-validation-link nil) ; 去掉validation显示
(setq org-html-link-home "index.html"); 设置home超链接
(setq org-html-link-up "index.html")
(setq eli-blog-base-dir "~/Dropbox/org/blog")
(setq eli-blog-publish-dir "~/Elilif.github.io")
(setq org-html-postamble nil)
(setq org-publish-project-alist
      `(("eli's blog"
         :base-directory ,eli-blog-base-dir
         :publishing-directory ,eli-blog-publish-dir
         :base-extension "org"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Eli's blog"
         :sitemap-sort-files anti-chronologically
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
         :with-creator nil
         :completion-function eli/push-to-gitpage
         )))
;;----------------------------------------------------------------------------

(use-package youdao-dictionary
  :after org
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search-at-point-posframe))
  :config
  (setq url-automatic-caching t))

;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-client-id "1055740533302-i0kv050tfr5sv6k5j39f1r7fa2smlcqg.apps.googleusercontent.com"
;; 	org-gcal-client-secret "4OkssNTRTN8b5Atrc0unbcWg"
;; 	org-gcal-fetch-file-alist '(("eli.q.qian@gmail.com" .  "~/Dropbox/org/test.org")
;;                                     ;; ("another-mail@gmail.com" .  "~/task.org")
;; 				    )))


(use-package wttrin
  :ensure t
  :load-path "~/.emacs.d/private/emacs-wttrin"
  :config
  (setq wttrin-default-cities '("JiaXing?m" "HangZhou?m"))
  (setq wttrin-mode-line-city "Jiaxing")
  (setq wttrin-mode-line-format "%l:+%c %t %w")
  (wttrin-display-weather-in-mode-line))

(use-package helm-org
  :ensure t
  :after org
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
  (add-to-list 'helm-org-headings-actions '("Insert id link(s) C-C v" . yuchen/helm-org-marked-heading-id-link) t)
)

(use-package helm-org-rifle
  :ensure t
  :after org
  :config)
(provide 'init-org)
