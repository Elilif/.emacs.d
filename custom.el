(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-minimum-prefix-length 1)
 '(custom-buffer-done-kill t)
 '(custom-safe-themes
   '("cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "ea18d4f243da987b679ff18aa8b8dc65734fb38b6fdce4ae2e7389029214a9d1" "ca56bb3ee27b0c6a7acafcee65aa2dd8a5e0f9dc86d606ed107c9d750bbafb18" "fa8120d4f4c2be78d5b68af25961fb854f1b23bb9955f6e01723cd44145e37e3" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "88f59acbeacefb4998f45126d4d8ae8b2184f2a48753db362a349fd55321c7e1" "a1917d891d4a4368d1957db07644265ed1dc4d9f570b0a7cbb7da76fff610985" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(delete-by-moving-to-trash t)
 '(dynamic-agenda-files nil t)
 '(large-file-warning-threshold 1000000000)
 '(org-agenda-clock-consistency-checks
   '(:max-duration "10:00" :min-duration 0 :max-gap "0:00" :gap-ok-around
		   ("4:00")
		   :default-face
		   ((:background "DarkRed")
		    (:foreground "white"))
		   :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil))
 '(org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 2 :fileskip0 t :sort
	   (3 . 84)
	   :formula %))
 '(org-agenda-files
   '("~/Dropbox/org/古文.org" "/home/eli/Dropbox/org/Français.org" "/home/eli/Dropbox/org/daily.org" "/home/eli/Dropbox/org/lists.org" "/home/eli/Dropbox/org/inbox.org" "/home/eli/Dropbox/org/words.org" "/home/eli/Dropbox/org/projects.org"))
 '(org-agenda-window-setup 'current-window)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (C . t)
     (R . t)))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
 '(org-roam-completion-everywhere t)
 '(package-selected-packages
   '(org-appear org-roam-ui org-clock-convenience org-mru-clock org-reverse-datetree ledger-mode org-roam-bibtex org-roam lsp-ui lsp-mode lsp-latex ccls link-hint emacsql-sqlite-builtin kind-icon eglot @ shackle popper ace-pinyin pinyinlib dash-functional org-latex-impatient xenops all-the-icons-dired smartparens consult-yasnippet cape corfu lyrics-fetcher elfeed-score flycheck-popup-tip svg-lib prescient org-media-note-org-ref dired-x pcre2el with-editor magit-todos magit hl-todo git-commit marginalia memory-usage org-ref bibtex-completion oc ox-pandoc modern-cpp-font-lock elfeed elfeed-org ace-jump-mode noflet ess-smart-equals ess flycheck-posframe flycheck citre cdlatex auctex preview ebib citar ox-org org-mu4e treemacs spinner pfuture markdown-mode cfrs bui ace-window easy-hugo ox-hugo dap-cpptools cal-china-x spray emms amread-mode keyfreq shrface nov yaml-mode rime w3m org-inlinetask restart-emacs multiple-cursors wordnut org-pdftools org-noter-pdftools undo-tree pretty-hydra helpful frame-local elisp-refs xr writeroom-mode writegood-mode visual-fill-column vertico zoutline page-break-lines embark-consult embark orderless org-contrib epc org-pomodoro ox-timeline mu4e-maildirs-extension org-ql org rainbow-delimiters zeal-at-point helm-org-rifle helm-org-rfile helm-org esup youdao-dictionary xref tablist simple-httpd shrink-path request-deferred project pos-tip persist parsebib org-superstar org-noter noflet names mu4e-alert memoize log4e key-chord jsonrpc htmlize helm-core helm gntp flyspell-correct flymake emacsql-sqlite3 emacsql eldoc doom-themes doom-modeline deferred chinese-word-at-point calibredb biblio-core biblio all-the-icons alert ace-jump-mode vterm wttrin quelpa-use-package mpv org-download golden-ratio goldendict quickrun org-mind-map dashboard beacon auto-yasnippet wgrep grab-x-link iedit expand-region which-key pad-tools saveplace-pdf-view use-package hungry-delete smex popwin))
 '(quelpa-checkout-melpa-p nil)
 '(quelpa-update-melpa-p nil)
 '(rime-deactivate-when-exit-minibuffer nil)
 '(send-mail-function 'smtpmail-send-it)
 '(use-package-always-ensure t)
 '(warning-suppress-types '((yasnippet backquote-change) (:warning)))
 '(which-key-frame-max-height 20)
 '(which-key-idle-delay 1)
 '(wttrin-mode-line-format "%l:+%c %t %w"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-face ((t (:background nil))))
 '(dashboard-banner-logo-title ((t (:foreground "#969595"))))
 '(flycheck-posframe-background-face ((t (:inherit tooltip))))
 '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
 '(flycheck-posframe-face ((t (:foreground "ForestGreen"))))
 '(flycheck-posframe-info-face ((t (:foreground "ForestGreen"))))
 '(lsp-ui-sideline-symbol-info ((t (:extend t :background "white smoke" :foreground "dim gray"))))
 '(mu4e-highlight-face ((t nil)))
 '(mu4e-region-code ((t nil)))
 '(org-block ((t (:extend t :background "#ededed"))))
 '(org-block-begin-line ((t (:background nil :foreground nil :inherit nil))))
 '(org-block-end-line ((t (:background nil :foreground nil :inherit nil))))
 '(org-dispatcher-highlight ((t (:foreground "red"))))
 '(org-document-info-keyword ((t (:foreground "#B0BEC5" :inherit nil))))
 '(org-drawer ((t (:foreground "#B0BEC5" :inherit nil))))
 '(org-ellipsis ((t (:foreground "#B0BEC5"))))
 '(org-footnote ((t (:foreground "#deb887"))))
 '(org-headline-done ((t (:foreground "#B0BEC5"))))
 '(org-level-1 ((t (:inherit nil :weight bold :height 1.25))))
 '(org-level-2 ((t (:inherit nil :weight bold :height 1.15))))
 '(org-level-3 ((t (:weight bold :inherit nil :height 1.05))))
 '(org-level-4 ((t (:inherit nil))))
 '(org-level-5 ((t (:inherit nil))))
 '(org-link ((t (:inherit nil :foreground "#000000" :underline (:color "dim gray" :style line)))))
 '(org-list-dt ((t (:weight bold :foreground "#5d1d9d"))))
 '(org-meta-line ((t (:foreground "#B0BEC5" :inherit nil))))
 '(org-quote ((t (:extend t :foreground "#888888" :background "#fafafa"))))
 '(org-ref-cite-&-face ((t (:inherit org-ref-cite-face))))
 '(org-ref-cite-face ((t (:foreground "#986801" :underline nil :weight light))))
 '(org-verse ((t (:extend t :foreground "#888888" :background "#fafafa"))))
 '(popper-echo-area ((t nil)))
 '(popper-echo-dispatch-hint ((t nil)))
 '(rime-default-face ((t (:background "gainsboro" :foreground "#333333"))))
 '(shr-link ((t (:inherit nil :foreground "#000000" :underline (:color "dim gray" :style line)))))
 '(vterm-color-black ((t (:background "dark gray" :foreground "dark gray")))))
