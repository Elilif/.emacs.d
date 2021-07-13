(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-minimum-prefix-length 1)
 '(custom-buffer-done-kill t)
 '(custom-safe-themes
   '("7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "ea18d4f243da987b679ff18aa8b8dc65734fb38b6fdce4ae2e7389029214a9d1" "ca56bb3ee27b0c6a7acafcee65aa2dd8a5e0f9dc86d606ed107c9d750bbafb18" "fa8120d4f4c2be78d5b68af25961fb854f1b23bb9955f6e01723cd44145e37e3" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "88f59acbeacefb4998f45126d4d8ae8b2184f2a48753db362a349fd55321c7e1" "a1917d891d4a4368d1957db07644265ed1dc4d9f570b0a7cbb7da76fff610985" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(dynamic-agenda-files nil t)
 '(elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:7890") t)
 '(large-file-warning-threshold 1000000000)
 '(org-agenda-files
   '("/home/eli/Dropbox/org/inbox.org" "~/Dropbox/org/daily.org" "~/Dropbox/org/FM.org" "~/Dropbox/org/TE.org" "/home/eli/Dropbox/org/words.org" "/home/eli/Dropbox/org/Clock_Report.org" "/home/eli/Dropbox/org/habits.org" "/home/eli/Dropbox/org/journal.org" "/home/eli/Dropbox/org/notes.org" "/home/eli/Dropbox/org/projects.org"))
 '(org-agenda-window-setup 'current-window)
 '(org-babel-load-languages '((emacs-lisp . t) (shell . t) (python . t) (C . t)))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-roam-completion-everywhere t)
 '(org-superstar-headline-bullets-list '(9776 9673 9675 10040))
 '(package-selected-packages
   '(mu4e-maildirs-extension org-ql multiple-cursors org rainbow-delimiters zeal-at-point ace-pinyin emojify helm-org-rifle helm-org-rfile helm-org benchmark-init esup youdao-dictionary xref tablist simple-httpd shrink-path request-deferred project pos-tip persist pdf-tools parsebib org-superstar org-roam org-ref org-pdftools org-noter-pdftools org-noter noflet names mu4e-alert memoize log4e key-chord jsonrpc htmlize helm-core helm-bibtex helm gntp flyspell-correct-ivy flyspell-correct flymake emacsql-sqlite3 emacsql-sqlite emacsql elfeed-org elfeed-goodies elfeed eldoc doom-themes doom-modeline deferred dash-functional chinese-word-at-point calibredb bibtex-completion biblio-core biblio all-the-icons alert ace-jump-mode vterm wttrin org-media-note quelpa-use-package mpv org-download anki-editor golden-ratio lsp-mode goldendict pinyinlib quickrun org-mind-map dashboard beacon dap-mode lsp-treemacs lsp-ivy lsp-ui ccls yasnippet-snippets auto-yasnippet yasnippet flycheck wgrep grab-x-link iedit expand-region which-key pad-tools magit rime saveplace-pdf-view company use-package hungry-delete smex swiper counsel smartparens popwin))
 '(quelpa-checkout-melpa-p nil)
 '(quelpa-update-melpa-p nil)
 '(recentf-auto-cleanup 'never)
 '(recentf-exclude
   '("/home/eli/.emacs.d/.cache/treemacs-persist-at-last-error" "/home/eli/.emacs.d/.cache/treemacs-persist" "\\.txt" "/home/eli/.emacs.d/elpa/*"))
 '(recentf-max-menu-items 50)
 '(recentf-max-saved-items 50)
 '(rime-deactivate-when-exit-minibuffer nil)
 '(send-mail-function 'smtpmail-send-it)
 '(warning-suppress-types '((yasnippet backquote-change) (:warning)))
 '(which-key-frame-max-height 20)
 '(which-key-idle-delay 0.1)
 '(wttrin-mode-line-format "%l:+%c %t %w"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mu4e-highlight-face ((t nil)))
 '(mu4e-region-code ((t nil)))
 '(org-block ((t (:extend t :background "#ededed"))))
 '(org-block-begin-line ((t (:background nil :foreground nil :inherit nil))))
 '(org-block-end-line ((t (:background nil :foreground nil :inherit nil))))
 '(org-document-info-keyword ((t (:foreground "#B0BEC5" :inherit nil))))
 '(org-drawer ((t (:foreground "#B0BEC5" :inherit nil))))
 '(org-ellipsis ((t (:foreground "#B0BEC5"))))
 '(org-headline-done ((t (:foreground "#B0BEC5"))))
 '(org-level-1 ((t (:inherit nil :weight bold :height 1.2))))
 '(org-level-2 ((t (:inherit nil :weight bold :height 1.1))))
 '(org-level-3 ((t (:weight bold :inherit nil))))
 '(org-level-4 ((t (:inherit nil))))
 '(org-level-5 ((t (:inherit nil))))
 '(org-link ((t (:underline t :inherit nil))))
 '(org-meta-line ((t (:foreground "#B0BEC5" :inherit nil))))
 '(org-quote ((t (:extend t :foreground "#888888" :background "#fafafa"))))
 '(org-verse ((t (:extend t :foreground "#888888" :background "#fafafa"))))
 '(rime-default-face ((t (:background "gainsboro" :foreground "#333333"))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
 '(vterm-color-black ((t (:background "dark gray" :foreground "dark gray")))))
