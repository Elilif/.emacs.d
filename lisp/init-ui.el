(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one-light t)
  (setq doom-one-light-brighter-comments t)
  (setq doom-one-light-brighter-modeline t)
  (setq doom-one-light-padded-modeline nil)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "✎")
					 ("#+END_SRC" . "□")
					 ("#+begin_src" . "✎")
					 ("#+end_src" . "□")
					 ("[ ]" . "☐")
					 ("[X]" . "☑")
					 ("#+begin_quote" . "»")
					 ("#+end_quote" . "«")
					 ("#+begin_verse" . "ζ")
					 ("#+end_verse" . "ζ")
					 ("#+begin_example" . "")
					 ("#+end_example" . "")
					 ))
  (setq org-ellipsis "▼")
  (custom-set-faces
   '(org-block ((t (:extend t :background "#ededed"))))
   '(org-quote ((t (:extend t :foreground "#888888" :background "#fafafa"))))
   '(org-verse ((t (:extend t :foreground "#888888" :background "#fafafa"))))
   '(org-block-begin-line ((t (:background nil :foreground nil :inherit nil))))
   '(org-block-end-line ((t (:background nil :foreground nil :inherit nil))))
   '(org-headline-done ((t (:foreground "#B0BEC5"))))
   '(org-level-1 ((t (:inherit nil :weight bold :height 1.2))))
   '(org-level-2 ((t (:inherit nil :weight bold :height 1.1))))
   '(org-level-3 ((t (:weight bold :inherit nil))))
   '(org-level-4 ((t (:inherit nil))))
   '(org-level-5 ((t (:inherit nil))))
   '(org-ellipsis ((t (:foreground "#B0BEC5" ))))
   '(org-document-info-keyword ((t (:foreground "#B0BEC5"  :inherit nil))))
   '(org-drawer ((t (:foreground "#B0BEC5" :inherit nil))))
   '(org-meta-line ((t (:foreground "#B0BEC5" :inherit nil))))
   '(org-link ((t (:underline t :inherit nil))))
   '(rime-default-face ((t (:background "gainsboro" :foreground "#333333"))))
   )
  (set-cursor-color "#000000")
  )

;; (use-package nano-theme
;;   :ensure nil
;;   :quelpa ((nano-theme
;;             :fetcher github
;;             :repo "rougier/nano-theme") :upgrade nil)
;;   :config
;;   (load-theme 'nano t)
;;   )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/Documents/images/ue-light.png")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          ;; (projects . 5)
                          ;; (agenda . 5)
                          ;; (registers . 5)
			  ))
  )

;; Setting English Font
(set-face-attribute 'default nil :font "Source Code Pro-13")

;; chinese fonts
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 20)))
(set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)
(set-fontset-font "fontset-default" 'unicode "AR PL New Kai" nil 'prepend)

(use-package emojify
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ui)
