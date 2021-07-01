(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-light t))

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
  (setq dashboard-startup-banner 2)
  )

;; Setting English Font
(set-face-attribute 'default nil :font "Source Code Pro-13")

;; chinese fonts
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 20)))
(set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)


(provide 'init-ui)
