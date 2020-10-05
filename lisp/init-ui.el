(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-light t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

(provide 'init-ui)
