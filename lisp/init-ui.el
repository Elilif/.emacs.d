(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-light t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(provide 'init-ui)
