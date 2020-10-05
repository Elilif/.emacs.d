(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(provide 'init-completion)
