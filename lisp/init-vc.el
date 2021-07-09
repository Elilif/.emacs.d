(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))


(provide 'init-vc)
