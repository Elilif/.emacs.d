(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :bind
  (:map magit-status-mode-map
	("q" . magit-kill-this-buffer))
  )


(provide 'init-vc)
