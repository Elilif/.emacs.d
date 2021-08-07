(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq git-commit-summary-max-length 50)
  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :bind
  (:map magit-status-mode-map
	("q" . mu-magit-kill-buffers))
  )

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode)
  (setq magit-todos-auto-group-items 3)
  )

(provide 'init-vc)
