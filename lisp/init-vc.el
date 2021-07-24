(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :bind
  (:map magit-status-mode-map
	("q" . magit-kill-this-buffer))
  :config
  (defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)
  )


(provide 'init-vc)
