(use-package elfeed
  :ensure t
  :init
  (setq elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:8889"))
  :bind
  ("C-c e" . elfeed)
  :config
  (setq elfeed-search-filter "@2-days-ago +unread +A")
  )

(use-package elfeed-org
  :ensure t
  :init
  (elfeed-org)
  (setq  rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org")))

(use-package elfeed-goodies
  :ensure t
  :init
  (elfeed-goodies/setup)
  :config
  (advice-add 'elfeed-goodies/show-mode-setup :after
	      (lambda ()
		(define-key elfeed-show-mode-map (kbd "M-v") nil))
	      ))


(provide 'init-elfeed)
