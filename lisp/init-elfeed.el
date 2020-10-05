(use-package elfeed
  :ensure t
  :init
  (setq elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:8889"))
  )

(use-package elfeed-org
  :ensure t
  :init
  (elfeed-org)
  (setq  rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org")))

(use-package elfeed-goodies
  :ensure t
  :init
  (elfeed-goodies/setup))

(provide 'init-elfeed)
