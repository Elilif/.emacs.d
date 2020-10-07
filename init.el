(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Bootstrap `use-package'
(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(add-to-list 'load-path (concat user-emacs-directory "lisp"))


(require 'init-completion)
(require 'init-better-defaults)
(require 'init-ui)
(require 'init-org)
(require 'init-elfeed)
(require 'init-vc)
(require 'init-lang)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)







