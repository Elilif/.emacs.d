(package-initialize)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'init-package)
(require 'init-completion)
(require 'init-better-defaults)
(require 'init-ui)
(require 'init-org)
(require 'init-elfeed)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)







