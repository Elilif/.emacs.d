(package-initialize)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'init-package)
(require 'init-completion)
(require 'init-better-defaults)
(require 'init-ui)
(require 'init-org)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load-file custom-file)







