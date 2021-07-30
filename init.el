(package-initialize)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
	("nongnu" . "http://elpa.nongnu.org/nongnu/")
	))

;; Bootstrap `use-package'
(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(add-to-list 'load-path (concat user-emacs-directory "lisp"))


(require 'init-completion)
(require 'init-better-defaults)
;; (require 'init-ivy)
(require 'init-ui)
(require 'init-org)
(require 'init-elfeed)
(require 'init-vc)
(require 'init-lang)
(require 'init-bib)
(require 'init-spell)
(require 'init-anki)
(require 'init-hydra)
;; HOLD: consult and selectrum
(require 'init-minibuffer)
(require 'init-eaf)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)
