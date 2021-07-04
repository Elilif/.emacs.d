(setq gc-cons-threshold most-positive-fixnum)
;; 清空避免加载远程文件的时候分析文件。
(setq file-name-handler-alist nil)
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       'garbage-collect))
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
(require 'init-bib)
(require 'init-spell)
(require 'init-anki)
(require 'init-hydra)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)







