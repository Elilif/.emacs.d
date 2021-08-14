(use-package flyspell
  :defer t
  :hook ((org-mode . flyspell-mode)
	 (text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode))
  )

(use-package ispell
  :defer t
  :config
  (setq ispell-personal-dictionary "~/.emacs.d/mydictionary"))

(use-package flyspell-correct
  :after flyspell
  :ensure t
  :bind ((:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
	 (:map flyspell-mode-map ("C-." . nil))))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package goldendict
  :ensure t
  :defer t
  :bind ("C-c h" . goldendict-dwim))

(use-package youdao-dictionary
  :ensure t
  :after org
  :defer t
  ;; :bind (("C-c y" . youdao-dictionary-search-at-point-posframe))
  :config
  (setq url-automatic-caching t))

(use-package wordnut
  :ensure t
  :defer t
  :bind (("C-c y" . wordnut-lookup-current-word)))

(defun Eli/dict-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/txtdict/")
	(INIT-INPUT))
    (consult-ripgrep BASEDIR INIT-INPUT)))

(defun Eli/te-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/TEdict")
	(INIT-INPUT "\\("))
    (consult-ripgrep BASEDIR INIT-INPUT)))


(provide 'init-spell)
