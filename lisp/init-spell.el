(use-package flyspell
  :defer t
  :defer 5
  :hook ((org-mode . flyspell-mode)
	 (text-mode . flyspell-mode))
  )

(use-package ispell
  :defer t
  :defer 5
  :config
  (setq ispell-personal-dictionary "~/.emacs.d/mydictionary")
  )

(use-package flyspell-correct
  :after flyspell
  :ensure t
  :bind ((:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
	 (:map flyspell-mode-map ("C-." . nil))))

(use-package flyspell-correct-ivy
  :ensure t
  :after ivy
  :defer 5
  :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package goldendict
  :ensure t
  :defer 5
  :bind ("C-c h" . goldendict-dwim))

(use-package youdao-dictionary
  :ensure t
  :after org
  :defer 5
  ;; :bind (("C-c y" . youdao-dictionary-search-at-point-posframe))
  :config
  (setq url-automatic-caching t))

(use-package wordnut
  :ensure t
  :defer 5
  :bind (("C-c y" . wordnut-lookup-current-word)))

(defun Eli/dict-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/txtdict/")
	(INIT-INPUT))
    (counsel-rg INIT-INPUT BASEDIR)))

(global-set-key (kbd "C-c d") 'Eli/dict-search)
(defun Eli/te-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/TEdict")
	(INIT-INPUT "\\("))
    (counsel-rg INIT-INPUT BASEDIR)))

(global-set-key (kbd "C-c s") 'Eli/te-search)
(add-hook 'minibuffer-setup-hook 'yas-minor-mode)


(provide 'init-spell)
