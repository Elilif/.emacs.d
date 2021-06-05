(use-package flyspell
  :hook (org-mode . flyspell-mode))

(use-package flyspell-correct
  :after flyspell
  :ensure t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)

(use-package goldendict
  :ensure t
  :bind ("C-c p" . goldendict-dwim))

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
