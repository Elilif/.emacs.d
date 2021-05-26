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




(provide 'init-spell)
