(use-package flyspell
  :hook (org-mode . flyspell-mode))

(use-package flyspell-correct
  :after flyspell
  :ensure t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)




(provide 'init-spell)
