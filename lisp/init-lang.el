(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (require 'warnings)
  (add-hook 'minibuffer-setup-hook 'yas-minor-mode)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package auto-yasnippet
  :ensure t
  :defer t)

(use-package yasnippet-snippets
  :ensure t
  :defer t)

;; (use-package irony
;;   :ensure t
;;   :hook ((c++-mode . irony-mode)
;;          (c-mode . irony-mode))
;;   :config
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   (use-package company-irony-c-headers
;;     :ensure t)
;;   (use-package company-irony
;;     :ensure t
;;     :config
;;     (add-to-list (make-local-variable 'company-backends)
;;                  '(company-irony company-irony-c-headers)))
;;   (use-package flycheck-irony
;;     :ensure t
;;     :config
;;     (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
;;     )
;;   (use-package irony-eldoc
;;     :ensure t
;;     :config
;;     (add-hook 'irony-mode-hook #'irony-eldoc)
;;     )
;;   )

(use-package ccls
  :ensure t
  :defer t
  :init
  (setq ccls-executable "/usr/bin/ccls"))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :defer t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

;; optionally
(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol
;;   :ensure t)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list
  :ensure t
  :defer t)

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :defer t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


(use-package quickrun
  :ensure t
  :defer t)

(use-package vterm
  :ensure t
  :defer t)

(use-package zeal-at-point
  :ensure t
  :defer t)

(provide 'init-lang)


