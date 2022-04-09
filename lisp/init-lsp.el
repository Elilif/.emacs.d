(use-package lsp-mode
  :ensure t
  :defer t
  :diminish
  :custom
  (lsp-completion-provider :none)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                          (lsp-deferred))))
         (markdown-mode . lsp-deferred)
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)))
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure flex
  (setq read-process-output-max (* 1024 1024)) ;; 1MB

  (setq
   lsp-keep-workspace-alive nil
   lsp-signature-auto-activate nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-workspace-status-enable nil
   lsp-headerline-breadcrumb-enable t

   lsp-enable-file-watchers nil
   lsp-enable-folding nil
   lsp-enable-symbol-highlighting t
   lsp-enable-text-document-color t

   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-lens-enable nil)
  :config
  (add-to-list 'lsp-language-id-configuration '(snippet-mode . "plaintext"))
  
  )

(use-package lsp-ui
  :ensure t
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover t
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-code-actions t
	lsp-ui-doc-show-with-cursor t)

  )

;; (use-package dap-mode
;;   :ensure t
;;   :defer t)

(provide 'init-lsp)
