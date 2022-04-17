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

  (setq centaur-lsp 'lsp-mode)
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
	 (defun ,intern-pre (info)
           (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                      "org-src-babel.tmp"))
           (pcase centaur-lsp
             ('eglot
              (when (fboundp 'eglot-ensure)
		(eglot-ensure)))
             ('lsp-mode
              (when (fboundp 'lsp-deferred)
		;; Avoid headerline conflicts
		(setq-local lsp-headerline-breadcrumb-enable nil)
		(lsp-deferred)))
             (_
              (user-error "LSP:: invalid `centaur-lsp' type"))))
	 (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      centaur-lsp (upcase ,lang)))

	 (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++"))
  ;; (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang)))
  
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
