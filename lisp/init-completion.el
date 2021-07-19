(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-show-numbers t)
  (setq completion-ignore-case t)
  (setq company-idle-delay 0.2)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config)

(use-package smartparens
  :ensure t
  :config
  (sp-pair "（" "）")
  (sp-pair "“" "”")
  (require 'smartparens-config)
  (smartparens-global-mode)
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
	  (t (save-excursion
	       (ignore-errors (backward-up-list))
	       (funcall fn))))))

(provide 'init-completion)
