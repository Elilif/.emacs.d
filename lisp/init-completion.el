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
  :config
  (setq company-box-scrollbar nil))

(use-package company-emojify
  :ensure t
  :config
  )

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

(defvar mcfly-commands
  '(consult-line))

(defvar mcfly-back-commands
  '(self-insert-command))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region
	  (progn (forward-visible-line 0) (point))
          (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (insert (propertize (save-excursion
			  (set-buffer (window-buffer (minibuffer-selected-window)))
			  (or (seq-some (lambda (thing) (thing-at-point thing t))
					;; '(region url symbol sexp))
					'(region url symbol))
			      "No thing at point")
			  )    'face 'shadow))
    (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)
    (forward-visible-line 0)
    ))

;; setup code
(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)

(provide 'init-completion)
