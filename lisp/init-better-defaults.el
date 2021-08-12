;; set fill column
(setq-default fill-column 80)
;; show file size
(size-indication-mode 1)
;; open customize group buffers in one buffer
(defadvice custom-buffer-create (before my-advice-custom-buffer-create)
  "Exit the current Customize buffer before creating a new one, unless there are modified widgets."
  (if (eq major-mode 'Custom-mode)
      (let ((custom-buffer-done-kill t)
            (custom-buffer-modified nil))
        (mapc (lambda (widget)
                (and (not custom-buffer-modified)
                     (eq (widget-get widget :custom-state) 'modified)
                     (setq custom-buffer-modified t)))
              custom-options)
        (if (not custom-buffer-modified)
            (Custom-buffer-done)))))
(ad-activate 'custom-buffer-create)
;; disable ad-like warning
(setq ad-redefinition-action 'accept)
;; disable ring bell when cussor at bottom 
(setq ring-bell-function 'ignore)

;; remove some bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


(setq inhibit-splash-screen t)

;; auto sync files
(global-auto-revert-mode 1)

;; disable backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; better fill region in capture
(defun eli/fill-region ()
  (interactive)
  (let* ((min (point-min))
         (max (- (point-max) 25)))
    (fill-region min max)))
(global-set-key (kbd "M-s-q") 'eli/fill-region)


(delete-selection-mode 1)
(setq initial-frame-alist '((fullscreen . maximized)))
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(setq auto-save-list-file-prefix nil)

;; http://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
;; improve copy
(defun my-copy-simple (&optional beg end)
  "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))

;; split window right
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; fix M-j
(defun eli-org-fill-prefix ()
  "Set `fill-prefix' to the empty string."
  (setq fill-prefix ""))
(add-hook 'org-mode-hook #'eli-org-fill-prefix)


;; use proxy
(setq url-proxy-services '(
                           ("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")
                           ;; ("socks5" . "127.0.0.1:7891")
                           ))

;; improve hippie-expand
(setq hippie-expand-try-function-list '(try-expand-debbrev
					try-expand-debbrev-all-buffers
					try-expand-debbrev-from-kill
					try-complete-file-name-partially
					try-complete-file-name
					try-expand-all-abbrevs
					try-expand-list
					try-expand-line
					try-complete-lisp-symbol-partially
					try-complete-lisp-symbol))
(global-set-key (kbd "s-/") 'hippie-expand)

;; simplify yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

(setq load-prefer-newer t)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)


(defun eli/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f5>") 'eli/open-init-file)

;; use winnder-mode
(use-package winner-mode
  :ensure nil
  :defer 5
  :init
  (defun transient-winner-undo ()
    "Transient version of winner-undo."
    (interactive)
    (let ((echo-keystrokes nil))
      (winner-undo)
      (message "Winner: [u]ndo [r]edo")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
	 (define-key map [?u] #'winner-undo)
	 (define-key map [?r] #'winner-redo)
	 map)
       t)))
  :hook (after-init . winner-mode)
  :bind
  ("C-c u" . transient-winner-undo))
(use-package avy
  :ensure t
  :defer 5
  :init
  (defun avy-goto-char-near-point (char)
    "Jump to the currently visible CHAR in the few lines near point."
    (interactive (list (read-char "char: " t)))
    (let ((avy-all-windows nil))
      (avy-with avy-goto-char
	(avy--process
	 (avy--regex-candidates
          (regexp-quote (string char))
          (line-beginning-position -1)
          (line-end-position 3))
	 (avy--style-fn avy-style)))))
  :bind
  ("C-:" . 'avy-goto-char-in-line)
  ("C-'" . 'avy-goto-char)
  ("C-\"" . 'avy-goto-char-near-point)
  )

(use-package ace-pinyin
  :ensure t
  :defer 5
  :config
  (ace-pinyin-global-mode 1))

(use-package iedit
  :ensure t
  :defer 5)

(use-package wgrep
  :ensure t
  :defer 5)

(use-package grab-x-link
  :ensure t
  :defer 5
  :config
  ;; (global-set-key (kbd "C-c i") 'grab-x-link-chromium-insert-link)
  ;; (global-set-key (kbd "C-c o") 'grab-x-link-chromium-insert-org-link)
  )

(use-package expand-region
  :ensure t
  :defer 2
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package which-key
  :ensure t
  :defer 5
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"))

(use-package auto-save
  :load-path "~/.emacs.d/private/auto-save"
  :defer 5
  :config
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace nil)
  (setq auto-save-idle 2)
  (auto-save-enable)
  )

(use-package dired-x
  :defer 5
  :bind
  (:map dired-mode-map
	("q" . 'kill-this-buffer))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (put 'dired-find-alternate-file 'disabled nil)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
  (setq dired-dwim-target t)
  ;; 切换buffer后，立即刷新
  (defadvice switch-to-buffer (after revert-buffer-now activate)
    (if (eq major-mode 'dired-mode)
	(revert-buffer)))

  ;; 执行shell-command后，立即刷新
  (defadvice shell-command (after revert-buffer-now activate)
    (if (eq major-mode 'dired-mode)
	(revert-buffer)))

  ;; 在Bookmark中进入dired buffer时自动刷新
  (setq dired-auto-revert-buffer t))

(use-package hungry-delete
  :ensure t
  :defer 5
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never)
  (setq  recentf-exclude
	 '("/home/eli/.emacs.d/.cache/treemacs-persist-at-last-error"
	   "/home/eli/.emacs.d/.cache/treemacs-persist"
	   "\\.txt"
	   "/home/eli/.emacs.d/elpa/*"
	   "/home/eli/.elfeed/index"
	   ))
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 50)
  )

(use-package all-the-icons
  :ensure t
  :defer 5)

(use-package golden-ratio
  :ensure t
  :defer 5
  :config
  (golden-ratio-mode 0))

(use-package popwin
  :ensure t
  :defer 5
  :config
  (popwin-mode t)
  (setq popwin:popup-window-position 'right)
  (setq popwin:popup-window-width 80))

(use-package quelpa
  :ensure t
  :config
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t)

(use-package esup
  :ensure t
  :defer 5
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package multiple-cursors
  :ensure t
  :defer 2
  :config
  (setq mc/always-run-for-all nil)
  (setq mc/insert-numbers-default 1))

(use-package treemacs
  :ensure t
  :defer 10)

(use-package ibuffer
  :defer 5
  :bind
  ("\C-x \C-b" . ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("emacs" (or
			   (mode . emacs-lisp-mode)
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
		 ("magit" (or
                           (mode . magit-status-mode)
                           (mode . magit-process-mode)
                           (mode . magit-diff-mode)
                           (mode . magit-revision-mode)
                           (mode . magit-log-mode)))
		 ("org" (mode . org-mode))
		 ))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  )

(use-package wttrin
  :ensure t
  :defer 5
  :load-path "~/.emacs.d/private/emacs-wttrin"
  :config
  (setq wttrin-default-cities '("WuZhen?m?T" "HangZhou?m?T"))
  (setq wttrin-mode-line-city "WuZhen")
  (setq wttrin-mode-line-format "%l:+%c %t %w")
  (wttrin-display-weather-in-mode-line))

(use-package hl-todo
  :ensure t
  :defer 5
  :hook (after-init . global-hl-todo-mode))

(use-package elisp-refs
  :ensure t
  :defer 5)

(use-package helpful
  :ensure t
  :defer 5
  :bind (
	 ("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 :map helpful-mode-map
	 ("q" . kill-buffer-and-window)
	 )
  :config
  (setq helpful-max-buffers 1)
  )

(use-package undo-tree
  :ensure t
  :defer 5
  :init
  (setq global-undo-tree-mode t))

(use-package restart-emacs
  :ensure t
  :defer t)

(provide 'init-better-defaults)
