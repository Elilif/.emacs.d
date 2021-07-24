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
  :bind
  ("C-:" . 'avy-goto-char-in-line)
  ("C-'" . 'avy-goto-char)
  :config
  )
(use-package ace-pinyin
  :ensure t
  :config
  (ace-pinyin-global-mode 1))

(use-package iedit
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package grab-x-link
  :ensure t
  :config
  ;; (global-set-key (kbd "C-c i") 'grab-x-link-chromium-insert-link)
  ;; (global-set-key (kbd "C-c o") 'grab-x-link-chromium-insert-org-link)
  )

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"))

(use-package auto-save
  :load-path "~/.emacs.d/private/auto-save"
  :config
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace nil)
  (setq auto-save-idle 2)
  (auto-save-enable)
  )

(use-package dired-x
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

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :config
  ;; remove "^"
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :bind
  (("\C-x b" . 'counsel-switch-buffer)
   ("\C-h v" . 'counsel-describe-variable)
   ("\C-h f" . 'counsel-describe-function)))

(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  :bind(("\C-s" . 'swiper)
	("\C-c m" . 'swiper-thing-at-point)
	("\C-x \C-r" . 'counsel-recentf)
	("<f6>" . 'ivy-resume)
	("M-x" . 'counsel-M-x)
	("\C-x \C-f" . 'counsel-find-file)))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display)))
  (defvar ivy-posframe--first-show t)
  (defun ivy-posframe-cleanup ()
    "Cleanup ivy's posframe."
    (setq ivy-posframe--first-show t)
    (when (posframe-workable-p)
      (posframe-hide ivy-posframe-buffer)))
  (defun ivy-posframe--display (str &optional poshandler)
    "Show STR in ivy's posframe with POSHANDLER."
    (if (not (posframe-workable-p))
	(ivy-display-function-fallback str)
      (with-ivy-window
	(if (not ivy-posframe--first-show)
            (with-current-buffer ivy-posframe-buffer
              (erase-buffer)
              (insert str))
          (setq ivy-posframe--first-show nil)
          (apply #'posframe-show
                 ivy-posframe-buffer
                 :font ivy-posframe-font
                 :string str
                 :position (point)
                 :poshandler poshandler
                 :background-color (face-attribute 'ivy-posframe :background nil t)
                 :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
                 :internal-border-width ivy-posframe-border-width
                 :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
                 :override-parameters ivy-posframe-parameters
                 (funcall ivy-posframe-size-function)))
	(ivy-posframe--add-prompt 'ignore)))
    (with-current-buffer ivy-posframe-buffer
      (setq-local truncate-lines ivy-truncate-lines)))
  (defun my-ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
          (width (min (or ivy-posframe-width 200) (round (* .75 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))
  (setq ivy-posframe-size-function 'my-ivy-posframe-get-size)
  (ivy-posframe-mode 1)
  )

;; Support pinyin in Ivy
;; Input prefix ':' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package pinyinlib
  :ensure t
  :commands pinyinlib-build-regexp-string
  :init
  (with-no-warnings
    (defun ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (pinyin-to-utf8 str)
          (and (fboundp 'ivy-prescient-non-fuzzy)
               (ivy-prescient-non-fuzzy str))
          (ivy--regex-plus str)))

    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) "!")
             (mapconcat
              #'my-pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'my-pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string "!" "" str )
                            "")))
              ""))
            (t nil)))

    (mapcar
     (lambda (item)
       (let ((key (car item))
             (value (cdr item)))
         (when (member value '(ivy-prescient-non-fuzzy
                               ivy--regex-plus))
           (setf (alist-get key ivy-re-builders-alist)
                 #'ivy--regex-pinyin))))
     ivy-re-builders-alist)))

(defun my-swiper-hack (&optional arg)
  (ignore arg)
  (if (region-active-p) (deactivate-mark)))
(advice-add 'swiper :before #'my-swiper-hack)

(use-package all-the-icons
  :ensure t)

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 0))

(use-package popwin
  :ensure t
  :config
  (popwin-mode t)
  (setq popwin:popup-window-position 'right)
  (setq popwin:popup-window-width 80))

(use-package quelpa
  :config
  (setq quelpa-update-melpa-p nil))
(use-package quelpa-use-package)

(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/always-run-for-all t))

(use-package treemacs
  :ensure t)

(provide 'init-better-defaults)
