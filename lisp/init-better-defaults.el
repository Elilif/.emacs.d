(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(global-auto-revert-mode 1)
(defun eli/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'eli/open-init-file)
(setq make-backup-files nil)
(setq auto-save-default nil)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "\C-c \C-r") 'recentf-open-files)
(delete-selection-mode 1)
(setq initial-frame-alist '((fullscreen . maximized)))
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(setq auto-save-list-file-prefix nil)

(setq url-proxy-services '(
                           ("http" . "127.0.0.1:8889")
                           ;; ("https" . "127.0.0.1:8889")
                           ;; ("socks5" . "127.0.0.1:1089")
                           ))
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

;; remove "^"
(setq ivy-initial-inputs-alist nil)

(use-package auto-save
  :load-path "~/.emacs.d/private/auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace nil))

(use-package dired-x
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (put 'dired-find-alternate-file 'disabled nil)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
  (setq dired-dwim-target t))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))
(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  :bind(("\C-s" . 'swiper)
	("\C-x \C-r" . 'counsel-recentf)
	("<f6>" . 'ivy-resume)
	("M-x" . 'counsel-M-x)
	("\C-x \C-f" . 'counsel-find-file)))

(use-package all-the-icons
  :ensure t)



(use-package popwin
  :ensure t
  :config
  (popwin-mode t))

(provide 'init-better-defaults) 
