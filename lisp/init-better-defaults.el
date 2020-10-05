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
	("\C-c \C-r" . 'counsel-recentf)
	("<f6>" . 'ivy-resume)
	("M-x" . 'counsel-M-x)
	("\C-x \C-f" . 'counsel-find-file)))



(use-package popwin
  :ensure t
  :config
  (popwin-mode t))

(provide 'init-better-defaults) 
