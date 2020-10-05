(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'package)
(require 'cl-lib)

;; Add Packages
(defvar my/packages '(
		      ;; --- Auto-completion ---
		      company
		      ;; --- Better Editor ---
		      use-package
		      hungry-delete
		      smex
		      swiper
		      counsel
		      smartparens
		      popwin
		      ) "Default packages")

(setq-default package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (cl-loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


(provide 'init-package)
