;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Calendar configuration.
;;

;;; Code:


(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-backends '(company-bbdb company-semantic company-cmake (company-capf :with company-yasnippet) company-clang company-files
	      (company-dabbrev-code company-gtags company-etags company-keywords)
	      company-oddmuse company-dabbrev))
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq completion-ignore-case t)
  (setq company-idle-delay 0.2)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  )

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-scrollbar nil))

(use-package smartparens
  :ensure t
  :hook (after-init . smartparens-global-mode)
  :config
  (sp-pair "（" "）")
  (sp-pair "“" "”")
  ;; improving emphasis marker
  (sp-local-pair 'org-mode "~" "~ ")
  (sp-local-pair 'org-mode "/" "/ ")
  (sp-local-pair 'org-mode "=" "= ")
  (sp-local-pair 'org-mode "+" "+ ")
  (sp-local-pair 'org-mode "*" "* ")
  (sp-local-pair 'org-mode "_" "_ ")
  (require 'smartparens-config)
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
	  (t (save-excursion
	       (ignore-errors (backward-up-list))
	       (funcall fn))))))

(defvar mcfly-commands
  '(consult-line
    consult-outline))

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
