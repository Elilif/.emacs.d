;; init-minibuffer.el --- Initialize minibuffer configurations.	-*- lexical-binding: t -*-

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


(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  )

(use-package vertico-directory
  :load-path "~/.emacs.d/private/vertico/extensions"
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :config
  ;; https://github.com/oantolin/orderless/issues/48
  ;; (define-advice company-capf
  ;;     (:around (orig-fun &rest args) set-completion-styles)
  ;;   (let ((completion-styles '(basic partial-completion)))
  ;;     (apply orig-fun args)))

  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)

  ;; https://github.com/minad/consult/wiki
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides
        '((buffer (styles basic partial-completion))
          (file (styles basic partial-completion))
          (command (styles +orderless-with-initialism))
          (variable (styles +orderless-with-initialism))
          (symbol (styles +orderless-with-initialism)))
        ;; allow escaping space with backslash!
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(+orderless-dispatch))
  )

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun eli/org-roam-backlink-node-read--completions (backlink-nodes &optional filter-fn sort-fn)
    (let* ((template (org-roam-node--process-display-format org-roam-node-display-template))
           (nodes (eli/get-backlink-list backlink-nodes))
           (nodes (mapcar (lambda (node)
                            (org-roam-node-read--to-candidate node template)) nodes))
           (nodes (if filter-fn
                      (cl-remove-if-not
                       (lambda (n) (funcall filter-fn (cdr n)))
                       nodes)
                    nodes))
           (sort-fn (or sort-fn
			(when org-roam-node-default-sort
                          (intern (concat "org-roam-node-read-sort-by-"
                                          (symbol-name org-roam-node-default-sort))))))
           (nodes (if sort-fn (seq-sort sort-fn nodes)
                    nodes)))
      nodes))

  (defun eli/org-roam-backlink-node-read (backlink-nodes &optional initial-input filter-fn sort-fn require-match prompt)
    (let* ((nodes (eli/org-roam-backlink-node-read--completions backlink-nodes filter-fn sort-fn)
		  )
           (prompt (or prompt "Node: "))
           (node (completing-read
                  prompt
                  (lambda (string pred action)
                    (if (eq action 'metadata)
			`(metadata
                          ;; Preserve sorting in the completion UI if a sort-fn is used
                          ,@(when sort-fn
                              '((display-sort-function . identity)
				(cycle-sort-function . identity)))
                          (annotation-function
                           . ,(lambda (title)
				(funcall org-roam-node-annotation-function
					 (get-text-property 0 'node title))))
                          (category . org-roam-node))
                      (complete-with-action action nodes string pred)))
                  nil require-match initial-input 'org-roam-node-history)))
      (or (cdr (assoc node nodes))
          (org-roam-node-create :title node))))

  (defun eli/get-backlink-list (backlink-nodes)
    (let ((counter 0)
	  (node-list nil))
      (while backlink-nodes
	(add-to-list 'node-list (org-roam-backlink-source-node (pop backlink-nodes)))
	(setq counter (1+ counter)))
      node-list
      ))

  (defun eli/follow-backlinks (entry)
    (let* ((node-at-point (get-text-property 0 'node entry))
	   (backlink-nodes (org-roam-backlinks-get node-at-point)))
      (org-roam-node-visit (eli/org-roam-backlink-node-read backlink-nodes))))

  (defvar my-org-roam-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "i") 'org-roam-node-insert)
      (define-key map (kbd "s") 'embark-collect)
      (define-key map (kbd "b") 'eli/follow-backlinks)
      map)
    "Keymap for 'org-roam' v2.")

  ;; make available my-org-roam-map to embark-act
  (add-to-list 'embark-keymap-alist '(org-roam-node . my-org-roam-map))

  ;; define the keymap
  (defvar my-citar-embark-become-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f") 'citar-open)
      (define-key map (kbd "x") 'biblio-arxiv-lookup)
      (define-key map (kbd "c") 'biblio-crossref-lookup)
      (define-key map (kbd "i") 'biblio-ieee-lookup)
      (define-key map (kbd "h") 'biblio-hal-lookup)
      (define-key map (kbd "s") 'biblio-dissemin-lookup)
      (define-key map (kbd "b") 'biblio-dblp-lookup)
      (define-key map (kbd "o") 'biblio-doi-insert-bibtex)
      map)
    "Citar Embark become keymap for biblio lookup.")

  ;; tell embark about the keymap
  (add-to-list 'embark-become-keymaps 'my-citar-embark-become-map)

  (defun ex/search-pdf-contents (keys-entries &optional str)
    "Search pdfs."
    (interactive (list (citar-select-refs)))
    (let ((files (citar-file--files-for-multiple-entries
                  (citar--ensure-entries keys-entries)
                  citar-library-paths
                  '("pdf")))
          (search-str (or str (read-string "Search string: "))))
      (pdf-occur-search files search-str t)))

  ;; with this, you can exploit embark's multitarget actions, so that you can run `embark-act-all`
  (add-to-list 'embark-multitarget-actions #'ex/search-pdf-contents)
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package savehist
  :init
  (savehist-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :init
  (setq marginalia-align 'right)
  :hook (after-init . marginalia-mode)
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  )

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (
	 ("C-s" . consult-line)
	 ("C-x C-r" . consult-recent-file)
	 ;; C-c bindings (mode-specific-map)
         ;; ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c b" . consult-bookmark)
         ;; ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-\"" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-'" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)	;; orig. yank-pop
         ("<help> a" . consult-apropos)	;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)	 ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)) ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (defun my/consult-org-file (&optional match)
    (interactive)
    (consult-org-heading match '(list org-agenda-file-inbox org-agenda-file-habit org-agenda-file-projects)))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep my/consult-org-file
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(provide 'init-minibuffer)
