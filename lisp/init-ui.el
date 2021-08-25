;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

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


(use-package doom-themes
  :ensure t
  :defer 0.01
  :config
  (load-theme 'doom-one-light t)
  (setq doom-one-light-brighter-comments t)
  (setq doom-one-light-brighter-modeline t)
  (setq doom-one-light-padded-modeline nil)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "✎")
					 ("#+END_SRC" . "□")
					 ("#+begin_src" . "✎")
					 ("#+end_src" . "□")
					 ("[ ]" . "☐")
					 ("[X]" . "☑")
					 ("#+begin_quote" . "»")
					 ("#+end_quote" . "«")
					 ("#+begin_verse" . "ζ")
					 ("#+end_verse" . "ζ")
					 ("#+begin_example" . "")
					 ("#+end_example" . "")
					 ))
  (setq org-ellipsis "▼")
  (custom-set-faces
   '(bookmark-face ((t (:background nil))))
   '(org-dispatcher-highlight ((t (:foreground "red"))))
   '(org-list-dt ((t (:weight bold :foreground "#5d1d9d"))))
   '(org-block ((t (:extend t :background "#ededed"))))
   '(org-quote ((t (:extend t :foreground "#888888" :background "#fafafa"))))
   '(org-verse ((t (:extend t :foreground "#888888" :background "#fafafa"))))
   '(org-block-begin-line ((t (:background nil :foreground nil :inherit nil))))
   '(org-block-end-line ((t (:background nil :foreground nil :inherit nil))))
   '(org-headline-done ((t (:foreground "#B0BEC5"))))
   '(org-level-1 ((t (:inherit nil :weight bold :height 1.25))))
   '(org-level-2 ((t (:inherit nil :weight bold :height 1.15))))
   '(org-level-3 ((t (:weight bold :inherit nil :height 1.05))))
   '(org-level-4 ((t (:inherit nil))))
   '(org-level-5 ((t (:inherit nil))))
   '(org-ellipsis ((t (:foreground "#B0BEC5" ))))
   '(org-document-info-keyword ((t (:foreground "#B0BEC5"  :inherit nil))))
   '(org-drawer ((t (:foreground "#B0BEC5" :inherit nil))))
   '(org-meta-line ((t (:foreground "#B0BEC5" :inherit nil))))
   '(org-link ((t (:underline t :foreground "#000000" :inherit nil))))
   '(rime-default-face ((t (:background "gainsboro" :foreground "#333333"))))
   )

  ;; set org-checkbox-done-text face
  (defface org-checkbox-done-text
    '((t (:foreground "#b5b5b5")))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords 'org-mode
                          '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                             1 'org-checkbox-done-text prepend))
                          'append)
  (set-cursor-color "#000000")
  )

;; (use-package nano-theme
;;   :ensure nil
;;   :quelpa ((nano-theme
;;             :fetcher github
;;             :repo "rougier/nano-theme") :upgrade nil)
;;   :config
;;   (load-theme 'nano t)
;;   )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil)
  (column-number-mode 1)
  )

(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode)
  )

;; Setting English Font
(set-face-attribute 'default nil :font "Source Code Pro 13")
;; (set-face-attribute 'default nil :font "Operator Mono" :height 140)
;; (set-face-attribute 'default nil :font "Inconsolata 18")

;; chinese fonts
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 20)))

(set-fontset-font "fontset-default" 'unicode "AR PL New Kai" nil 'prepend)
(set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'prepend)

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-user-emojis '(("☑" . (("name" . "Checkbox")
				      ("image" . nil)
				      ("style" . "github")))
			      ("▶" . (("name" . "BLACK RIGHT-POINTING TRIANGLE")
				      ("image" . nil)
				      ("style" . "github")))
			      ))
  )

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package dashboard
  :if (< (length command-line-args) 2)
  :preface
  (defun my/dashboard-banner ()
    "Sets a dashboard banner including information on package initialization
     time and garbage collections."
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time
                   (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :custom-face
  (dashboard-banner-logo-title ((t (:foreground "#969595"))))
  :custom (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook)
    (setq dashboard-set-init-info t)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/Documents/images/ue-light.png")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          ;; (projects . 5)
                          ;; (agenda . 5)
                          ;; (registers . 5)
			  ))
  )



(provide 'init-ui)
