;; init-lang.el --- Initialize lang configurations.	-*- lexical-binding: t -*-

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

(use-package yasnippet
  :ensure t
  :defer 5
  :hook (after-init . yas-global-mode)
  :config
  ;; (yas-global-mode)
  (require 'warnings)
  (add-hook 'minibuffer-setup-hook 'yas-minor-mode)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (add-to-list 'warning-suppress-log-types '(yasnippet backquote-change))
  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (bound-and-true-p yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))

  ;; Try after every insertion
  (add-hook 'post-command-hook #'my/yas-try-expanding-auto-snippets)
  )

(use-package auto-yasnippet
  :ensure t
  :defer t)

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :defer t)

(use-package quickrun
  :ensure t
  :defer t)

(use-package vterm
  :ensure t
  :defer t)

(use-package zeal-at-point
  :ensure t
  :defer t)

(provide 'init-lang)


