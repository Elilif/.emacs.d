;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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

(use-package lsp-mode
  :ensure t
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp lsp-deffered)

(use-package lsp-ui
  :ensure t)

(use-package lsp-treemacs
  :ensure t)

;; (use-package ccls
;;   :ensure t
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp)))  
;;   :config
;;   (setq lsp-prefer-flymake nil)   (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++ -cppcheck c/c++-gcc))
;;   (setq ccls-executable "/usr/bin/ccls"))


;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :defer t
  :config
  (use-package dap-cpptools))
;; (use-package dap-LANGUAGE) to load the dap adapter for your language



(provide 'init-lsp)
