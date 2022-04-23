;; init.el --- Initialize init configurations.	-*- lexical-binding: t -*-

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

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))


(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
	("nongnu" . "http://elpa.nongnu.org/nongnu/")
	))

(package-initialize)


;; Speed up startup
(setq auto-mode-case-fold nil)

;; Bootstrap `use-package'
(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'init-hydra)
(require 'init-completion)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-reader)
(require 'init-elfeed)
(require 'init-vc)
(require 'init-lsp)
(require 'init-flycheck)
(require 'init-lang)
(require 'init-c)
(require 'init-r)
(require 'init-bib)
(require 'init-spell)
(require 'init-anki)
(require 'init-minibuffer)
(require 'init-ui)
(require 'init-calendar)
(require 'init-music)
(require 'init-blog)
(require 'init-tex)
(require 'init-corfu)
(require 'init-finance)
;; (require 'init-eaf)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)
