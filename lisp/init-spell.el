;; init-spell.el --- Initialize spell configurations.	-*- lexical-binding: t -*-

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


(use-package flyspell
  :defer t
  :hook ((org-mode . flyspell-mode)
	 (text-mode . flyspell-mode)
	 (emacs-lisp-mode . flyspell-prog-mode))
  :config
  (setq flyspell-mark-duplications-flag nil)
  )

(use-package ispell
  :defer t
  :config
  (setq ispell-personal-dictionary "~/.emacs.d/mydictionary")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

(use-package flyspell-correct
  :after flyspell
  :ensure t
  :bind ((:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
	 (:map flyspell-mode-map ("C-." . nil))))

(use-package goldendict
  :ensure t
  :defer t
  :bind ("C-c h" . goldendict-dwim))

(use-package youdao-dictionary
  :ensure t
  :after org
  :defer t
  :bind (("C-c t" . youdao-dictionary-search-at-point-posframe))
  :init
  (setq url-automatic-caching t)
  )

(use-package wordnut
  :ensure t
  :defer t
  :bind (("C-c y" . wordnut-lookup-current-word)))

(defun Eli/dict-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/txtdict/")
	(INIT-INPUT))
    (consult-ripgrep BASEDIR INIT-INPUT)))

(defun Eli/te-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/TEdict")
	(INIT-INPUT "\\("))
    (consult-ripgrep BASEDIR INIT-INPUT)))


(provide 'init-spell)
