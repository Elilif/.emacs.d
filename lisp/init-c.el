;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;; C/C++ configuration.
;;

;;; Code:

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :defer t
  :hook ((c-mode-common . (lambda () (c-set-style "stroustrup")))
	 (c-mode-common . eli/cc-mode-hook))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
    :ensure t
    :diminish
    :init (modern-c++-font-lock-global-mode t))
  (defun eli/cc-mode-hook ()
    (let* ((file-name (buffer-file-name))
	   (is-windows (equal 'windows-nt system-type))
	   (exec-suffix (if is-windows ".exe" ".out"))
	   (os-sep (if is-windows "\\" "/")))
      (if file-name
	  (progn
	    (setq file-name (file-name-nondirectory file-name))
	    (let ((out-file (concat (file-name-sans-extension file-name) exec-suffix)))
	      (setq-local compile-command (format "g++ -std=c++14 %s -o %s && .%s%s" file-name out-file os-sep out-file)))
	    )
	))
    )
  )


(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
