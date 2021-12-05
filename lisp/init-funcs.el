;; init-funcs.el --- Initialize funcs configurations.	-*- lexical-binding: t -*-

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

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and centaur-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(defun childframe-workable-p ()
  "Test whether childframe is workable."
  (and emacs/>=26p
       (eq centaur-completion-style 'childframe)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(provide 'init-funcs)
