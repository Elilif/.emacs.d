;; init-elfeed.el --- Initialize elfeed configurations.	-*- lexical-binding: t -*-

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


(use-package elfeed
  :ensure t
  :defer t
  :init
  (setq elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:7890"))
  (defun eli/elfeed-search-quit-and-kill-buffers ()
    "Save the database, then kill elfeed buffers, asking the user
for confirmation when needed."
    (interactive)
    (elfeed-db-save)
    (let (buf)
      (dolist (file rmh-elfeed-org-files)
	(setq buf (get-file-buffer file))
	(when (and (buffer-modified-p buf)
		   file
		   (y-or-n-p (format "Save file %s? " file)))
          (with-current-buffer buf (save-buffer)))
	(kill-buffer buf)))
    (kill-buffer "*elfeed-log*")
    (kill-buffer (current-buffer)))
  :bind
  ((:map elfeed-search-mode-map
	 ("q" . eli/elfeed-search-quit-and-kill-buffers))
   (:map elfeed-show-mode-map
	 ("\C-k" . keyboard-quit)))
  :config
  (setq elfeed-search-filter "@2-days-ago +unread +A")
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))

  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  )

(use-package elfeed-org
  :ensure t
  :after elfeed
  :init
  (elfeed-org)
  (setq  rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org")))

;; required by elfeed-goodies
(use-package powerline
  :ensure t
  :after elfeed-goodies)

(use-package elfeed-goodies
  :load-path "~/.emacs.d/private/elfeed-goodies"
  ;; :ensure t
  :after elfeed
  :custom
  (elfeed-goodies/date-format "%Y-%m-%d")
  :config
  (elfeed-goodies/setup)
  (advice-add 'elfeed-goodies/show-mode-setup :after
	      (lambda ()
		(define-key elfeed-show-mode-map (kbd "M-v") nil))
	      ))

;; (setq smtpmail-auth-credentials "~/.authinfo")
(use-package smtpmail
  :after mu4e
  :config
  (setq smtpmail-smtp-user "eli.q.qian@gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 465
	smtpmail-stream-type 'ssl)
  (setq send-mail-function 'smtpmail-send-it)
  (setq sendmail-program "/usr/bin/msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)
  ;; (setq url-gateway-method 'socks)
  ;; (setq socks-server '("Default server" "127.0.0.1" 7891 5))
  )


(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :defer t
  :commands (mu4e)
  :if (executable-find "mu")
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq user-full-name "Eli")
  (setq user-mail-address "eli.q.qian@gmail.com")
  (setq
   mu4e-get-mail-command "proxychains mbsync -a"
   ;; mu4e-html2text-command "w3m -T text/html"
   mu4e-update-interval 600)
  ;; configure the bookmarks.
  (setq mu4e-bookmarks
	'( ("flag:unread AND NOT flag:trashed"                    "Unread messages"                  ?u)
           ("date:today..now"                                     "Today's messages"                 ?t)
           ("date:7d..now"                                        "Last 7 days"                      ?w)
           ("date:1d..now AND NOT list:emacs-orgmode.gnu.org"     "Last 1 days"                      ?o)
           ("date:1d..now AND list:emacs-orgmode.gnu.org"         "Last 1 days (org mode)"           ?m)
           ("maildir:/sent"                                       "sent"                             ?s)
           ("maildir:/drafts"                                     "drafts"                           ?d)
           ("mime:image/*"                                        "Messages with images"             ?p)
	   ("maildir:/trash"                                      "Trash"                            ?g)
	   )))

(use-package mu4e-alert
  :ensure t
  :defer t
  :hook (mu4e-main-mode . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'notifications)
  )

(use-package mu4e-maildirs-extension
  :ensure t
  :after mu4e
  :hook (mu4e-main-mode . mu4e-maildirs-extension))

(use-package w3m
  :ensure t
  :after mu4e)

(provide 'init-elfeed)
