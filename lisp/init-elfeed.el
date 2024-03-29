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
	 ("q" . kill-buffer-and-window)))
  :config
  (setq elfeed-show-entry-switch #'pop-to-buffer)
  (setq elfeed-search-filter "@2-days-ago +unread +A")

  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (eval-after-load 'elfeed-search
    '(push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
    )

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))

  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

  ;; Filter elfeed search buffer by the feed under cursor.
  (defun eli/elfeed-search-filter-source (entry)
    "Filter elfeed search buffer by the feed under cursor."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (when (elfeed-entry-p entry)
      (elfeed-search-set-filter
       (concat
	"@6-months-ago "
	;; "+unread "
	"="
	(replace-regexp-in-string
	 (rx "?" (* not-newline) eos)
	 ""
	 (elfeed-feed-url (elfeed-entry-feed entry)))))))

  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "f") 'eli/elfeed-search-filter-source))

  (defun eli/elfeed-search-starred-entries ()
    (interactive)
    (elfeed-search-set-filter "+starred"))

  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "M") 'eli/elfeed-search-starred-entries))
  )

(use-package elfeed-score
  :ensure t
  :after elfeed
  :config
  (setq elfeed-score-serde-score-file "~/.emacs.d/private/elfeed.score")
  (setq elfeed-score-rule-stats-file "~/.emacs.d/private/elfeed.stats")
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  ;; (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
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

(use-package ace-jump-mode
  :ensure t)

(use-package noflet
  :ensure t)
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
  :bind (:map mu4e-headers-mode-map
	      ("f" . eli/mu4e-search-filter-source)
	      ("!" . mu4e-headers-mark-for-refile)
	      ("r" . mu4e-headers-mark-for-read))
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq user-full-name "Eli Qian")
  (setq user-mail-address "eli.q.qian@gmail.com")
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq shr-use-colors nil)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-prefer-html t)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-get-mail-command "proxychains mbsync -a"
	mu4e-update-interval 600)
  ;; configure the bookmarks.
  (setq mu4e-bookmarks
	'( ("flag:unread AND NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"  "Unread messages"                  ?u)
	   ("NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                  "All messages"                     ?a)
           ("date:today..now AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                   "Today's messages"                 ?t)
           ("date:7d..now AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                      "Last 7 days"                      ?w)
           ("date:1d..now AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org"                      "Last 1 days"                      ?y)
           ("list:emacs-orgmode.gnu.org"                                                                            "Org mode"                         ?o)
           ("list:emacs-devel.gnu.org"                                                                              "Emacs Devel"                      ?e)
           ("maildir:/sent"                                                                                         "sent"                             ?s)
           ("maildir:/drafts"                                                                                       "drafts"                           ?d)
           ("mime:image/*"                                                                                          "Messages with images"             ?p)
	   ("maildir:/trash"                                                                                        "Trash"                            ?g)
	   ))

  ;; filter
  (defun eli/mu4e-search-filter-source ()
    (interactive)
    (let* ((msg (mu4e-message-at-point))
	   (sender-address (mu4e~org-first-address msg :from))
	   (sender-email (progn
			   (string-match "<\\(.*\\)>" sender-address)
			   (match-string 1 sender-address))))
      (mu4e~headers-search-execute (concat "from:" sender-email) nil)))

  ;; citation format
  (setq mu4e-view-show-addresses t)
  (setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")
  (setq message-citation-line-function #'message-insert-formatted-citation-line)

  ;; from https://ag91.github.io/blog/2022/02/28/set-all-incoming-emails-as-read-with-mu4e/
  (defun my/emails-set-all-as-read ()
    "Make all emails read."
    (interactive)
    (require 'mu4e-contrib)
    (with-temp-buffer
      (mu4e-headers-search-bookmark "flag:unread AND list:emacs-orgmode.gnu.org")
      (sleep-for 0.15)
      (mu4e-headers-mark-all-unread-read)
      (mu4e-mark-execute-all 'no-confirmation)))

  ;; overlay the original `mu4e~view-make-urls-clickable' to  hide URLs
  (defun mu4e~view-make-urls-clickable ()
    "Turn things that look like URLs into clickable things.
Also number them so they can be opened using `mu4e-view-go-to-url'."
    (let ((num 0))
      (save-excursion
	(setq mu4e~view-link-map ;; buffer local
              (make-hash-table :size 32 :weakness nil))
	(goto-char (point-min))
	(while (re-search-forward mu4e~view-beginning-of-url-regexp nil t)
          (let ((bounds (thing-at-point-bounds-of-url-at-point)))
            (when bounds
              (let* ((url (thing-at-point-url-at-point))
                     (ov (make-overlay (car bounds) (cdr bounds))))
		(puthash (cl-incf num) url mu4e~view-link-map)
		(overlay-put ov 'after-string
                             (propertize (format "\u200B[%d]" num)
					 'face 'mu4e-url-number-face))
		(overlay-put ov 'display "")
		)))))))
  )

(use-package mu4e-alert
  :ensure t
  :defer t
  :hook (mu4e-main-mode . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'notifications)
  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT list:emacs-orgmode.gnu.org AND NOT list:emacs-devel.gnu.org")
  )

(use-package mu4e-maildirs-extension
  :ensure t
  :after mu4e
  :hook (mu4e-main-mode . mu4e-maildirs-extension))

(use-package w3m
  :ensure t
  :after mu4e)

(provide 'init-elfeed)
