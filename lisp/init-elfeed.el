(use-package elfeed
  :ensure t
  :defer t
  :init
  (setq elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:8889"))
  :bind
  :config
  (setq elfeed-search-filter "@2-days-ago +unread +A")
  )

(use-package elfeed-org
  :ensure t
  :after elfeed
  :init
  (elfeed-org)
  (setq  rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org")))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :init
  (elfeed-goodies/setup)
  :config
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
  ;; (setq url-gateway-method 'socks)
  ;; (setq socks-server '("Default server" "127.0.0.1" 7891 5))
  )


(use-package mu4e
  :defer 2
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :if (executable-find "mu")
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq user-full-name "Eli")
  (setq user-mail-address "eli.q.qian@gmail.com")
  (setq
   mu4e-get-mail-command "proxychains mbsync -a"
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
           ("mime:image/*"                                        "Messages with images"             ?p))))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  ;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  ;; (setq mu4e-alert-interesting-mail-query
  ;; 	(concat
  ;; 	 "flag:unread"
  ;; 	 " AND NOT flag:trashed"
  ;; 	 " AND NOT maildir:"
  ;; 	 "\"/[Gmail].All Mail\""))
  )

(use-package mu4e-maildirs-extension
  :ensure t
  :config
  (mu4e-maildirs-extension))

(provide 'init-elfeed)
