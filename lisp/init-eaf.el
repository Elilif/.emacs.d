(use-package eaf
  :load-path "~/.emacs.d/private/emacs-application-framework"
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (setq eaf-browser-continue-where-left-off t)
  (eaf-setq eaf-browser-default-zoom  "1.25")
  (setq browse-url-browser-function 'eaf-open-browser)
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-setq eaf-browser-enable-adblocker t)
  (setq eaf-chrome-bookmark-file "~/.config/chromium/Default/Bookmarks")
  (setq eaf-proxy-type "http")
  (setq eaf-proxy-host "127.0.0.1")
  (setq eaf-proxy-port "7890")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (setq eaf-pdf-extension-list '("xps" "oxps" "cbz" "epub" "fb2" "fbz")))


(provide 'init-eaf)
