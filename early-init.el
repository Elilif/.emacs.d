(setq gc-cons-threshold most-positive-fixnum)
;; 清空避免加载远程文件的时候分析文件。
;; (setq file-name-handler-alist nil)
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       'garbage-collect))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
