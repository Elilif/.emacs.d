(setq gc-cons-threshold most-positive-fixnum)
;; 清空避免加载远程文件的时候分析文件。
(setq file-name-handler-alist nil)
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       'garbage-collect))
