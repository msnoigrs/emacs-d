;;; my-markdown.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Igarashi Masanao

;; Author: Igarashi Masanao <syoux2@gmail.com>

;; (defun my-markdown-preview ()
;;   (interactive)
;;   (when (get-process "grip") (kill-process "grip"))
;;   (when (get-process "grip<1>") (kill-process "grip<1>"))
;;   (start-process "grip" "*grip*" "grip"
;;                  (format "--user=%s" github-user)
;;                  (format "--pass=%s" github-pass)
;;                  "--browser" buffer-file-name)
;;   (when (get-process "grip") (set-process-query-on-exit-flag (get-process "grip")) nil)
;;   (when (get-process "grip<1>" ))
;;   )
