;;; early-init.el -*- lexical-bindings:t; no-byte-compile:t -*-

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq
 package-enable-at-startup nil
 package-quickstart nil)

(with-eval-after-load 'package
  (setopt package-enable-at-startup nil))

;; 自動生成ファイルを無効にする
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; Don't resize the frame with font change at startup to avoid delay:
(setq frame-inhibit-implied-resize t)

(setq default-directory "~/")
(setq command-line-default-directory "~/")

(setq confirm-kill-emacs 'y-or-n-p)

;; Ctrl-h でカーソル前の文字を消す
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; suspend-frame を無効にする
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(set-default-coding-systems 'utf-8)
