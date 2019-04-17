;;; my-web-mode.el --- 

;; Copyright (C) 2017  Igarashi Masanao

;; Author: Igarashi Masanao <syoux2@gmail.com>

(require 'web-mode)
(require 'emmet-mode)

;(set-face-attribute 'web-mode-symbol-face nil :foreground "#FF7400")

;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$"       . web-mode))

;;; インデント数
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;;;; yasnippet
;(add-to-list 'ac-modes 'web-mode)
;(add-hook 'web-mode-hook
;          '(lambda ()
;             (defun web-mode-buffer-refresh ()
;               (interactive)
;               (web-mode-scan-buffer)
;               )
;             ))


(dolist (hook (list
               'sgml-mode-hook
               'css-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq emmet-preview-default nil) ;don't show preview when expand code
                   (emmet-mode)
                   )))

;(lazy-set-mode-autoload-key
; '(
;   ("M-(" . web-mode-element-wrap)
;   ("M-)" . web-mode-element-unwrap)
;   ("M-k" . web-mode-element-kill)
;   ("C-M-SPC" . web-mode-mark-and-expand)
;   ("%" . web-mode-match-paren)
;   ("C-:" . web-mode-comment-or-uncomment)
;   )
; web-mode-map nil "web-mode-extension")
;(lazy-set-mode-autoload-key
; '(
;   ("C-c C-r" . mc/mark-sgml-tag-pair))
; web-mode-map nil "multiple-cursors")
