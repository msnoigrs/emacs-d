;(auto-insert-mode)
;(setq auto-insert-directory "~/templates")
;(setq auto-insert-query nil)
;(define-auto-insert "\.org" "my-org-mode.org")

;http://ochiailab.blogspot.jp/2012/12/yasnippet.html

(fset 'yes-or-no-p 'y-or-n-p)

;http://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/

;;; yasnippet
(setq yas-snippet-dirs
	  (list (expand-file-name (concat user-emacs-directory "snippets"))
            "/usr/share/emacs/site-lisp/yasnippet-snippets/snippets"))
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete
;http://dev.ariel-networks.com/Members/matsuyama/auto-complete-0-1-0
(setq ac-use-menu-map t)     ; キーバインド
;(setq ac-auto-start 4)       ; 4 文字以上で起動
(setq ac-delay 0.2)
(setq ac-auto-show-menu 0.3) ; 0.3秒でメニュー表示
(setq ac-use-comphist t)     ; 補完候補をソート
(setq ac-candidate-limit nil); 補完候補表示を無制限に
;(setq ac-use-quick-help nil) ; tool tip 無し

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               (expand-file-name (concat user-emacs-directory "ac-dict")))
  (ac-config-default)

  (ac-set-trigger-key "TAB")
  (add-to-list 'ac-modes 'coffee-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'web-mode)
  (add-to-list 'ac-modes 'text-mode)

  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  ;;(define-key ac-completing-map (kbd "<tab>") 'nil)
  ;; (define-key ac-completing-map (kbd "<tab>") 'ac-complete)
  ;;(define-key ac-completing-map (kbd "M-/")   'ac-stop)
  ;;(define-key ac-completing-map (kbd "RET") nil) ; return での補完禁止
)
;(define-key ac-menu-map "\C-n" 'ac-next)
;(define-key ac-menu-map "\C-p" 'ac-previous)

;; wget https://raw.github.com/sandai/dotfiles/master/.emacs.d/ac-dict/js2-mode
;;(add-hook 'web-mode-hook
;;          '(lambda ()
;;             (add-to-list 'ac-dictionary-files "~/.emacs.d/ac-dict/js2-mode")
             ;;(add-to-list 'ac-dictionary-files "~/.emacs.d/ac-dict/php-mode")
;;             ))


;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
;; (setqだとtermなどで干渉問題ありでした)
;; もちろんTAB以外でもOK 例えば "C-;"とか
;(custom-set-variables '(yas-trigger-key "TAB"))

;; 既存スニペットを挿入する
;(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
;(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
;(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;; helm interface
(eval-after-load "helm-config"
  (quote
   (progn
     (defun my-yas-prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (helm-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*helm yas-prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas-prompt-functions '(my-yas-prompt)))
     (define-key helm-command-map (kbd "y") 'yas-insert-snippet))))

;; snippet-mode for *.yasnippet files
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

; http://stackoverflow.com/questions/12058717/confusing-about-the-emacs-custom-system
(auto-insert-mode)
(setq auto-insert-query nil)
(setq auto-insert-directory "~/templates/")
(setq auto-insert 'other)

(defun my:autoinsert-yas-expand ()
      "Replace text in yasnippet template."
       (setq yas-indent-line 'fixed)
       (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;http://d.hatena.ne.jp/saitodevel01/20110123/1295756051
(setq auto-insert-alist
	  (append
	   '((("posts/.*\\.org$" . "org-mode") . ["nikola.org" my:autoinsert-yas-expand])
         (("stories/.*\\.org$" . "org-mode") . ["nikola.org" my:autoinsert-yas-expand])
         (("news/.*\\.org$" . "org-mode") . ["nikola-news.org" my:autoinsert-yas-expand])
         (("root/.*\\.org$" . "org-mode") . ["nikola.org" my:autoinsert-yas-expand])
		 ((".*\\.org$" . "org-mode") . ["template.org" my:autoinsert-yas-expand])
		 ((".*\\.py$" . "python-mode") . ["template.py" my:autoinsert-yas-expand])
		 ((".*\\.txt$" . "text-mode") . ["template.txt" my:autoinsert-yas-expand])
		) auto-insert-alist))
;http://www.emacswiki.org/emacs/AutoInsertMode
;https://github.com/jixiuf/emacs_conf/blob/master/site-lisp/joseph/joseph-yasnippet-auto-insert.el

;http://blog.keshi.org/hogememo/2013/11/04/solaar-using-logitech-unifying-in-linux
