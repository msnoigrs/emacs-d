;(setq howm-menu-top nil)
(setq howm-menu-lang 'ja)

; howmのプレフィックスキーは、デフォルト設定C-c, だけど、
; org-priorityとかぶってる
(setq howm-prefix (kbd "C-x C-z"))

;;http://www.gfd-dennou.org/member/uwabami/cc-env/emacs/howm_config.html
;大文字小文字を区別しない
(setq howm-keyword-case-fold-search t)

;(setq howm-view-grep-command "egrep")
;(setq howm-view-fgrep-command "fgrep")
;(setq howm-view-grep-extended-option nil)
;(setq howm-view-grep-fixed-option nil)
;(setq howm-view-grep-file-stdin-option nil)

(setq howm-search-other-dir nil)
(setq howm-excluded-file-regexp
      (concat "/\\.#" "\\|"
              "[~#]$" "\\|"
              "\\.bak$" "\\|"
              "/CVS/" "\\|"
              "\\.doc" "\\|"
              "\\.pdf" "\\|"
              "\\.txt$" "\\|"
              "\\.html$" "\\|"
              "\\.tex$" "\\|"
              "\\.dvi$" "\\|"
              "\\.fdb_latexmk$" "\\|"
              "\\.ppt$" "\\|"
              "\\.xls$" "\\|"
              "\\.howm-menu$" "\\|"
              "\\.howm-keys$" "\\|"
              "\\.png$" "\\|"
              "\\.gif$" "\\|"
              "\\.tif$" "\\|"
              "\\.tiff$" "\\|"
              "\\.jpg$" "\\|"
              "\\.jpeg$" "\\|"
              "\\.el$" "\\|"
              "\\.aux$" "\\|"
              "\\.log$" "\\|"
              "Makefile" "\\|"
              "\\.txt$" "\\|"
              "EUC-UCS2" "\\|"
              "\\.fdb_latexmk$" "\\|"
              "latexmkrc"))

(setq howm-rootdir "~/Dropbox/howm/")
(setq howm-directory howm-rootdir)
(setq howm-history-file (concat howm-rootdir "history"))
(setq howm-keyword-file (concat howm-rootdir "keys"))
(setq howm-menu-file (concat howm-rootdir "menu.txt"))

; 新しくメモを作る時は、先頭の「=タイトル」だけ挿入。
;(setq howm-template "= %title%cursor\n\n")
(setq howm-view-title-header "*")

;(add-hook 'howm-mode-hook
;          '(lambda () 
; Dropboxでバックアップがあるので、バックアップファイルは作らなくてもいいだろう。
;             (set (make-local-variable 'backup-inhibited) t)))

;; *.org を開いたら howm-mode も起動する
(add-hook 'org-mode-hook 'howm-mode)
; Dropbox/howmの下でorg-modeで編集する場合はhowm-modeを追加。
;(add-hook 'org-mode-hook
;          '(lambda ()
;             (if (string-match "Dropbox/howm" buffer-file-name)
;                 (howm-mode))))

;; howm ファイル名を設定する。org-mode を起動するため拡張子は .org にする。
;(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org")
 ;;howm 1 日 1 ファイル (メモ置き場/年/月/年_月_日.txt に)
;(setq howm-file-name-format "%Y/%m/%Y-%m-%d.txt")
(setq howm-file-name-format "%Y-%m-%d.org")

;;http://pukapukasuru84.blogspot.jp/2012/04/howmorg-mode.html
;;日付のフォーマット変更
;
;(setq howm-dtime-format (concat "<" "%Y-%m-%d %H:%M" ">"))
;(setq howm-insert-date-format "<%s>")
;;;日付を検索するフォーマットの変更
;; howm-date-regexp-grep
;(setq howm-reminder-regexp-grep-format
;      (concat "<" "[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]" "[ :0-9]*>%s"))
;; howm-date-regexp
;(setq howm-reminder-regexp-format
;      (concat "\\(<" "\\([1-2][0-9][0-9][0-9]\\)" "-"
;              "\\([0-1][0-9]\\)" "-"
;              "\\([0-3][0-9]\\)" "[ :0-9]*>\\)\\(\\(%s\\)\\([0-9]*\\)\\)"))
;;;本日の日付を挿入する変数の変更
;(setq howm-reminder-today-format
;      (format howm-insert-date-format howm-date-format))

(defadvice howm-action-lock-done-done
  (after my-org-todo-done () activate)
  (org-todo))

;; ORG
; Dropbox/Howmの下の.howmか.txtのファイルのみorg-modeにする。
;(add-to-list 'auto-mode-alist '("Dropbox/howm/.+\\.howm$" . org-mode))
;(add-to-list 'auto-mode-alist '("Dropbox/howm/.+\\.txt$" . org-mode))


;(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
(mapc
 (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-grep howm-create
             howm-keyword-to-kill-ring howm-mode))

(global-set-key "\C-c,," 'howm-menu)

;C-c , c で howm-create を実行すると自動的に org-mode になっているので、 org-mode のメモをアウトライン表記で記入するのみ。
