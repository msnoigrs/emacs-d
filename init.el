;;; init.el --- emacs init file

;;; Commentary:

;;; Code:

(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)

(if (eq system-type 'windows-nt)
    ;; Windows
    (progn
      (prefer-coding-system 'utf-8-dos)
      (set-file-name-coding-system 'cp932)
      (set-keyboard-coding-system 'cp932)
      (set-terminal-coding-system 'cp932)
      (modify-coding-system-alist
       'file ".+\\.\\(org\\|py\\|java\\|go\\)$" 'utf-8-unix)
      (defun run-bash ()
        (interactive)
        (let ((explicit-shell-file-name "c:/msys64/usr/bin/bash.exe")
	          (shell-file-name "bash")
	          (explicit-bash.exe-args '("--noediting" "--login" "-i")))
          (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
          (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
          (shell "*bash*"))))
  ;; Windows以外
  (prefer-coding-system 'utf-8-unix))

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-loadpath)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path
 "my-settings"
 "elisp"
)

;; (setq user-full-name "Name")
;; (setq user-mail-address "example@example.com")
(load "my-profile" t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (set-scroll-bar-mode 'right))

;; Ctrl-h でカーソル前の文字を消す
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; suspend-frame を無効にする
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(setq initial-frame-alist
      (append '((top . 10)
                (left . 570))
              initial-frame-alist))

(setq default-frame-alist
      (append '((width . 81)
                (tool-bar-lines . 0))
              default-frame-alist))

;; https://2ch.vet/re_toro_unix_1514601894_331_100
;; https://knowledge.sakura.ad.jp/8494/
;; http://extra-vision.blogspot.com/2016/07/emacs.html
;; https://www.shimmy1996.com/en/posts/2018-06-24-fun-with-fonts-in-emacs/
;; https://idiocy.org/emacs-fonts-and-fontsets.html
;; https://gist.github.com/alanthird/7152752d384325a83677f4a90e1e1a05

(create-fontset-from-ascii-font
 "Cica-10.5:weight=normal:slant=normal" nil "user") ;; fontset-user
(set-fontset-font "fontset-user" 'ascii
                  "Cica-10.5:weight=normal:slant=normal")
(set-fontset-font "fontset-user" 'arabic
                  "Cica-10.5:weight=normal:slant=normal")
(set-fontset-font "fontset-user" 'latin
                  "Cica-10.5:weight=normal:slant=normal")
(set-fontset-font "fontset-user" 'kana
                  "Cica-10.5:weight=normal:slant=normal")
(set-fontset-font "fontset-user" 'han
                  "Cica-10.5:weight=normal:slant=normal")
(set-fontset-font "fontset-user" 'cjk-misc
                  "Cica-10.5:weight=normal:slant=normal")
(set-fontset-font "fontset-user" 'hangul
                  "Noto Sans CJK KR Regular-10:weight=regular:slant=normal")
(set-fontset-font "fontset-user" 'han
                  "Noto Sans CJK SC Regular-10:weight=regular:slant=normal"
                  nil 'append)
(set-fontset-font "fontset-user" 'cjk-misc
                  "Noto Sans CJK SC Regular-10:weight=regular:slant=normal"
                  nil 'append)
(set-fontset-font "fontset-user" 'han
                  "Noto Sans CJK TC Regular-10:weight=regular:slant=normal"
                  nil 'append)
(set-fontset-font "fontset-user" 'cjk-misc
                  "Noto Sans CJK TC Regular-10:weight=regular:slant=normal"
                  nil 'append)
(set-fontset-font "fontset-user" 'unicode
                  "Cica-10.5:weight=normal:slant=normal"
                  nil 'append)

(if (eq system-type 'windows-nt)
    ;; Windows
    (setq default-frame-alist
	  (append '((font . "fontset-user")
                (height . 120) ; 4K panel
                (line-spacing . 0.15))
              default-frame-alist))
  ;; Windows以外
  (setq default-frame-alist
	;; 全角と半角の表示幅の比率が正確に2:1になるのは
	;; 10.5/12/13.5/15/18pt(1.5の倍数)
	;(append '((font . "M+ 1mn light-10.5")
    ;(append '((font . "Source Han Mono-9.5")
        (append '((font . "fontset-user")
                  (height . 100)) ; 4K panel
                default-frame-alist)))

;;;; The lines above are basic settings.

;; (setq url-proxy-services
;;       '(("http" . "xxx.xxx.xxx.xxx:8080")
;;         ("https" . "xxx.xxx.xxx.xxx:8080")))
(let ((proxy-settings (expand-file-name
                       (concat user-emacs-directory "my-settings/my-proxy.el"))))
  (when (file-exists-p proxy-settings)
    (load-file proxy-settings)))
;;;(load "my-proxy" t)

(autoload 'async-start "async")
(autoload 'async-start-process "async")

(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; sessionファイルを~/.emacs.d/sessionsに作成
;; (defun emacs-session-filename (SESSION-ID)
;;   (expand-file-name
;;    (concat user-emacs-directory "sessions/session." SESSION-ID)))

;; mozc
(when (require 'mozc nil t)
  ;; (if (require 'mozc-popup nil t)
  ;;     (setq mozc-candidate-style 'popup)
  ;;   (setq mozc-candidate-style 'overlay))
  (if (require 'mozc-posframe nil t)
      (setq mozc-candidate-style 'posframe))
  (require 'mozc-isearch nil t)
  (require 'mozc-mode-line-indicator nil t)
  (setq default-input-method "japanese-mozc")
  (setq mozc-keymap-kana mozc-keymap-kana-101us)
  (defun mozc-tool ()
    (interactive)
    (shell-command-to-string
     "/usr/libexec/mozc/mozc_tool --mode=word_register_dialog"))
  (global-set-key (kbd "C-<f7>") 'mozc-tool)
  ;; .xprofile
  ;; xcape -e 'Shift_L=Muhenkan;Shift_R=Henkan_Mode'
  (global-set-key [zenkaku-hankaku] 'toggle-input-method)
  (global-set-key [henkan]
                  (lambda () (interactive)
                    (mozc-mode 1)
                    (when (null current-input-method) (toggle-input-method))))
  (global-set-key [muhenkan]
                  (lambda () (interactive)
                    (deactivate-input-method)))
  (defadvice mozc_handle-event (around intercept-keys (event))
    "Intercept keys muhenkan and zenkaku-hankaku, before passing keys
to mozc-server (which the function mozc-handle-event does), to
properly disable mozc-mode."
    (if (member event (list 'zenkaku-hankaku 'muhenkan))
        (progn
          (mozc-clean-up-session)
          (mozc-mode nil)
          (deactivate-input-method))
      (progn
        ad-do-it)))
  (ad-activate 'mozc-handle-event))

;; tabグローバル設定
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;(setq-default judge-indent-default-indent-width 4
;	      judge-indent-default-tab-width 4)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; https://github.com/pseudonamed/.emacs.d/blob/master/init.el
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key "\C-a" 'smarter-move-beginning-of-line)

;; Auto complile a lisp buffer if one doesn't already exist
(defun auto-recompile-el-buffer ()
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-recompile-el-buffer)

(defun untabify-buffer ()
  (interactive)
  (untabify 1 (point-max))
  (if (not (eq major-mode 'mew-draft-mode))
      ;; delete-trailing-whitespace does not work in mew-draft-mode.
      (delete-trailing-whitespace)))

(if (not (fboundp 'defun-if-undefined))
    (defmacro defun-if-undefined (name &rest rest)
      `(unless (fboundp (quote ,name))
         (defun ,name ,@rest))))

(defun-if-undefined inside-string-or-comment-p ()
  (let ((state (parse-partial-sexp (point-min) (point))))
    (or (nth 3 state) (nth 4 state))))

(defun-if-undefined re-search-forward-without-string-and-comments (&rest args)
  (let ((value (apply #'re-search-forward args)))
    (if (and value (inside-string-or-comment-p))
        (apply #'re-search-forward-without-string-and-comments args)
      value)))

(defun my-buffer-indent-tabs-code-p (&optional buffer)
  "Check first indent char."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (and (re-search-forward-without-string-and-comments "^[ \t]"
                                                              (point-max) t)
               (string= (match-string 0) "\t")))))))

(defun my-set-indent-tabs-mode ()
  (setq indent-tabs-mode (my-buffer-indent-tabs-code-p)))

;(require 'dropdown-list)

;(require 'dired-launch)
(add-hook 'dired-load-hook (lambda () (load "dired-x")))

(global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode)
;;; 種類ごとの色
(add-hook 'font-lock-mode-hook
          '(lambda ()
             (set-face-foreground 'font-lock-comment-face "#969696")
             (set-face-foreground 'font-lock-string-face "#CE7B00")
             (set-face-foreground 'font-lock-keyword-face "#0000E6")
             (set-face-foreground 'font-lock-builtin-face "#0000E6")
             (set-face-foreground 'font-lock-function-name-face "magenta4")
             (set-face-foreground 'font-lock-variable-name-face "#009900")
             (set-face-foreground 'font-lock-type-face "purple4")
             (set-face-foreground 'font-lock-constant-face "cyan4")
             (set-face-foreground 'font-lock-warning-face "maroon")
             ))

;;(setq browse-url-browser-function 'browse-url-generic)
;; (setq browse-url-generic-program
;;       (if (file-exists-p "/usr/bin/chromium")
;;           "/usr/bin/chromium"))
;;(setq browse-url-generic-program "xdg-open")
;;(setq browse-url-browser-function 'browse-url-generic)
(defalias 'my-urls-externl-brower 'browse-url-xdg-open)

(defun my-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW, if no browse
with external browser."
  (apply
   (if (y-or-n-p "Browse with EWW? ")
       'eww-browse-url
     #'my-urls-external-browser)
   args))
(setq browse-url-browser-function #'my-browse-url)

;; (when (require 'google-translate nil t)
;;   (require 'google-translate-default-ui)
;;   (setq google-translate-default-source-language "en")
;;   (setq google-translate-default-target-language "ja")
;;   (global-set-key "\C-ct" 'google-translate-at-point)
;;   (global-set-key "\C-cT" 'google-translate-query-translate)
;;   )
(when (require 'google-translate nil t)
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist
        '(("en" . "ja") ("ja" , "en")))
  (setq google-translate-backend-method 'curl)
  (global-set-key "\C-ct" 'google-translate-smooth-translate)
  )

(defvar my-google-translate-english-chars "[:ascii:]'""-")
(defvar my-google-translate-base-url "https://translate.google.com/?source=gtx#")
(require 'url-util)
(defun chromium-translate ()
  "Open google translate with chromium."
  (interactive)
  (if (use-region-p)
      (let* ((val (buffer-substring-no-properties (region-beginning) (region-end)))
             (asciip (string-match (format "\\`[%s]+\\'" my-google-translate-english-chars) val))
             (targ (if asciip "en/ja/" "ja/en/")))
        (deactivate-mark)
        (browse-url-xdg-open (concat my-google-translate-base-url targ (url-hexify-string val))))
    (let* ((val (read-string "Google Translate: "))
           (asciip (string-match (format "\\`[%s]+\\'" my-google-translate-english-chars) val))
           (targ (if asciip "en/ja/" "ja/en/")))
      (browse-url-xdg-open (concat my-google-translate-base-url targ (url-hexify-string val))))))
(global-set-key "\C-cT" 'chromium-translate)

(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/orgdocs/%Y-%m%d-%H%M%S.")
  (global-set-key "\C-xj" 'open-junk-file))

(when (require 'editorconfig nil t)
  (require 'editorconfig-core)
  (editorconfig-mode 1))

(require 'org)
(load "my-org-settings" t)

;; (when (require 'helm-config nil t)
;;   (global-set-key (kbd "C-;") 'helm-mini)
;;   (global-set-key (kbd "M-r") 'helm-resume)
;;   (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;   (global-set-key (kbd "C-x b") 'helm-buffers-list)

;;   (helm-mode 1)

;;   ;http://www49.atwiki.jp/ntemacs/m/pages/32.html
;;   ;; 自動補完を無効に
;;   (setq helm-ff-auto-update-initial-value nil)
;;   ;; TAB で補完する
;;   (define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)

;;   (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;;   (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
;;   (add-to-list 'helm-completing-read-handlers-alist '(insert-file . nil))

;;   (when (require 'helm-ls-git nil t)))

(let ((popwinp (require 'popwin nil t))
      (direxp (require 'direx nil t)))
  (when popwinp (popwin-mode 1))
  (when direxp
    (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
    (setq direx:leaf-icon "   "
          direx:open-icon " - "
          direx:closed-icon " + "))
  (when (and popwinp direxp)
    ;; direx:direx-modeのバッファをウィンドウ左辺に幅25でポップアップ
    ;; :dedicatedにtを指定することで、direxウィンドウ内でのバッファの切り替えが
    ;; ポップアップ前のウィンドウに移譲される
    (push '(direx:direx-mode :position left :width 40 :dedicated t)
          popwin:special-display-config)))

(load "my-auto-insert" t)

(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$"      . web-mode))
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  )

(when (require 'emmet-mode nil t)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
  (dolist (hook (list
                 'vue-mode-hook
                 'sgml-mode-hook
                 'css-mode-hook
                 'web-mode-hook))
    (add-hook hook (lambda ()
                   (setq emmet-preview-default nil) ;don't show preview when expand code
                   (emmet-mode)))))

(when (require 'markdown-mode nil t)
  (set-face-attribute 'markdown-code-face nil :inherit 'Default)
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.md" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
  ;(add-to-list 'auto-mode-alist '("README\\.md" . gfm-mode))
  ;(setq markdown-open-command "~/bin/opengrip.sh")
  (setq markdown-command-needs-filename nil))

(when (require 'scss-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))  
  (add-hook 'scss-mode-hook
            '(lambda ()
               (setq scss-compile-at-save nil))))
               
(when (require 'coffee-mode nil t)
  (add-hook 'coffee-mode-hook
            '(lambda ()
               (setq tab-width 2)
               (setq coffee-tab-width 2))))

;; css-mode
(setq cssm-indent-function #'cssm-c-style-indenter)
(add-hook 'css-mode-hook
          '(lambda ()
             (setq css-indent-offset 2)))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)

;; (autoload 'auto-complete-mode "auto-complete" "AutoComplete mode" t)
;; (eval-after-load 'auto-complete
;;   '(progn
;;      (add-to-list 'ac-dictionary-directories
;;                   "/usr/share/emacs/etc/auto-complete/dict")
;; ;     (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
;;      (dolist (hook '(css-mode-hook))
;;        (add-hook hook 'ac-css-mode-setup))))

(with-eval-after-load 'image-file
  ;; Exclude .svg image from supported image list, as Emacs doesn't come
  ;; with SVG shared library.
  (setq image-file-name-extensions (remove "svg" image-file-name-extensions))
  ;; Re-initialize the image-file handler.
  (auto-image-file-mode t))

;(load "view-mode-key" t)

;;; eshell
(setq eshell-output-filter-functions
      (list 'eshell-handle-ansi-color
            'eshell-handle-control-codes
            'eshell-watch-for-password-prompt))
(add-hook 'eshell-mode-hook 'ansi-color-for-comint-mode-on)

;; cc-mode
(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "Google")))
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "linux")))

(defconst my-java-style
         '((c-basic-offset . 4) ; change
           (tab-width . 4)
           (indent-tabs-mode . nil)
           (c-offsets-alist .
                ((statement-block-intro . +)
                 (knr-argdecl-intro     . 5)
                 (substatement-open     . +)
                 (label                 . 0)
                 (case-label            . +)
                 (statement-case-open   . +)
                 (statement-cont        . +)
                 (arglist-intro . +) ; change
                 (arglist-close . 0) ; change
                 (access-label  . 0)))
           (c-echo-syntactic-information-p . t)
           )
         "My Java Style")

(defconst my-java-style2
         '((c-basic-offset . 4) ; change
           (tab-width . 4)
           (indent-tabs-mode . nil)
           (c-offsets-alist .
                ((inline-open           . 0)
                 (inline-close          . 0)
                 (class-open            . 0)
                 (class-close           . 0)
                 (inclass               . +)
                 (inexpr-class          . 0)
                 (statement-block-intro . +)
                 (knr-argdecl-intro     . 5)
                 (substatement-open     . 0)
                 (label                 . 0)
                 (case-label            . +)
                 (statement-case-open   . +)
                 (statement-cont        . +)
                 (arglist-intro . +) ; change
                 (arglist-close . 0) ; change
                 (access-label  . 0)))
           (c-echo-syntactic-information-p . t)
           )
         "My Java Style2")

;;; java-mode-indent-annotations
;(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook
          '(lambda ()
             ;;(gtags-mode 1)
             (c-add-style "my-java-style2" my-java-style2 t)
             (c-set-style "my-java-style2")))
             ;(java-mode-indent-annotations-setup)))

;;; global(gtags)
(add-hook 'gtags-mode-hook
          '(lambda ()
             (setq gtags-pop-delete t)
             (setq gtags-path-style 'absolute)
             (local-set-key "\M-t" 'gtags-find-tag)
             (local-set-key "\M-r" 'gtags-find-rtag)
             (local-set-key "\M-s" 'gtags-find-symbol)
             (local-set-key "\C-t" 'gtags-pop-stack)))
;Ctrl+t gtagsでジャンプする一つ前の状態に戻る
;Alt+s 指定した変数、定義の定義元を探す
;Alt+r 指定した関数が参照されている部分を探す
;Alt+t 指定した関数が定義されている部分をさがす
;gtags -v
;htags -saF ->html化

(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)))

;;; text mode
(add-hook 'text-mode-hook
          '(lambda ()
             ;; 76文字幅でオートインデント
             (set-fill-column 76)))

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (jedi:setup)
;;              (setq indent-tabs-mode nil)))
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)
;; (autoload 'jedi:setup "jedi" nil t)

;; (when (require 'flycheck-pos-tip nil t)
;;   (with-eval-after-load 'flycheck
;;     (flycheck-pos-tip-mode)))
(when (require 'flycheck-posframe nil t)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(when (require 'flycheck nil t)
  (global-flycheck-mode))

(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))

(with-eval-after-load 'company
  ;(setq company-auto-complete nil)
  ;(setq company-transformers '(company-sort-by-backend-importance))
  (setq company-transformers '(company-sort-by-statistics))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer)
  (when (eq system-type 'windows-nt)
      (setq company-clang-executable "c:/Program Files/LLVM/bin/clang++.exe"))
  )
(when (require 'company nil t)
  (global-company-mode))
(when (require 'company-statistics nil t)
  (company-statistics-mode))
(when (require 'company-posframe nil t)
  (company-posframe-mode 1))

(when (require 'projectile nil t)
  (setq projectile-enable-caching t)
  (projectile-global-mode))

(when (require 'google-c-style nil t)
  (defun cc-mode-init ()
    (google-set-c-style))
;;;;; clang-format -dump-config -style=Google > .clang-format
;;;;; NamespaceIndentation: All
;;;;; IndentWidth: 2
;;;;; TabWidth: 2
  (add-hook 'c-mode-hook #'cc-mode-init)
  (add-hook 'c++-mode-hook #'cc-mode-init))

(when (require 'lsp-mode nil t)
  (setq lsp-enable-indentation nil)
  (require 'lsp-clients)
  (add-hook 'python-mode-hook #'lsp)
  (defun cc-mode-ccls ()
    (when (require 'ccls nil t)
      (if (eq system-type 'windows-nt)
          (setq ccls-executable "c:/Users/admin/work/ccls/ccls.exe")
        (setq ccls-executable "/usr/bin/ccls")))
    (lsp))
  (add-hook 'c-mode-hook #'cc-mode-ccls)
  (add-hook 'c++-mode-hook #'cc-mode-ccls)
  (add-hook 'objc-mode-hook #'cc-mode-ccls)
  (when (require 'lsp-ui nil t)
    (setq lsp-ui-doc-header nil)
    (setq lsp-ui-doc-border "violet")
    (setq lsp-ui-sideline-update-mode 'point)
    (setq lsp-ui-sideline-delay 1)
    (setq lsp-ui-sideline-ignore-duplicate t)
    (setq lsp-ui-peek-always-show t)
    (setq lsp-ui-flycheck-enable t)
    (add-hook 'lsp-mode-hook #'lsp-ui-mode))
  (when (require 'company nil t)
    (when (require 'company-lsp nil t)
      (push 'company-lsp company-backends))))

(when (require 'go-mode nil t)
  (when (require 'lsp-mode nil t)
    (add-hook 'go-mode-hook #'lsp))
  (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)))
  (add-hook 'before-save-hook #'lsp-format-buffer nil 't))
  ;(add-hook 'before-save-hook 'lsp-format-buffer nil 'local))

(when (require 'company-mode nil t)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle))

(require 'toml-mode nil t)

(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(require 'vue-mode nil t)

(when (require 'js2-mode nil t)
  (autoload 'ac-js2-mode "ac-js2" nil t)
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode)))

(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(autoload 'ebuild-mode "ebuild-mode"
  "Major mode for Portage .ebuild and .eclass files." t)
(autoload 'gentoo-newsitem-mode "gentoo-newsitem-mode"
  "Major mode for Gentoo GLEP 42 news items." t)
(autoload 'glep-mode "glep-mode"
  "Major mode for Gentoo Linux Enhancement Proposals." t)

(add-to-list 'auto-mode-alist '("\\.\\(ebuild\\|eclass\\)\\'" . ebuild-mode))
(add-to-list 'auto-mode-alist
	     '("/[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]-.+\\.[a-z]\\{2\\}\\.txt\\'"
	       . gentoo-newsitem-mode))
(add-to-list 'auto-mode-alist '("/glep.*\\.rst\\'" . glep-mode))
(add-to-list 'interpreter-mode-alist '("openrc-run" . sh-mode))
(add-to-list 'interpreter-mode-alist '("runscript" . sh-mode))
(modify-coding-system-alist 'file "\\.\\(ebuild\\|eclass\\)\\'" 'utf-8)

;(load "with-editor-autoloads")
;(load "ghub-autoloads")
;(load "magit-popup-autoloads")
;(load "magit-autoloads")

(require 'powershell nil t)
(autoload 'bat-mode "bat-mode" "batch file mode." t)
(add-to-list 'auto-mode-alist '("\\.\\(cmd\\|bat\\)$" . bat-mode))

(when (require 'cmake-mode nil t)
  (when (eq system-type 'windows-nt)
    (setenv "PATH" (concat (getenv "PROGRAMFILES") "(x86)\\CMake\\bin;" (getenv "PATH")))))

(when (require 'git-gutter+ nil t)
 (global-git-gutter+-mode t)
 (global-set-key (kbd "C-x g") 'git-gutter+-mode) ; Turn on/off in the current buffer
 (global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally

 (eval-after-load 'git-gutter+
   '(progn
   ;;; Jump between hunks
      (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
      (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

   ;;; Act on hunks
      (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
      (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
      ;; Stage hunk at point.
      ;; If region is active, stage all hunk lines within the region.
      (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
      (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
      (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
      (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
      (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))))

(when (require 'quickrun nil t)
  ;; 結果の出力バッファと元のバッファを行き来したい場合は
  ;; ':stick t'の設定をするとよいでしょう
  (push '("*quickrun*") popwin:special-display-config)
  ;; よく使うならキーを割り当てるとよいでしょう
  (global-set-key (kbd "<f5>") 'quickrun))

(when (require 'migemo nil t)
  (if (eq system-type 'windows-nt)
      ;; Windows
      (progn
        (setq migemo-command "c:/msys64/usr/local/bin/cmigemo.exe")
        (setq migemo-options '("-q" "--emacs" "-i" "\a"))
        (setq migemo-dictionary "c:/msys64/usr/local/share/migemo/utf-8/migemo-dict")
        (setq migemo-user-dictionary nil)
        (setq migemo-regex-dictionary nil)
        (setq migemo-coding-system 'utf-8-unix)
        (migemo-init))
    ;; Windows以外
    (progn
      (setq migemo-command "cmigemo")
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-dictionary "/usr/share/migemo/migemo-dict")
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8-unix)
      (migemo-init))))

;;;https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-connection-alist
      '((xxxxdb (sql-product 'postgres)
               (sql-port 5432)
               (sql-server "localhost")
               (sql-user "xxxxuser")
               (sql-password "xxxx")
               (sql-database "xxxxdb"))))

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))

(defun my-sql-xxxdb ()
  (interactive)
  (my-sql-connect 'postgres 'xxxxdb))


(defun open-file-dwim (filename)
  "Open fileInfo dwim"
  (let *((winp (eq system-type 'windows-nt))
         (opener (if (file-directory-p filename)
                     (if winp '("explorer.exe") '("gnome-open"))
                   (if winp '("gnome-open"))))
         (fn (replace-regexp-in-string "/$" "" filename))
         (args (append opener (list (if winp
                                        (replace-regexp-in-string "/" (rx "\\") fn)))))
         (process-connection-type nil))
       (apply 'start-process "open-file-dwim" nil args)))

(defun dired-open-dwim ()
  "Open file under the cursor"
  (interactive)
  (open-file-dwim (dired-get-filename)))

(defun dired-open-here ()
  "Open current directory"
  (interactive)
  (open-file-dwim (expand-file-name dired-directory)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-c o") 'dired-open-dwim)
            (define-key dired-mode-map (kbd "C-c .") 'dired-open-here)))

;; (defun exec-filemanager ()
;;   (interactive)
;;   (let ((process-connection-type nil))
;;     (start-process "nautilus" nil "/usr/bin/nautilus"
;;                    (or (file-name-directory buffer-file-name)
;;                        default-directory))))
(defun exec-filemanager ()
  (interactive)
  (call-process "nautilus" nil 0 nil
                (or (file-name-directory buffer-file-name)
                    default-directory)))

(defalias 'nau 'exec-filemanager)

(if (require 'comment-dwim-2 nil t)
    (global-set-key (kbd "M-;") 'comment-dwim-2)
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

(if (require 'smartparens nil t)
    (smartparens-global-mode t)
  (electric-pair-mode t))

(show-paren-mode 1)

;; バッファ自動読み込み
(global-auto-revert-mode 1)

(setq backup-inhibited t)
(setq make-backup-files nil)

;; place backup files here, rather than sprinkling them everywhere.
;; https://github.com/josteink/emacs-oob-reboot/issues/25
(setq backup-directory-alist
      `((".*" . ,(expand-file-name
                  (concat user-emacs-directory "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq delete-auto-save-files t)

(setq backup-by-copying t)

(column-number-mode t)
(line-number-mode t)

(auto-compression-mode t)

(which-function-mode 1)

(auto-image-file-mode t)

(mouse-wheel-mode t)

(setq tramp-default-method "scp")

(setq ring-bell-function 'ignore)

(when window-system
  (require 'server)
  (unless (eq (server-running-p) 't)
    (server-start)))

(cd "~/")
