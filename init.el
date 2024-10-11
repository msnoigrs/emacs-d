;;; init.el -*- lexical-bindings:t; no-byte-compile:t -*-

(when window-system
  (setq modus-themes-bold-constructs t)
  (load-theme 'modus-operandi-tinted)

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
	    (append
	     (list
	      '(font . "fontset-user")
 	      '(vertical-scroll-bars . nil)
	      '(tool-bar-lines . 0)
	      '(menu-bar-lines . 0)
 	      '(width . 81)
              '(height . 120) ; 4K panel
              '(line-spacing . 0.15))))

    ;; Windows以外
    (setq default-frame-alist
	  ;; 全角と半角の表示幅の比率が正確に2:1になるのは
	  ;; 10.5/12/13.5/15/18pt(1.5の倍数)
	  ;(append '((font . "M+ 1mn light-10.5")
	  ;(append '((font . "Source Han Mono-9.5")
          (append
	   (list
	    '(font . "fontset-user")
 	    '(vertical-scroll-bars . nil)
	    '(tool-bar-lines . 0)
	    '(menu-bar-lines . 0)
 	    '(width . 81)
        '(height . 100)))))) ; 4K panel

(cond
 ((eq system-type 'windows-nt)
  (setq w32-get-true-file-attributes nil)
  ;(setq w32-pipe-read-delay 50)
  (setq w32-pipe-buffer-size (* 64 1024))
  (setq w32-use-native-image-API t)
  (prefer-coding-system 'utf-8-dos)
  (set-file-name-coding-system 'cp932)
  ;(set-keyboard-coding-system 'cp932)
  ;(set-terminal-coding-system 'cp932)
  ;(setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos))
  ;(add-to-list 'process-coding-system-alist '("git" utf-8 . utf-8))
  (modify-coding-system-alist
   'file ".+\\.\\(org\\|py\\|java\\|go\\)$" 'utf-8-unix))
 (t
  ((prefer-coding-system 'utf-8-unix))))


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

(setq native-comp-async-report-warnings-errors 'silent)

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
    NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

;; https://www.emacswiki.org/emacs/SetupEl
(elpaca setup (require 'setup))
(elpaca-wait)

(defun setup-wrap-to-install-package (body _name)
"Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'."
(if (assq 'elpaca setup-attributes)
    `(elpaca ,(cdr (assq 'elpaca setup-attributes)) ,@(macroexp-unprogn body))
  body))
;; Add the wrapper function
(add-to-list 'setup-modifier-list #'setup-wrap-to-install-package)
(setup-define :elpaca
  (lambda (order &rest recipe)
    (push (cond
	   ((eq order t) `(elpaca . ,(setup-get 'feature)))
	   ((eq order nil) '(elpaca . nil))
	   (`(elpaca . (,order ,@recipe))))
	  setup-attributes)
    ;; If the macro wouldn't return nil, it would try to insert the result of
    ;; `push' which is the new value of the modified list. As this value usually
    ;; cannot be evaluated, it is better to return nil which the byte compiler
    ;; would optimize away anyway.
    nil)
  :documentation "Install ORDER with `elpaca'.
The ORDER can be used to deduce the feature context."
  :shorthand #'cadr)

(setup-define :opt
  (lambda (&rest pairs)
    `(setopt ,@pairs))
  :after-loaded t)

(setup-define :mode-remap
  (lambda (src-mode)
    `(add-to-list 'major-mode-remap-alist '(,src-mode . ,(setup-get 'feature)))))

(setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup migemo
  (:elpaca migemo :host github :repo "emacs-jp/migemo")
  (:opt migemo-dictionary "c:/msys64/usr/local/share/migemo/utf-8/migemo-dict"
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix)
  (require 'cmigemo)
  ;;(require 'migemo-isearch-auto-enable)
  (cmigemo-init))

(setup posframe
  (:elpaca t))

(cond
 ((eq system-type 'windows-nt)
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  ;; IME 制御（yes/no などの入力の時に IME を off にする）
  (w32-ime-wrap-function-to-control-ime 'universal-argument)
  (w32-ime-wrap-function-to-control-ime 'read-string)
  (w32-ime-wrap-function-to-control-ime 'read-char)
  (w32-ime-wrap-function-to-control-ime 'read-from-minibuffer)
  (w32-ime-wrap-function-to-control-ime 'y-or-n-p)
  (w32-ime-wrap-function-to-control-ime 'yes-or-no-p)
  (w32-ime-wrap-function-to-control-ime 'map-y-or-n-p)
  (w32-ime-wrap-function-to-control-ime 'register-sread-with-preview))
 (t
  (when (require 'mozc nil t)
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
                      (deactivate-input-method))))))

;; sessionファイルを~/.emacs.d/sessionsに作成
(defun emacs-session-filename (SESSION-ID)
  (expand-file-name
   (concat user-emacs-directory "sessions/session." SESSION-ID)))

;; tabグローバル設定
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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

;; (defun untabify-buffer ()
;;   (interactive)
;;   (untabify 1 (point-max))
;;   (if (not (eq major-mode 'mew-draft-mode))
;;       ;; delete-trailing-whitespace does not work in mew-draft-mode.
;;       (delete-trailing-whitespace)))

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

(setopt frame-title-format (format "emacs@%s : %%f" (system-name)))

;; editroconfig-core-c
;; https://github.com/editorconfig/editorconfig-core-c/blob/master/INSTALL.md
;; https://github.com/editorconfig/editorconfig-core-go
(setup editorconfig
  ;;(:opt editorconfig-exec-path "/usr/bin/editorconfig")
  (:opt editorconfig-exec-path "/usr/local/bin/editorconfig.exe")
  (editorconfig-mode 1))

;; (setopt editorconfig-exec-path "c:/msys64/usr/local/bin/editorconfig.exe")
;; (editorconfig-mode 1)

(savehist-mode 1)

;; (use-package f :demand t)
(setup f
  (:elpaca t))
;; As this is asynchronous let's call `elpaca-await` to ensure that f.el
;; is available for use in my emacs configuration
(elpaca-wait)

(setup comment-dwim-2
  (:elpaca comment-dwim-2 :host github :repo "remyferre/comment-dwim-2")
  (:opt comment-dwim-2--inline-comment-behavior 'reindent-comment)
  (:with-map org-mode-map
    (:bind
     "M-;" org-comment-dwim-2))
  (:global
   "M-;" comment-dwim-2))

(setup expand-region
  (:elpaca expand-region :host github :repo "magnars/expand-region.el")
  (:global
   "C-=" er/expand-region))

;; https://emacs.liujiacai.net/post/038-hello-treesitter/
;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/20

;; (setup smartparens
;;   (:elpaca smartparens :host github :repo "Fuco1/smartparens")
;;   (:with-mode (sh-mode
;;                js-mode
;;                text-mode
;;                markdown-mode
;;                latex-mode
;;                go-mode
;;                html-mode
;;                rst-mode
;;                rust-mode
;;                python-ts-mode
;;                cc-mode
;;                c-ts-mode
;;                org-mode)
;;     (:hook smartparens-mode))
;;   (require 'smartparens-config))

(setup puni
  (:elpaca puni :host github :repo "AmaiKinono/puni")
  (puni-global-mode))

;; バッファ自動読み込み
(global-auto-revert-mode 1)

;; 行末のスペースやタブの可視化
;(setq-default show-trailing-whitespace t)

(setup flycheck
  (:elpaca flycheck :host github :repo "flycheck/flycheck")
  (global-flycheck-mode))

;; dprint
;; npm install -g dprint
;; pnpm add -D dprint

;; https://apribase.net/2024/06/10/emacs-reformatter/
;; yamlfmt
;; go install github.com/google/yamlfmt/cmd/yamlfmt@latest
;; https://github.com/google/yamlfmt
;; shfmt
;; go install mvdan.cc/sh/v3/cmd/shfmt@latest
;; https://github.com/mvdan/sh

(setup reformatter
  (:elpaca t)
  (reformatter-define cljstyle
    :program "cljstyle" :args '("pipe"))
  (reformatter-define dprint
    :program "dprint" :args `("fmt" "--stdin" ,buffer-file-name))
  (reformatter-define fish_indent
    :program "fish_indent" :args '("-"))
  (reformatter-define nixfmt
    :program "nixfmt" :args '("-"))
  (reformatter-define shfmt
    :program "shfmt" :args `("--indent" ,(number-to-string sh-basic-offset) "-"))
  (reformatter-define stylua
    :program "stylua" :args `("-" "--indent-type=Spaces" ,(format "--indent-width=%s" lua-indent-level)))
  (reformatter-define ruff
    :program "ruff" :args `("format" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define rufo
    :program "rufo")
  (reformatter-define taplo
    :program "taplo" :args '("fmt"))
  (reformatter-define yamlfmt
    :program "yamlfmt" :args '("-in")))

;; https://github.com/hrsh7th/vscode-langservers-extracted
;; npm -g install nexe
;; nexe -i server.js -o yaml-language-server.exe -t windows-x64-14.15.3
;;
;; svelteserver
;; vscode-css-language-server
;; vscode-json-language-server
;; vscode-markdown-language-server
;; vscode-eslint-language-server
;; typescript-language-server
;; tsserver
;; windowsではvoltaを使ってインストール
;; c:/Users/[username]/AppData/Local/Volta/bin
;; https://zenn.dev/longrun_jp/articles/volta-node-corepack-pnpm
;;
;; https://github.com/golang/tools/gopls
;;
;; Svelte
;; https://qiita.com/akirak/items/11dafdf89e32d34f3fc9

(setup eglot
  (:with-mode (python-mode
               html-mode)
    (:hook eglot-ensure))
  (:opt eglot-sync-connect 3
        eglot-connect-timeout 30
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-events-buffer-size 0
        eglot-report-progress nil
        eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :foldingRangeProvider)
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil
        eglot-report-progress nil)
  (:when-loaded
    (dolist (pair '((svelte-mode . ("svelteserver" "--stdio"))
                    (css-mode . ("vscode-css-language-server" "--stdio"))
                    (css-ts-mode . ("vscode-css-language-server" "--stdio"))
                    (json-ts-mode . ("vscode-json-language-server" "--stdio"))
                    (yaml-ts-mode . ("yaml-language-server" "--stdio"))))
      (add-to-list 'eglot-server-programs pair))))

(setup eglot-booster
  (:elpaca eglot-booster :host github :repo "jdtsmith/eglot-booster")
  (eglot-booster-mode))

(setup flycheck-eglot
  (:elpaca flycheck-eglot :host github :repo "flycheck/flycheck-eglot")
  (:load-after flycheck eglot)
  (global-flycheck-eglot-mode 1))

(setup flycheck-posframe
  (:elpaca flycheck-posframe :host github :repo "alexmurray/flycheck-posframe")
  (:with-hook flycheck-mode
    (:hook flycheck-posframe-mode)))

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; https://apribase.net/2024/06/12/emacs-treesit-ts-mode/
;; https://erick.navarro.io/blog/moving-to-emacs-tree-sitter-modes/
;; https://repo.msys2.org/mingw/ucrt64/

;; https://github.com/iquiw/emacs-tree-sitter-module-dll
;; git clone --recursive
;; mkdir -p dist/licenses
;; bash ./build.sh go UCRT64 dist

;; treesit-extra-load-path

;; https://github.com/camdencheek/tree-sitter-go-mod
;; go-ts-mode go-mod-ts-mode

;; 
(setup treesit
  (:option treesit-font-lock-level 4
           treesit-extra-load-path '("c:/msys64/home/igarashi/emacs-tree-sitter-module-dll/dist"))
  )
;;          treesit-language-source-alist
;;          '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;             (clojure "https://github.com/sogaiu/tree-sitter-clojure")
;;             (cmake "https://github.com/uyha/tree-sitter-cmake")
;;             (css "https://github.com/tree-sitter/tree-sitter-css")
;;             (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
;;             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;             (go "https://github.com/tree-sitter/tree-sitter-go")
;;             (html "https://github.com/tree-sitter/tree-sitter-html")
;;             (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;             (json "https://github.com/tree-sitter/tree-sitter-json")
;;             (make "https://github.com/alemuller/tree-sitter-make")
;;             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;             (nix "https://github.com/nix-community/tree-sitter-nix")
;;             (python "https://github.com/tree-sitter/tree-sitter-python")
;;             (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;             (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;            (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;; (mapc (lambda (lang)
;;         (unless (treesit-language-available-p lang nil)
;;           (treesit-install-language-grammar lang)))
;;       (mapcar #'car treesit-language-source-alist)))

;; https://github.com/zbelial/treesitter-context.el

(setup go-ts-mode
  (:file-match "\\.go\\'")
  (:hook eglot-ensure))

(setup go-mod-ts-mode
  (:file-match "go.mod\\'"))

(setup css-ts-mode
  (:mode-remap css-mode)
  (:hook dprint-on-save-mode
         eglot-ensure))

(setup js-ts-mode
  (:mode-remap javascript-mode)
  (:hook dprint-on-save-mode
         eglot-ensure))

(setup typescript-ts-mode
  (:file-match "\\.tsx\\'")
  (:hook dprint-on-save-mode
         eglot-ensure))

(setup json-ts-mode
  (:file-match "\\.jsonc?\\'")
  (:option js-indent-level 2)
  (:hook dprint-on-save-mode
         eglot-ensure))

(setup yaml-ts-mode
  (:file-match "\\.ya?ml\\'")
  (:hook yamlfmt-on-save-mode
         eglot-ensure))

(setup toml-ts-mode
  (:mode-remap conf-toml-mode)
  (:hook dprint-on-save-mode
         eglot-ensure))

(setup bash-ts-mode
 (:mode-remap sh-mode)
 (:option sh-basic-offset 2)
 (:hook shfmt-on-save-mode
        eglot-ensure))

;; (setup dockerfile-mode
;;   (:elpaca t)
;;   (:hook dprint-on-save-mode
;;          eglot-ensure))

(setup dockerfile-ts-mode
  (:file-match "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'")
  (:hook dprint-on-save-mode
         eglot-ensure))

(setup rust-mode
  (:elpaca t)
  (:option rust-format-on-save t
           rust-mode-treesitter-derive t)
  (:hook eglot-ensure))

;; https://mickey-happygolucky.hatenablog.com/entry/2021/08/25/221924 表
;; C-c C-h ヘルプ
;; https://jblevins.org/projects/markdown-mode/
;; https://qiita.com/tadsan/items/7bb0099479f647d2c106

;; (setup markdown-mode
;;   (:elpaca markdown-mode :host github :repo "jrblevin/markdown-mode")
;;   (:opt auto-mode-alist (cons '("\\.md\\'" . gfm-mode) auto-mode-alist)))

;; https://github.com/simonhaenisch/md-to-pdf
;; https://github.com/elliotblackburn/mdpdf
;; https://github.com/led-mirage/Markdown-PDF-Guide
;; https://github.com/sindresorhus/github-markdown-css
;; "white-space: pre;" を "white-space: pre-wrap;" に変更する（変更しないとプログラムコードが途中で途切れる）
;; https://zenn.dev/takanori_is/articles/md-to-pdf-with-marked-custom-heading-id
;; https://zenn.dev/ebang/articles/231106_emacs-markdown

(setup edit-indirect
  (:elpaca edit-indirect :host github :repo "Fanael/edit-indirect"))

(setup visual-line-mode
  (:hook word-wrap-whitespace-mode))

(setup word-wrap-mode
  (:when-loaded
   (add-to-list 'word-wrap-whitespace-characters ?\])))

(setup visual-fill-column
  (:elpaca t)
  (:opt visual-fill-column-width 80
        visual-line-fringe-indicators '(left-curly-arrow nil)))

(setup adaptive-wrap
  (:elpaca t))

(setup gfm-mode
  (:file-match "\\.md\\'")
  (:elpaca markdown-mode :host github :repo "jrblevin/markdown-mode")
  (:option markdown-command "md-to-pdf --as-html --config-file .emacs.d/md2html.js"
           markdown-open-command "~/.emacs.d/mdopen.bat"
           markdown-fontify-code-blocks-natively t
           markdown-indent-on-enter 'indent-and-new-item)
  (:hook turn-off-auto-fill
         turn-on-visual-line-mode
         visual-fill-column-mode
         adaptive-wrap-prefix-mode))

(which-key-mode 1)

;; https://a.conao3.com/blog/2024/7c7c265/
;; https://apribase.net/2024/07/27/modern-emacs-2024/
;; https://qiita.com/nobuyuki86/items/122e85b470b361ded0b4
;; https://emacs.takeokunn.org/

(cd "~/")

;; https://qiita.com/nobuyuki86/items/122e85b470b361ded0b4
;; https://zenn.dev/takeokunn/articles/56010618502ccc
;; https://gitlab.com/jdm204/dotfiles/-/blob/master/config.org
;; https://www.patrickdelliott.com/emacs.d/
;; https://apribase.net/2024/05/29/emacs-elpaca-setup-el/
;; https://www.grugrut.net/posts/my-emacs-init-el/
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((lexical-bindings . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
