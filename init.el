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

(defvar elpaca-installer-version 0.8)
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
;; (defun smarter-move-beginning-of-line (arg)
;;   "Move point back to indentation of beginning of line.

;; Move point to the first non-whitespace character on this line.
;; If point is already there, move to the beginning of the line.
;; Effectively toggle between the first non-whitespace character and
;; the beginning of the line.

;; If ARG is not nil or 1, move forward ARG - 1 lines first.  If
;; point reaches the beginning or end of the buffer, stop there."
;;   (interactive "^p")
;;   (setq arg (or arg 1))

;;   ;; Move lines first
;;   (when (/= arg 1)
;;     (let ((line-move-visual nil))
;;       (forward-line (1- arg))))

;;   (let ((orig-point (point)))
;;     (back-to-indentation)
;;     (when (= orig-point (point))
;;       (move-beginning-of-line 1))))

;; ;; remap C-a to `smarter-move-beginning-of-line'
;; (global-set-key "\C-a" 'smarter-move-beginning-of-line)

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

(setopt frame-title-format (format "emacs@%s : %%f" (system-name)))

(setup mwim
  (:elpaca mwim :host github :repo "alezost/mwim.el")
  (:global
   "C-a" mwim-beginning
   "C-e" mwim-end))

;; editroconfig-core-c
;; https://github.com/editorconfig/editorconfig-core-c/blob/master/INSTALL.md
;; https://github.com/editorconfig/editorconfig-core-go
(setup editorconfig
  ;;(:opt editorconfig-exec-path "/usr/bin/editorconfig")
  (:opt editorconfig-exec-path "/usr/local/bin/editorconfig.exe")
  (editorconfig-mode 1))

;; (setopt editorconfig-exec-path "c:/msys64/usr/local/bin/editorconfig.exe")
;; (editorconfig-mode 1)

(setup savehist
  (:option savehist-file (locate-user-emacs-file "savehist")
           history-length 500
           history-delete-duplicates t
           savehist-save-minibuffer-history t)
  (savehist-mode 1))

;; (use-package f :demand t)
(setup f
  (:elpaca t))
;; As this is asynchronous let's call `elpaca-await` to ensure that f.el
;; is available for use in my emacs configuration
;; (elpaca-wait)

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

(setup puni
  (:elpaca puni :host github :repo "AmaiKinono/puni")
  (puni-global-mode))

;; バッファ自動読み込み
(global-auto-revert-mode 1)

;; 行末のスペースやタブの可視化
;(setq-default show-trailing-whitespace t)

;; (setup git-commit
;;   (:elpaca t))

;; (setup git-gutter+
;;   (:elpaca t)
;;   (global-git-gutter+-mode))

(setup transient
  (:elpaca transient :host github :repo "magit/transient"))

(setup magit
  (:elpaca magit :host github :repo "magit/magit")
  (:load-after transient))

(setup diff-hl
  (:elpaca t)
  (:with-mode dired-mode
    (:hook diff-hl-dired-mode))
  (:with-mode magit-pre-refresh
    (:hook diff-hl-magit-pre-refresh))
  (:with-mode magit-post-refresh
    (:hook diff-hl-magit-post-refresh))
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1))

(setup spacious-padding
  (:elpaca t)
  (:option spacious-padding-widths
           '(:internal-border-width 8
                                    :header-line-width 4
                                    :mode-line-widht 0
                                    :tab-width 4
                                    :right-divider-width 0
                                    :scroll-bar-width 8
                                    :left-fringe-width 4
                                    :right-fringe-width 8))
  (spacious-padding-mode 1))

;; https://www.grugrut.net/posts/202408192021/
(setup corfu
  (:elpaca corfu :host github :repo "minad/corfu")
  (:opt corfu-auto t
        corfu-auto-delay 0.3
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match t
        corfu-preselect 'valid
        corfu-on-exact-match nil)
  (:option text-mode-ispell-word-completion nil
           tab-always-indent 'complete)
  (defun corfu-beginning-of-prompt()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))
  (defun corfu-end-of-prompt()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))
  (:with-map corfu-map
    (:bind
     ;; "<remap> <move-beginning-of-line>" corfu-beginning-of-prompt
     ;; "<remap> <move-end-of-line" corfu-end-of-prompt
     "TAB" corfu-insert
     "<tab>" corfu-insert
     "RET" nil
     "<return>" nil))
  (global-corfu-mode))

(setup corfu-popupinfo
  (:elpaca corfu-popupinfo :host github :repo "minad/corfu")
  (:load-after corfu)
  (corfu-popupinfo-mode +1))

(setup cape
  (:elpaca cape :host github :repo "minad/cape")
  (:opt cape-dabbrev-min-length 2))

(setup orderless
  (:elpaca orderless :host github :repo "oantolin/orderless")
  (:option completion-style '(orderless basic)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion))))
           ;; completion-category-overrides nil)
  (:with-hook corfu-mode-hook
    (:hook (lambda ()
             (setq-local orderless-matching-styles '(orderless-flex)))))
  (defun orderless-migemo (component)
    (let ((pattern (downcase (migemo-get-pattern component))))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  (:when-loaded
    (add-to-list 'orderless-matching-styles 'orderless-migemo)))

(setup prescient
  (:elpaca prescient :host github :repo "radian-software/prescient.el")
  (:opt prescient-aggressive-file-save t
        prescient-persist-mode t))

(setup corfu-prescient
  (:elpaca corfu-prescient :host github :repo "radian-software/prescient.el")
  (:load-after corfu orderless)
  (:option corfu-prescient-enable-filterring nil)
  (:opt corfu-prescient-mode t))

(setup kind-icon
  (:elpaca kind-icon :host github :repo "jdtsmith/kind-icon")
  (:opt kind-icon-default-face 'corfu-default)
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(setup corfu-terminal
  (:elpaca corfu-terminal :host "codeberg.org" :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; https://qiita.com/nobuyuki86/items/7c65456ad07b555dd67d

(setup tempel
  (:elpaca tempel :host github :repo "minad/tempel")
  (:global
   "M-+" tempel-complete
   "M-*" tempel-insert)
  (defun tempel-setup-capf(&optional arg)
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-noninterruptible
                  (cape-capf-buster
                   (cape-capf-properties
                    (cape-capf-super
                     (if arg
                         arg
                       (car completion-at-point-functions))
                     #'tempel-complete
                     #'cape-dabbrev
                     #'cape-keyword
                     #'cape-file)
                    :sort t
                    :exclusive 'no))))
                cape-dabbrev-check-other-buffers nil))
  (:with-hook prog-mode-hook
    (:hook tempel-setup-capf))
  (:with-hook text-mode-hook
    (:hook tempel-setup-capf))
  (:with-hook conf-mode-hook
    (:hook tempel-setup-capf))
  (:with-hook org-mode-hook
    (:hook tempel-setup-capf)))

(setup tempel-collection
  (:elpaca tempel-collection :host github :repo "Crandel/tempel-collection"))

(setup eglot-tempel
  (:elpaca eglot-tempel :host github :repo "fejfighter/eglot-tempel")
  (:load-after tempel eglot)
  (eglot-tempel-mode t))

(setup flycheck
  (:elpaca flycheck :host github :repo "flycheck/flycheck")
  (global-flycheck-mode))

;; choco 管理者で volta入れる
;; volta で node入れる
;; PATH
;; /ucrt64/bin:/usr/local/bin:/usr/bin:/bin:/c/Windows/System32:/c/Windows:/c/Windows/System32/Wbem:/c/Windows/System32/WindowsPowerShell/v1.0/:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/c/emacs/bin:/c/users/xx/go/bin:/c/users/xx/.cargo/bin:/C/Users/xx/AppData/Local/Volta/bin:/c/msys64/home/xx/eask-cli/bin:/c/program files/go/bin:/c/texlive/2024/bin/windows:/c/program files/inkscape/bin:/c/program files/volta
;; chocolatey

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
  (:with-mode html-mode
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
  (:with-hook eglot-managed-mode-hook
    (:hook labmda ()
           (eglot-booster-mode t))))

(setup eglot-x
  (:elpaca eglot-x :host github :repo "nemethf/eglot-x")
  (:with-map eglot-mode-map
    (:bind
     "s-." eglot-x-find-refs))
  (:when-loaded
    (eglot-x-setup)))

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
  (:option go-ts-mode-indent-offset 4)
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

(setup python-ts-mode
  (:mode-remap python-mode)
  (:hook eglot-ensure))

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

(setup indent-bars
  (:elpaca indent-bars :host github :repo "jdtsmith/indent-bars")
  (:opt indent-bars-no-descend-lists t
        indent-bars-treesit-support t
        indent-bars-treesit-ignore-blank-lines-types '("module")
        indent-bars-treesit-scope '((python function_definition class_definition for_statement if_statement with_statement while_statement)))
  (:with-mode (python-ts-mode
               yaml-ts-mode)
    (:hook indent-bars-mode)))

(setup ace-link
  (:elpaca ace-link :host github :repo "abo-abo/ace-link")
  (ace-link-setup-default))

(setup eww
  ;; (:option eww-search-prefix "https://www.google.co.jp/search?q="
  ;;          shr-use-fonts nil)
  (:load-after ace-link)
  (:option eww-search-prefix "https://www.google.co.jp/search?&gws_rd=cr&complete=0&pws=0&tbs=li:1&q="
           shr-use-fonts nil)
  (defun eww-mode-hook--rename-buffer ()
    "Rename eww browser's buffer so sites open in new page."
    (rename-buffer "eww" t))
  (defun eww-mode-hook--disable-image ()
    (setq-local shr-put-image-function 'shr-put-image-alt))
  (:hook eww-mode-hook--rename-buffer
         eww-mode-hook--disable-image
         (lambda ()
           (defun eww-disable-images ()
             "don't display image on eww"
             (interactive)
             (setq-local shr-put-image-function 'shr-put-image-alt)
             (eww-reload))
           (defun eww-enable-images ()
             "display image on eww"
             (interactive)
             (setq-local shr-put-image-function 'shr-put-image)
             (eww-reload))
           (defun shr-put-image-alt (spec alt &optional flags)
             (insert alt))
           (display-line-numbers-mode -1)))
  (:with-map eww-mode-map
    (:bind
     "f" ace-link-eww
     "r" eww-reload
     "o" eww
     "&" eww-browse-with-external-browser
     "b" eww-back-url
     "]" eww-next-url
     "[" eww-previous-url
     "g" eww-top-url
     "h" backward-char
     "j" next-line
     "k" previous-line
     "l" forward-char
     "/" isearch-forward
     "?" isearch-backward
     "c 0" eww-copy-page-url
     "p" scroll-down
     "n" isearch-next))
  (defvar eww-home-page "https://duckduckgo.com")

  (defun browse-url-with-eww ()
    (interactive)
    (let ((url-region (bounds-of-thing-at-point 'url)))
      ;; url
      (if url-region
          (eww-browse-url (buffer-substring-no-properties (car url-region)
						                                  (cdr url-region))))
      ;; org-link
      (setq browse-url-browser-function 'eww-browse-url)
      (org-open-at-point)))

  (defun eww-gohome ()
    "Go to to the homepage specified by `eww-home-page'."
    (interactive)
    (unless eww-home-page
      (user-error "No `eww-home-page` is found."))
    (eww-browse-url eww-home-page)))

;; https://blog.tomoya.dev/posts/a-new-wave-has-arrived-at-emacs/

(setup vertico
  (:elpaca vertico :host github :repo "minad/vertico")
  (:option vertico-scroll-margin 0
           vertico-count 40
           ;; vertico-resize t
           vertico-cycle t
           enable-recursive-minibuffers t
           read-extended-command-predicate #'command-completion-default-include-p)
  (:with-map vertico-map
    (:bind
     "?" minibuffer-completion-help
     "M-RET" minibuffer-force-complete-and-exit
     "M-TAB" minibuffer-complete
     "DEL" vertico-directory-delete-char
     "C-l" vertico-directory-up
     "M-q" vertico-quick-insert
     "C-q" vertico-quick-exit))
  (vertico-mode)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (:with-hook minibuffer-setup-hook
    (:hook cursor-intangible-mode)))

;; (setup vertico-posframe
;;   (:elpaca vertico-posframe :host github :repo "tumashu/vertico-posframe")
;;   (:load-after vertico posframe)
;;   (vertico-posframe-mode 1))

(setup vertico-repeat
  (:elpaca vertico-repeat :host github :repo "minad/vertico")
  (:load-after vertico)
  (:with-hook minibuffer-setup-hook
    (:hook vertico-repeat-save)))

(setup vertico-directory
  (:elpaca vertico-directory :host github :repo "minad/vertico")
  (:load-after vertico)
  (:with-map vertico-map
    (:bind
     "<backspace>" vertico-directory-delete-char)))

;; (setup vertico-buffer
;;   (:elpaca vertico-buffer :host github :repo "minad/vertico")
;;   (:load-after vertico)
;;   (:option vertico-buffer-display-action '(display-buffer-at-bottom))
;;   (vertico-buffer-mode +1))

(setup embark
  (:elpaca t)
  (:option prefix-help-command #'embark-prefix-help-command)
  (:global
   "C-." embark-act
   "C-;" embark-dwim
   "<f1> B" embark-bindings)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(setup resentf
  (recentf-mode 1))

(setup switch-buffer-functions
  (:elpaca t)
  (:load-after resentf)
  ;; https://tsuu32.hatenablog.com/entry/2019/12/16/124052
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  (:with-hook switch-buffer-functions
    (:hook my-recentf-track-visited-file)))

;; https://zenn.dev/ykrods/articles/7f2116495de1e0

;; https://joppot.info/posts/2d8a8c1d-6d7f-4cf8-a51a-0f7e5c7e3c80
(setup consult
  (:elpaca t)
  (:global
   "C-c M-x" consult-mode-command
   "C-c h" consult-history
   "C-c k" consult-kmacro
   "C-c m" consult-man
   "C-c i" consult-info
   [remap Info-search] consult-info
   "C-x M-:" consult-complex-command
   "C-x b" consult-buffer
   "C-x 4 b" consult-buffer-other-window
   "C-x 5 b" consult-buffer-other-frame
   "C-x t b" consult-buffer-other-tab
   "C-x r b" consult-bookmark
   "C-x p b" consult-project-buffer
   "M-#" consult-register-load
   "M-'" consult-register-store
   "C-M-#" consult-register
   "M-y" consult-yank-pop
   "M-g e" consult-compiler-error
   "M-g f" consult-flycheck
   "M-g g" consult-goto-line
   "M-g M-g" consult-goto-line
   "M-g o" consult-outline
   "M-g m" consult-mark
   "M-g k" consult-global-mark
   "M-g i" consult-imenu
   "M-g I" consult-imenu-multi
   "M-s d" consult-find
   "M-s c" consult-locate
   "M-s g" consult-grep
   "M-s G" consult-git-grep
   "M-s r" consult-ripgrep
   "M-s l" consult-line
   "M-s L" consult-line-multi
   "M-s k" consult-keep-lines
   "M-s u" consult-focus-lines
   "M-s e" consult-isearch-history)
  (:with-map isearch-mode-map
    (:bind
     "M-e" consult-isearch-history
     "M-s e" consult-isearch-history
     "M-s l" consult-line
     "M-s L" consult-line-multi))
  (:with-map minibuffer-local-map
    (:bind
     "M-s" consult-history
     "M-r" consult-history))
  (:with-hook completion-list-mode-hook
    (:hook consult-preview-at-point-mode))
  (:option register-preview-delay 0.5
           register-preview-function #'consult-register-format
           xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref
           consult-narrow-key "<"
           consult-project-function #'consult--default-project-function)
  (advice-add #'register-preview :override #'consult-register-window)
  (:when-loaded
    ;; https://tam5917.hatenablog.com/entry/2022/02/05/202816
    ;; カーソル下のシンボルを拾ってconsult-line発動
    ;; (defun consult-line-symbol-at-point (&optional at-point)
    ;;   (interactive "P")
    ;;   (if at-point
    ;;       (consult-line (thing-at-point 'symbol))
    ;;     (consult-line)))

    ;; consult--source-fileの公式実装では「現在バッファとして保持しているファイル」が
    ;; ファイル(recentf)の検索対象から除外されるので、
    ;; それをやめるため単純にitemsにrecentf-listだけを持ってくる。
    ;; この設定の影響範囲はconsult-bufferのみ。consult-recent-fileは影響を受けない
    (setq consult--source-recent-file
          `(:name     "File"
                      :narrow   ?f
                      :category file
                      :face     consult-file
                      :history  file-name-history
                      :action   ,#'consult--file-action
                      :enabled   ,(lambda () recentf-mode)
                      :items ,recentf-list))
    (consult-customize
     consult-theme
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     :preview-key '(:debounce 0.2 any))))

(setup consult-eglot
  (:elpaca t)
  (:load-after consult eglot))

(setup consult-eglot-embark
  (:elpaca t)
  (:load-after consult eglot embark)
  (consult-eglot-embark-mode))

(setup consult-dir
  (:elpaca t)
  (:load-after consult)
  (:global
   "C-x C-d" consult-dir)
  (:with-map minibuffer-local-completion-map
    (:bind
     "C-x C-d" consult-dir
     "C-x C-j" consult-dir-jump-file)))

(setup embark-consult
  (:elpaca t)
  (:load-after embark consult)
  (:with-mode embark-collect-mode
    (:hook consult-preview-at-point-mode)))

;; (setup projectile
;;   (:elpaca t)
;;   (:load-after consult)
;;   (:option consult-project-function (lambda (_) (projectile-project-root)))
;;   (:with-map projectile-mode-map
;;     (:bind
;;      "s-p" projectile-command-map
;;      "C-c p" projectile-command-map))
;;   (projectile-mode +1))

(setup marginalia
  (:elpaca t)
  (:with-map minibuffer-local-map
    (:bind
     "M-A" marginalia-cycle))
  (marginalia-mode))

(setup nerd-icons
  (:elpaca t)
  (:option nerd-icons-font-family "Symbols Nerd Font Mono"))

(setup nerd-icons-completion
  (:elpaca t)
  (:load-after nerd-icons vertico marginalia)
  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context
          ((and +vertico-current-arrow
                (not (bound-and-true-p vertico-flat-mode)))
           (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat (nerd-icons-faicon "nf-fa-hand_o_right" :face 'nerd-icons-blue)
                    "\t" cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat " " (nerd-icons-faicon "nf-fa-hand_o_right" :face 'nerd-icons-blue)
                  "\t" cand)
        (concat "\t" cand))))
  (nerd-icons-completion-mode))

(setup nerd-icons-dired
  (:elpaca t)
  (:load-after nerd-icons)
  (:with-hook dired-mode-hook
    (:hook nerd-icons-dired-mode)))

(setup nerd-icons-corfu
  (:elpaca t)
  (:load-after nerd-icons)
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;; (setup dirvish
;;   (:elpaca dirvish :host github :repo "alexluigit/dirvish")
;;   (dirvish-override-dired-mode))


(setup kanji-mode
  (:elpaca t)
  (:opt *km:kakasi-executable* (locate-file "kakasi.exe" exec-path)))

;; Org-Mode

(setup org
  (:global
   "C-c l" org-store-link
   "C-c c" org-capture
   "C-c a" org-agenda)
  (:option org-directory "~/.emacs.d/orgdocs"
           orgx1-default-notes-file (concat org-directory "/notes.org")
           org-agenda-files '("~/.emacs.d/orgdocs")
           org-refile-targets '((org-agenda-files :maxlevel . 3))
           org-return-follows-link t
           org-mouse-1-follows-link t)
  (:hook turn-off-auto-fill
         turn-on-visual-line-mode
         visual-fill-column-mode
         adaptive-wrap-prefix-mode))

;; https://ueeda.sakura.ne.jp/misc/cheat_sheet_for_org-journal.html
;; https://ueeda.sakura.ne.jp/misc/ways_for_howm_to_survive.html

(setup org-roam
  (:elpaca org-roam :host github :repo "org-roam/org-roam")
  (:load-after org-journal)
  (:option
   org-roam-db-location (file-truename "~/.emacs.d/org-roam.db")
   org-roam-directory (file-truename "~/.emacs.d/org-roam")
   org-roam-completion-everywhere t
   org-roam-capture-templates '(("d" "default" plain "%?"
                                 :target (file+head "%<%Y%m%d%H%M%S>-${customslug}.org"
                                                    "#+title: ${title}\n")
                                 :unnarrowed t))
   org-roam-capture-ref-templates '(("r" "ref" plain "%U\n${ref}\n%?"
                                     :target (file+head+olp "ref/%<%Y%m%H%M%S>-ref.org"
                                                            ":PROPERTIES:\n:CATEGORY: ref\n:END:\n#+title: ${title}\n"
                                                            ("${title}"))
                                     :unnarrowed t
                                     :empty-lines-before 1)))
  (:global
   "C-c n f" org-roam-node-find
   "C-c n r" org-roam-node-random)
  (:with-map org-mode-map
    (:bind
     "C-c n i" org-roam-node-insert
     "C-c n l" org-roam-buffer-toggle
     "C-c n o" org-id-get-create
     "C-c n t" org-roam-tag-add
     "C-c n a" org-roam-alias-add))
  (with-eval-after-load 'org-roam
    (require 'kanji-mode)
    (require 'cl-lib)
    (cl-defmethod org-roam-node-customslug ((node org-roam-node))
      "Return the slug of NODE."
      (let ((title (org-roam-node-title node))
            (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                               768 ; U+0300 COMBINING GRAVE ACCENT
                               769 ; U+0301 COMBINING ACUTE ACCENT
                               770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                               771 ; U+0303 COMBINING TILDE
                               772 ; U+0304 COMBINING MACRON
                               774 ; U+0306 COMBINING BREVE
                               775 ; U+0307 COMBINING DOT ABOVE
                               776 ; U+0308 COMBINING DIAERESIS
                               777 ; U+0309 COMBINING HOOK ABOVE
                               778 ; U+030A COMBINING RING ABOVE
                               779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                               780 ; U+030C COMBINING CARON
                               795 ; U+031B COMBINING HORN
                               803 ; U+0323 COMBINING DOT BELOW
                               804 ; U+0324 COMBINING DIAERESIS BELOW
                               805 ; U+0325 COMBINING RING BELOW
                               807 ; U+0327 COMBINING CEDILLA
                               813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                               814 ; U+032E COMBINING BREVE BELOW
                               816 ; U+0330 COMBINING TILDE BELOW
                               817 ; U+0331 COMBINING MACRON BELOW
                               )))
        (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                   (strip-nonspacing-marks (s) (string-glyph-compose
                                                (apply #'string
                                                       (seq-remove #'nonspacing-mark-p
                                                                   (string-glyph-decompose s)))))
                   (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
          (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                          ("__*" . "-")                   ;; remove sequential underscores
                          ("^_" . "")                     ;; remove starting underscore
                          ("_$" . "")))                   ;; remove ending underscore
                 (slug (-reduce-from #'cl-replace (km:all->romaji (strip-nonspacing-marks title)) pairs)))
            (downcase slug))))))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; javascript:location.href =
;;     'org-protocol://roam-ref?template=r&ref='
;;     + encodeURIComponent(location.href)
;;     + '&title='
;;     + encodeURIComponent(document.title)
;;     + '&body='
;;     + encodeURIComponent(window.getSelection())

(setup org-roam-timestamps
  (:elpaca org-roam-timestamps :host github :repo "tefkah/org-roam-timestamps")
  (:option org-roam-timestamps-parent-file t)
  (with-eval-after-load 'org-roam
    (org-roam-timestamps-mode)))

(setup org-journal
  (:elpaca t)
  (:option org-journal-dir (file-truename "~/.emacs.d/org-roam/journal")
           org-journal-find-file 'find-file
           org-journal-file-type 'daily
           org-journal-file-format "%Y%m%d-journal.org"
           org-journal-file-header "#+title: %Y%m%d Journal\n\n"
           org-journal-date-format "%Y-%m-%d"
           org-journal-date-prefix "* "
           org-journal-time-format "%R\n\n"
           org-journal-time-prefix "** ")
  (:global
   "C-cj" org-journal-new-entry))

(setup org-download
  (:elpaca t)
  (:option org-download-image-dir "~/.emacs.d/pictures")
  (:with-map org-mode-map
    (:bind
     "s-Y" org-download-screenshot
     "s-y" org-download-yank))
  (:with-hook dired-mode-hook
    (:hook org-download-enable)))

(which-key-mode 1)

;; https://a.conao3.com/blog/2024/7c7c265/
;; https://apribase.net/2024/07/27/modern-emacs-2024/
;; https://apribase.net/2024/05/29/emacs-elpaca-setup-el/
;; https://qiita.com/nobuyuki86/items/122e85b470b361ded0b4
;; https://uwabami.github.io/cc-env/Emacs.html
;; https://emacs.takeokunn.org/
;; https://mako-note.com/ja/python-emacs-eglot/
;; https://www.yargry.com/notebook/emacs_imenu_list.html
;; https://github.com/iocanel/emacs.d?tab=readme-ov-file
;; https://zenn.dev/takeokunn/articles/56010618502ccc
;; https://gitlab.com/jdm204/dotfiles/-/blob/master/config.org
;; https://www.patrickdelliott.com/emacs.d/
;; https://www.grugrut.net/posts/my-emacs-init-el/

;; python 関係は設定していない
;; https://tam5917.hatenablog.com/entry/2024/07/01/150557
;; https://tam5917.hatenablog.com/entry/2024/01/01/102643

(when window-system
  (require 'server)
  (unless (eq (server-running-p) 't)
    (server-start)))

(cd "~/")

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
