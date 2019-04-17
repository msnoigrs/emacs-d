;http://d.hatena.ne.jp/rubikitch/20081104/1225745862

(setq view-read-only t)

(defun view-goto-line-last (&optional line)
  (interactive "P")
  (View-goto-line (if (integerp line) line
                    (line-number-at-pos (point-max)))))

(defun view-mode-hook0 ()
  (hl-line-mode 1)
  (define-key view-mode-map (kbd "h") 'backward-word)
  (define-key view-mode-map (kbd "l") 'forward-word)
  (define-key view-mode-map (kbd "G") 'view-goto-line-last)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
  (define-key view-mode-map (kbd "v") 'toggle-view-mode)
  )
(add-hook 'view-mode-hook 'view-mode-hook0)

;; 書き込み不能なファイルはview-modeで開くように
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))
;; 書き込み不能な場合はview-modeを抜けないように
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))

(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)

;;; end view-mode-key

;http://d.hatena.ne.jp/Ubuntu/20081226/1230284322
;; C-x C-y または C-x y で view-mode を切り替える
(defun toggle-view-mode ()
  (interactive)
  (cond (view-mode
	 (view-mode nil)
	 (setq hl-line-mode nil))
	(t
	 (view-mode))))
(define-key global-map "\C-x\C-y" 'toggle-view-mode)
(define-key global-map "\C-x\ y" 'toggle-view-mode)

;http://d.hatena.ne.jp/znz/20081226/emacs
;(static-when (functionp 'hl-line-mode)
;	     (add-hook 'view-mode-hook '(lambda () (hl-line-mode 1)))
;	     (defadvice view-mode-disable (after disable-hl-line-mode activate)
;	       (hl-line-mode -1))
;	     )

;http://masutaka.net/chalow/2009-03-08-2.html
(eval-after-load "view"
  '(setcar (cdr (assq 'view-mode minor-mode-alist))
           (list (propertize " View"
                             'face
                             '(:foreground "white" :background "DeepPink1")))))
