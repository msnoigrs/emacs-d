;; org-mode
(setq org-log-done 'time)
(setq org-startup-folded "showall")
(setq org-startup-truncated nil)

(require 'org-id)
;(setq org-rst-link-use-ref-role t)


;(setq org-completion-use-helm nil)

(add-hook 'org-mode-hook
		  '(lambda ()
			 (setq tab-width 2)
			 (setq indent-tabs-mode nil)
             (setq org-agenda-files (file-expand-wildcards "~/orgdocs/*.org"))
             (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                              (org-agenda-files :maxlevel . 9))))
;			 (set (make-local-variable 'system-time-locale) "C")
			 ))

; http://shibayu36.hatenablog.com/entry/2012/12/16/152155
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c b") 'org-iswitchb)
;(setq-default org-hide-leading-stars t)
(setq org-directory "~/orgdocs")
(setq org-default-notes-file "~/orgdocs/notes.org")

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "CANCELLED(c@/!)" "DEFERRED(f@/!)"))))

(setq org-todo-state-tags-triggers
         (quote (("" ("Doing") ("DEFERRED") ("WAITING"))
                 ("CANCELLED" ("Doing") ("DEFERRED") ("WAITING"))
                 ("WAITING" ("Doing") ("DEFERRED") ("WAITING" . t))
                 ("DEFERRED" ("Doing") ("DEFERRED" . t) ("WAITING"))
                 ("TODO" ("Doing" . t) ("DEFERRED") ("WAITING"))
                 ("STARTED" ("Doing" . t) ("DEFERRED") ("WAITING"))
                 ("DONE" ("Doing") ("DEFERRED") ("WAITING")))))

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      ("@idulu" . ?i)
                      ("@charmy" . ?c)
                      (:endgroup . nil)
                      ("Doing" . ?d)))

(setq org-log-done 'time)
(setq org-log-into-drawer "LOGBOOK")

(defun my-sparse-doing-tree ()
  (interactive)
  (org-tags-view nil "Doing"))
(define-key org-mode-map (kbd "C-c 3") 'my-sparse-doing-tree)


; http://qiita.com/takaxp/items/4dfa11a81e18b29143ec
; http://doc.norang.ca/org-mode.html
; http://members.optusnet.com.au/~charles57/GTD/gtd_workflow.html
; https://github.com/takaishi/.emacs.d/blob/master/conf.d/30_org-mode.org
; http://pastelwill.jp/wiki/doku.php?id=emacs:init.el#org-crypt_ツリーを暗号化する

;; org-capture
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      (quote (("b" "bookmark" entry (file "~/orgdocs/bookmark.org")
               "* %?\n[[%x]]\n")
              ("t" "todo" entry (file "~/orgdocs/refile.org")
               "* TODO %? :Doing:\n%U\n  %i\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/orgdocs/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "journal" entry (file+datetree "~/orgdocs/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              )))
 
;; org-refile
(setq org-refile-use-outline-path 'file)
; Allow refile to create parent tasks with confirmation
;(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-allow-creating-parent-nodes t)
;(setq org-outline-path-complete-in-steps nil)

;; org-agenda
(define-key global-map (kbd "C-c a") 'org-agenda)
(add-hook 'org-agenda-mode-hook
		  '(lambda ()
			 ;(setq hl-line-face 'underline)
			 ;(hl-line-mode 1)
             (org-defkey org-agenda-mode-map [(tab)]
                         '(lambda () (interactive)
                            (org-agenda-goto)
                            (with-current-buffer "*Org Agenda*"
                              (org-agenda-quit))))))

(setq ps-multibyte-buffer 'non-latin-printer)
(setq ps-right-header
	  '("/pagenumberstring load" ps-time-stamp-yyyy-mm-dd ps-time-stamp-hh:mm:ss))


;; org-table
(setq org-table-automatic-realign t)
(setq org-table-copy-increment t)

;;(require 'org-bookmark)

; org-agenda-custom-commands
;; http://skalldan.wordpress.com/2011/08/18/iphone-%E3%81%A7-org-capture/
;; http://pastelwill.jp/wiki/doku.php?id=emacs:utility.el
(setq org-mobile-directory "~/Dropbox/MobileOrg/")
; ~/Dropbox/MobileOrg/mobileorg.orgから反映させるファイル。
(setq org-mobile-inbox-for-pull "~/orgdocs/mobile.org")

;(setq org-mobile-use-encryption t) ; Enable encryption
;(setq org-mobile-encryption-password "minimini") ; Set a password

(require 'ox-rst)

(require 'ox-md)
(require 'ox-gfm)
(require 'ox-qmd)

(require 'ox-html)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

; http://qiita.com/takaxp/items/0b717ad1d0488b74429d
(require 'ox-icalendar)
(setq org-icalendar-use-scheduled '(event-if-todo))
(setq org-icalendar-use-deadline '(event-if-todo))

;; org latex
(require 'ox-latex)
;(setq org-export-latex-quotes
;  '(("en" ("\\(\\s-\\|[[(]\\)\"" . "\"") ("\\(\\S-\\)\"" . "\"") ("\\(\\s-\\|(\\)'" . "\""))
;	("ja" ("\\(\\s-\\|[[(]\\)\"" . "\"") ("\\(\\S-\\)\"" . "\"") ("\\(\\s-\\|(\\)'" . "\""))))
;(setq org-export-latex-coding-system 'utf-8-unix)
;(setq org-export-latex-date-format "%Y-%m-%d")
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
;(setq org-latex-listings 'listings)
(setq org-latex-custom-lang-environments
	  '((emacs-lisp "common-lispcode")))
(setq org-latex-minted-options
	  '(("frame" "lines")
		("fontsize" "\\scriptsize")
		("linenos" "")))
(setq org-latex-pdf-process
	  '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		"lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		"lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-export-with-section-numbers nil)
(setq org-export-time-stamp-file nil)
(setq org-export-with-email nil)
(setq org-export-with-creator nil)
(setq org-export-with-toc nil)
(setq org-export-with-sub-superscripts nil)
;(setq org-latex-with-hyperref nil)

;(setq org-latex-title-command "\\maketitle\\thispagestyle{empty}\n%")

;(setq org-latex-default-table-environment "tabu")
(setq org-latex-default-table-environment "tabular")


;; "verbatim" → "Verbatim" 置換
(defun my-org-latex-filter-fancyvrb (text backend _info)
  "Convert begin/end{verbatim} to begin/end{Verbatim}.
  Allows use of the fancyvrb latex package."
  (when (or (org-export-derived-backend-p backend 'beamer)
            (org-export-derived-backend-p backend 'latex))
    (replace-regexp-in-string
     "\\\\\\(begin\\|end\\){verbatim}"
     "\\\\\\1{Verbatim}" text)))
(add-to-list 'org-export-filter-final-output-functions
             'my-org-latex-filter-fancyvrb)


(setq org-latex-text-markup-alist '((bold . "{\\gtfamily\\bfseries %s}")
									(code . verb)
									(italic . "{\\itshape\\gtfamily %s}")
									(strike-through . "\\sout{%s}")
									(underline . "\\uline{%s}")
									(verbatim . protectedtexttt)))

(setq org-latex-default-packages-alist
	  '((""     "luatexja"  t)
        (""     "luatexja-otf"  t)
		("bold"     "luatexja-preset"  t)
		(""     "fixltx2e"  nil)
		(""     "graphicx"  t)
		(""     "float"     nil)
		(""     "wrapfig"   nil)
		("normalem" "ulem"  t)
		(""     "textcomp"  t)
		(""     "marvosym"  t)
		(""     "wasysym"   t)
		(""     "amssymb"   t)
		(""     "amstext"   nil)
		("unicode=true"     "hyperref"  nil) ;日本語化けないように
		;(""     "tabu" nil)
		(""     "array" nil)
		(""     "paralist" nil)
		("usenames,table,hyperref"     "xcolor"     t)
		"\\tolerance=1000"
		"\\definecolor{lightb}{RGB}{217,224,250}"
		"\\hypersetup{colorlinks=true}"
		)
	  )

;		"\\defaultjfontfeatures{Scale=0.92487}"
;		"\\setsansfont[Ligatures=TeX]{Verdana}"
;		"\\setsansfont[Ligatures=TeX]{Ubuntu}"
;		"\\setmonofont{Inconsolata}"

;		"\\setmainfont[Ligatures=TeX]{TexGyreTermes}"
;		"\\setsansfont[Ligatures=TeX]{TexGyreHeros}"
;		"\\setmonofont[Ligatures=TeX]{TexGyreCursor}"

;(setq org-latex-default-class "ltjsarticle")
(setq org-latex-default-class "ltjsarticle-simple")

(setq org-latex-classes nil)

(add-to-list 'org-latex-classes
			 '("ltjsarticle"
			   "\\documentclass{ltjsarticle}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{geometry}
\\geometry{left=25truemm,right=25truemm,top=22truemm,bottom=22truemm}
\\setmainfont[Ligatures=TeX]{DroidSerif}
\\setsansfont[Ligatures=TeX]{DroidSans}
\\setmonofont{Inconsolata}
\\setmainjfont[BoldFont={Source Han Sans JP-Medium}]{Source Han Serif JP-Regular}
\\setsansjfont[BoldFont={Source Han Sans JP-Bold}]{Source Han Sans JP-Regular}
\\renewcommand{\\headfont}{\\sffamily\\gtfamily\\bfseries}
\\usepackage{fancyhdr}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\newpage
  \\begin{flushleft}%
    \\let\\footnote\\thanks
        {\\sffamily\\bfseries\\LARGE\\@title\\par}%
  \\end{flushleft}%
  \\begin{flushright}%
    \\vskip -3truemm%
    {\\bfseries\\@author\\par}%
    \\vskip -1truemm%
    {\\bfseries\\@date}%
  \\end{flushright}%
  \\vskip 5truemm%
}
\\makeatother
\\pagestyle{fancy}
\\makeatletter
\\lhead{}
\\chead{}
\\rhead{}
\\lfoot{{\\small© \\@author}}
\\makeatother
\\cfoot{}
\\rfoot{{\\rmfamily\\thepage}}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0.5pt}
[EXTRA]
"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))

(add-to-list 'org-latex-classes
			 '("ltjsarticle-simple"
			   "\\documentclass{ltjsarticle}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{geometry}
\\geometry{left=25truemm,right=25truemm,top=22truemm,bottom=22truemm}
\\setmainfont[Ligatures=TeX]{DroidSerif}
\\setsansfont[Ligatures=TeX]{DroidSans}
\\setmonofont{Inconsolata}
\\setmainjfont[BoldFont={Source Han Sans JP-Medium}]{Source Han Serif JP-Regular}
\\setsansjfont[BoldFont={Source Han Sans JP-Bold}]{Source Han Sans JP-Regular}
\\renewcommand{\\headfont}{\\sffamily\\gtfamily\\bfseries}
\\usepackage{fancyhdr}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\newpage
  \\begin{flushleft}%
    \\let\\footnote\\thanks
        {\\sffamily\\bfseries\\LARGE\\@title\\par}%
  \\end{flushleft}%
  \\begin{flushright}%
    \\vskip -3truemm%
    {\\bfseries\\@author\\par}%
    \\vskip -1truemm%
    {\\bfseries\\@date}%
  \\end{flushright}%
  \\vskip 5truemm%
}
\\makeatother
\\pagestyle{fancy}
\\makeatletter
\\lhead{}
\\chead{}
\\rhead{}
\\lfoot{}
\\makeatother
\\cfoot{}
\\rfoot{{\\rmfamily\\thepage}}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}
[EXTRA]
"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))


(add-to-list 'org-latex-classes
			 '("ltjsarticle-plane"
			   "\\documentclass{ltjsarticle}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{geometry}
\\geometry{left=25truemm,right=25truemm,top=22truemm,bottom=22truemm}
\\setmainfont[Ligatures=TeX]{DroidSerif}
\\setsansfont[Ligatures=TeX]{DroidSans}
\\setmonofont{Inconsolata}
\\setmainjfont[BoldFont={Source Han Sans JP-Medium}]{Source Han Serif JP-Regular}
\\setsansjfont[BoldFont={Source Han Sans JP-Bold}]{Source Han Sans JP-Regular}
\\renewcommand{\\headfont}{\\sffamily\\gtfamily\\bfseries}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\lfoot{}
\\cfoot{}
\\rfoot{}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}
[EXTRA]
"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))

(add-to-list 'org-latex-classes
			 '("ltjsarticle-plane-chinese"
			   "\\documentclass{ltjsarticle}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{geometry}
\\geometry{left=25truemm,right=25truemm,top=22truemm,bottom=22truemm}
\\setmainfont[Ligatures=TeX]{DroidSerif}
\\setsansfont[Ligatures=TeX]{DroidSans}
\\setmonofont{Inconsolata}
\\setmainjfont{AR PL UMing CN}
\\setsansjfont[BoldFont={Source Han Sans JP-Bold}]{Source Han Sans JP-Regular}
\\renewcommand{\\headfont}{\\sffamily\\gtfamily\\bfseries}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\lfoot{}
\\cfoot{}
\\rfoot{}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}
[EXTRA]
"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))

(add-to-list 'org-latex-classes
			 '("ltjsbook"
			   "\\documentclass{ltjsbook}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
    ("\\part{%s}" . "\\part*{%s}")
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
))


;(setq org-export-latex-hyperref-format "\\ref{%s}")


;;;

(setq org-export-impress-js-date-format-string "%Y-%m-%d")

;;;
(setq org-html-style-include-scripts nil
	  org-html-style-include-default nil)
(setq org-html-style-default
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"org-style.css\" />")


(setq latex-run-command "lualatex -shell-escape -interaction nonstopmode")
(setq tex-compile-commands
	  '(("lualatex -shell-escape -interaction nonstopmode %f")
		("evince %r.pdf")))
