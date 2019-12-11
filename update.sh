#!/bin/bash

PKGS="
app-emacs/async
comment-dwim2
company-lsp
company-mode
company-statistics
dash
company-quickhelp
posframe
company-posframe
desktop-file-utils
direx-el
dropdown-list
ebuild-mode
toml-mode
web-mode
yaml-mode
yasnippet
yasnippet-snippets
mozc-cand-posframe
mozc-el-extensions
smartparens
spinner
org-mode
ox-gfm
ox-qmd
popup-el
popwin-el
pos-tip
projectile
quickrun
emacs-ccls
emmet-mode
s
seq
f
ht
let-alist
flycheck
flycheck-posframe
go-mode
app-emacs/hydra
lsp-mode
lsp-ui
treepy
transient
libegit2
with-editor
ghub
git-modes
magit
git-gutter-plus
editorconfig-emacs
google-translate"

#mozc-popup
#flycheck-pos-tip

emerge -1 ${PKGS}
