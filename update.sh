#!/bin/bash

PKGS="
app-emacs/async
comment-dwim2
company-lsp
company-mode
company-statistics
app-emacs/dash
posframe
company-posframe
desktop-file-utils
direx-el
dropdown-list
app-emacs/ebuild-mode
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
org-contrib
ox-gfm
ox-qmd
popup-el
popwin-el
pos-tip
company-quickhelp
projectile
quickrun
emacs-ccls
emmet-mode
s
f
ht
flycheck
flycheck-posframe
go-mode
rust-mode
carog-el
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
google-translate
powershell-mode"

#mozc-popup
#flycheck-pos-tip

emerge -1 ${PKGS}
