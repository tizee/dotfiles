;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-keybindings.el - Keybindings

; use COMMAND for meta
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-meta-modifier 'none))

; prefer y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-keybindings)
