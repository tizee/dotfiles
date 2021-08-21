;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-org.el -- org mode

(use-package org
  ;; :init (setq org-startup-indented t)
  :config
  (setq org-startup-indented t
    org-todo-keywords '((sequence "TODO" "DOING" "DONE" "FIXME"))
    org-todo-keyword-faces '(("DOING" . "orange"))))

; flymake - Emacs builtin package
(use-package flymake
  :ensure nil
  :diminish (flymake " Flym.")
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

; Abbrev mode
(use-package abbrev-mode
  :ensure nil
  :init (setq-default abbrev-mode t))

(provide 'init-org)

