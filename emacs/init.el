;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}
;;; I also use vim's modeline for Emacas. Why? Because I like it.

;;;     ______
;;;   / ____/___ ___  ____ ___________
;;;  / __/ / __ `__ \/ __ `/ ___/ ___/
;;; / /___/ / / / / / /_/ / /__(__  )
;;;/_____/_/ /_/ /_/\__,_/\___/____/
;;;
;;; init-globals.el     - Global constants
;;; init-basic.el       - Basic settings
;;; init-elpa.el        - Elpa settings
;;; ini-utils.el        - Interactive funcs or utility
;;; ini-ui.el           - UI settings
;;; ini-org.el          - org mode
;;; init-packages.el    - Package List
;;; init-lsp.el         - lsp mode
;;; init-kbd.el         - Key bindings
;;; init-edit.el        - edit enhancement
;;;

(let ((minver "27.2"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.2")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

; add confs to load-path
; load packages under lisp
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))

(require 'init-globals)
(require 'init-utils)
(require 'init-basic)
(require 'init-modeline)
(require 'init-elpa)
(require 'init-packages)
(require 'init-lsp)
(require 'init-org)
(require 'init-ui)
(require 'init-edit)
(require 'init-modeline)
(require 'init-kbd)

; config via the package 'customize' interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

; roswell helper
(setq roswell-helper-file (expand-file-name "~/.roswell/helper.el"))
(when (file-exists-p roswell-helper-file)
  (message "load file %s." roswell-helper-file)
  (load-file roswell-helper-file)
  (setq inferior-lisp-program "ros -Q run"))
