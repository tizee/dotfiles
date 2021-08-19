;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}
;;; I also use vim's modeline for Emacas. Why? Because I like it.

;;;     ______                         
;;;   / ____/___ ___  ____ ___________
;;;  / __/ / __ `__ \/ __ `/ ___/ ___/
;;; / /___/ / / / / / /_/ / /__(__  ) 
;;;/_____/_/ /_/ /_/\__,_/\___/____/  
;;;
;;; ini-ui.el           - UI settings
;;; init-packages.el    - Packages
;;; init-globals.el     - Global constants
;;; init-keybindings.el - Key bindings
;;;

; add confs to load-path
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))

(require 'init-packages)
(require 'init-globals)
(require 'init-ui)
(require 'init-keybindings)

; personal config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

