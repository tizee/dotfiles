;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; vim: foldmethod=marker foldmarker={{{,}}}
;;; I also use vim's modeline for Emacas. Why? Because I like it.
;;;     ______                         
;;;   / ____/___ ___  ____ ___________
;;;  / __/ / __ `__ \/ __ `/ ___/ ___/
;;; / /___/ / / / / / /_/ / /__(__  ) 
;;;/_____/_/ /_/ /_/\__,_/\___/____/  
;;;
;;; init-ui.el - UI settings
;;; init-key.el - Key bindings
;;; init-packages.el - Packages
;;;

; add confs to load-path
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))

(require 'init-packages)
(require 'init-ui)

; personal config
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(when (file-exists-p custom-file)
  (load-file custom-file))
