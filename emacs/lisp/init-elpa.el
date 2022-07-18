;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

; init-elpa.el -- elpa relating settings

; Source Mirror

; skip signature checking
(setq package-check-signature nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; Sometimes mirrors fail to sync
(when (bound-and-true-p elpa-use-tsinghua-mirror)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))))

; prevent re-initialize
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

; install use-package via Emacs builtin package.el
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use use-package to manage package configurations
; use-package configuration
(eval-and-compile
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-always-demand nil
      use-package-always-minimally t
      use-package-verbose t))
(setq load-prefer-newer t)

(eval-when-compile (require 'use-package))

(provide 'init-elpa)
