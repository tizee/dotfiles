;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-packages.el - Package management

; Source Mirror 
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
; skip signature checking
(setq package-check-signature nil)
(require 'package)

; prevent re-initialize
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize)
  )

(unless package-archive-contents
  (package-refresh-contents))

; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; use-package configuration
(eval-and-compile 
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-always-demand nil
      use-package-always-minimally t
      use-package-verbose t))
(setq load-prefer-newer t)

(eval-when-compile (require 'use-package))
; (require 'use-package)

; Package list {{{
(use-package diminish)
(use-package restart-emacs)
(use-package gruvbox-theme
             :init (load-theme 'gruvbox-dark-soft t))
; benckmarking
 (use-package benchmark-init
              :config (benchmark-init/activate)
              :hook (after-init . benchmark-init/deactivate))
; ivy-counsel-swiper for better searching
(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config (ivy-mode 1)
          (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
         )

(use-package counsel 
             :after ivy
             :bind (("M-x" . counsel-M-x)
                    ("C-x C-f" . counsel-find-file)
                    ("C-c g" . counsel-git)
                    ("C-h f" . counsel-describe-function)
                    ("C-h v" . counsel-describe-variable))
             )

(use-package swiper
             :after ivy
             :bind (("C-s" . swiper)
                    ("C-r" . swiper-isearch-backward))
             :config (setq swiper-include-line-number-in-search t
                           swiper-action-recenter t)
             )

(use-package which-key
              :defer nil
              :config (which-key-mode)
	      )

;;; ideas from:
;;; https://andreyorst.gitlab.io/posts/2020-07-21-programming-ligatures-in-emacs/
;;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;; https://github.com/JetBrains/JetBrainsMono/wiki/List-of-supported-symbols
;;; and centaur emacs
(use-package composite
  :ensure nil
  :init (defvar composition-ligature-table (make-char-table nil))
  :hook (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
          . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; use hard-coded value instead of regexp-opt to prevent hang
  (when (>= emacs-major-version 27)
    (let ((alist 
        '((33 . "\\(?:!\\(?:==\\|[!=]\\)\\)") ;;; (?!  . ,(regexp-opt '("!==" "!!" "!=")))
          (35 . "\\(?:#\\(?:###?\\|_(\\|[!#(:=?[_{]\\)\\)") ;;; (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
          (36 . "\\(?:\\$>\\)") ;;;(?$  . ,(regexp-opt '("$>")))
          (38 . "\\(?:&&&?\\)") ;;;(?&  . ,(regexp-opt '("&&&" "&&")))
          (42 . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)") ;;;(?*  . ,(regexp-opt '("*>" "***" "*/")))
          (43 . "\\(?:\\+\\(?:\\+\\+\\|[+>]\\)\\)") ;;;(?+  . ,(regexp-opt '("+++" "+>" "++")))
          (45 . "\\(?:-\\(?:-[>-]\\|<<\\|>>\\|[<>|~-]\\)\\)") ;;; (?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
          (46 . "\\(?:\\.\\(?:\\.[.<]\\|[.=?-]\\)\\)") ;;;(?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
          (47 . ("/**" "/*" "///" "/=" "/==" "/>" "//")) ;;;(?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
          (58 . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)") ;;;(?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
          (59 . "\\(?:;;\\)") ;;;(?\; . ,(regexp-opt '(";;")))
          (60 . "\\(?:<\\(?:!--\\|\\$>\\|\\*>\\|\\+>\\|-[<>|]\\|/>\\|<[<=-]\\|=\\(?:=>\\|[<=>|]\\)\\||\\(?:||::=\\|[>|]\\)\\|~[>~]\\|[$*+/:<=>|~-]\\)\\)") ;;;(?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
          (61 . "\\(?:=\\(?:!=\\|/=\\|:=\\|=[=>]\\|>>\\|[=>]\\)\\)") ;;;(?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
          (62 . "\\(?:>\\(?:=>\\|>[=>-]\\|[]:=-]\\)\\)") ;;; (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
          (63 . "\\(?:\\?[.:=?]\\)") ;;;(?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
          (91 . "\\(?:\\[\\(?:||]\\|[<|]\\)\\)") ;;;(?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
          (92 . "\\(?:\\\\/?\\)") ;;;(?\\ . ,(regexp-opt '("\\" "\\/")))
          (93 . "\\(?:]#\\)") ;;;(?\] . ,(regexp-opt '("]#")))
          (94 . "\\(?:\\^=\\)") ;;;(?^  . ,(regexp-opt '("^=")))
          (95 . "\\(?:_\\(?:|?_\\)\\)") ;;;(?_  . ,(regexp-opt '("_|_" "__")))
          (123 . "\\(?:{|\\)") ;;;(?\{ . ,(regexp-opt '("{|")))
          (124 . "\\(?:|\\(?:->\\|=>\\||\\(?:|>\\|[=>-]\\)\\|[]=>|}-]\\)\\)") ;;; (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
          (126 . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)")) ;;;                 (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
          ))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))


; }}}

(provide 'init-packages)
