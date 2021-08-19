;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-ui.el - UI settings

; general settings {{{
; hide startup screen messages
(setq-default inhibit-startup-screen t)
; line number
(setq-default dispaly-line-numbers-width 3
              display-line-numbers-type 'relative)
(global-display-line-numbers-mode 'relative)
; scroll bar
(setq-default scroll-bar-mode -1)
; }}}

(provide  'init-ui)
