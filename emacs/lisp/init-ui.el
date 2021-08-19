;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-ui.el - UI settings

; general settings {{{

; encoding
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

; backup files
(setq make-backup-files nil
      auto-save-default nil)

; hide startup screen messages
(setq inhibit-startup-screen t)
; line number
(setq dispaly-line-numbers-width 3
              display-line-numbers-type 'relative)
(global-display-line-numbers-mode 'relative)
; }}}

; GUI {{{
; hide tool bar
(tool-bar-mode -1)
; scroll bar
(scroll-bar-mode -1)
; }}}

(provide 'init-ui)
