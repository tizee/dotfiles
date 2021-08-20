;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-ui.el - UI settings

; general settings {{{

; encoding
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

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
;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
; hide tool bar
(tool-bar-mode -1)
; scroll bar
(scroll-bar-mode -1)
					; blinking cusor
(blink-cursor-mode -1)
; font
(set-face-attribute 'default nil
		    :font (font-spec :name "JetBrains Mono"
                        :size (cond (*is-win* 16)
                                    (*is-mac* 16)
                                   (t 16))))

;}}}

(provide 'init-ui)
