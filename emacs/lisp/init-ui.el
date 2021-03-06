;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-ui.el - UI settings

; general settings {{{

; hide startup screen messages
(setq inhibit-startup-screen -1)
; line number
(use-package line-number-settings
  :ensure nil
  :init
  (setq dispaly-line-numbers-width 3
        display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t))

; tab-settings -  tab width
(use-package tab-settings
             :ensure nil
              :init(setq-default tab-width 4
                                 fill-column 80 ;https://www.emacswiki.org/emacs/EightyColumnRule
                                 indent-tabs-mode nil))

; Cursor & Current Line
(use-package cursor-line-settings
  :ensure nil
  :init
  ; disable blinking cusor
  (blink-cursor-mode -1)
  ; highlight current line
  (global-hl-line-mode 1)
  ; (set-face-background hl-line-face "#F2F2F2")
  )
; }}}

; GUI {{{
;; Inhibit resizing frame
(setq-default frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
; hide tool bar
(tool-bar-mode -1)
; scroll bar
(scroll-bar-mode -1)
; font
(set-face-attribute 'default nil
    :font (font-spec :name "JetBrains Mono"
                        :size (cond (*is-win* 18)
                                    (*is-mac* 18)
                                   (t 18))))

;}}}

(provide 'init-ui)
; vim: foldmethod=marker foldmarker={{{,}}}
