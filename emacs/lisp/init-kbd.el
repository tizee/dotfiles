;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-keybindings.el - Keybindings

;;; Meta - macOS option, windows left Windows
;;; super - macOS command, windows left Alt
;;; Ctrl - CapsLock

; macOS config
; use COMMAND for super
; use option for meta
(when *is-mac*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

;; macOS keyboard mappings
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-x") 'kill-region)
; reload config
(global-set-key (kbd "<f1>") #'utils/reload-init-file)
; reload init file
(global-set-key (kbd "<f2>") #'utils/open-init-file)


;; Fullscreen
(when (display-graphic-p)
  (add-hook 'window-setup-hook #'utils/fix-fullscreen-cocoa)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))

; prefer y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; enhancement with crux
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c ^" . crux-top-join-line)
         ("C-," . crux-find-user-init-file)))

;; move lines up/down with drag-stuff
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

; window switching replacement
(use-package ace-window
             :bind (("C-x o" . 'ace-window)))

(provide 'init-kbd)
