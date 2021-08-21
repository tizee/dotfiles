;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-keybindings.el - Keybindings

; use COMMAND for meta
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-meta-modifier 'none))

;; Fullscreen
(when (display-graphic-p)
  (add-hook 'window-setup-hook #'utils/fix-fullscreen-cocoa)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))

; prefer y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; enhancement
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c ^" . crux-top-join-line)
         ("C-," . crux-find-user-init-file)))

; move lines up/down
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

; window switching replacement
(use-package ace-window
             :bind (("C-x o" . 'ace-window)))

(provide 'init-kbd)
