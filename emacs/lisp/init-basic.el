;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

; init-basic.el -- Basic settings for Emacs

; encoding
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

; default major mode
(setq-default major-mode 'fundamental-mode)
(global-linum-mode 1)

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;;; When use Emacs GUI in macOS, we need to setup the PATH variable manually to match the one used by the shell
(when *is-mac*
  ;;; credit https://www.emacswiki.org/emacs/ExecPath
  (defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match
  that used by the user's shell.

  This is particularly useful under Mac OS X and macOS, where GUI
  apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
          "[ \t\n]*$" "" (shell-command-to-string
              "$SHELL --login -c 'echo $PATH'"
                  ))))
      (setenv "PATH" path-from-shell)
      ; (message "PATH %s" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)
)

(provide 'init-basic)
