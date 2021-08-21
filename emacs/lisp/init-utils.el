;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

; init-utils.el -- Interactive funcs or utility

;; WORKAROUND: fix blank screen issue on macOS.
(defun utils/fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen."
  (and *mac-with-cocoa*
       *emacs26*
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

(defun utils/reload-init-file ()
  "Reload the user init file."
  (interactive)
  (load-file user-init-file))

(defun utils/packages-upgrade()
  "Upgrade all the packages."
  (interactive)
  (message "Upgrading all the packages...")
  (auto-package-update-now)
  (message "All the packages are up to date.")
  (let ((buffer (get-buffer "*Compile-Log*")))
    (if buffer
        (kill-buffer buffer))))

(defun utils/user-login-info ()
  "Print the login user info as init message"
  (let ((prefix ";; Configured by utils <github.com/cabins>.\n")
        (os-version (format ";; %20s: %S\n" "Operating System" system-type))
        (user-names (format ";; %20s: %s\n" "Login User" (user-login-name)))
        (machine-name (format ";; %20s: %s\n" "Machine Name" (system-name)))
        (suffix ";; Enjoy!"))
    (concat prefix ";;\n" os-version user-names machine-name ";;\n" suffix)))

(defun utils/lsp-update-tools (pkgs program &rest args)
  "Install or update the lsp tools."

  ;; Check the language is installed correctly
  (unless (executable-find program)
    (user-error (format "Pls install %s first." program)))

  ;; Install or update the tools
  (let ((pname "lsp-tools")
        (pbuffer "*LSP Tools*"))
    (dolist (pkg pkgs)
      (set-process-sentinel
       (apply #'start-process
              pname pbuffer program (append args (list pkg)))
       nil))))

(defun utils/go-update-tools ()
  "Install or update the tools for golang development."
  (interactive)
  (utils/lsp-update-tools go--tools "go" "get" "-u" "-v"))

(defun utils/python-update-tools ()
  "Install or update modules for Python development."
  (interactive)

  (utils/lsp-update-tools python--tools "pip" "install" "-U"))

(defun utils/bash-update-tools ()
  "Install or update bash language server."
  (interactive)
  (message "Note: the version of node.js should be greater than 8.0")
  (utils/lsp-update-tools '("bash-language-server")
                           "npm" "install" "-g"))

(provide 'init-utils)
