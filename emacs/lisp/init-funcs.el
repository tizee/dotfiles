;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

(defun icons-displayable-p ()
  (and tizee-emacs-icons
       (display-graphic-p)
       (require 'all-the-icons nil t)))

;; Fonts
(defun tizee/font-dir ()
  "Return current directory font installed into"
  (cond
     ;; Default Linux install directories
     ((member system-type '(gnu gnu/linux gnu/kfreebsd))
      (concat (or (getenv "XDG_DATA_HOME")
                  (concat (getenv "HOME") "/.local/share"))
              "/fonts/"))
     ;; Default MacOS install directory
     ((eq system-type 'darwin)
      (concat (getenv "HOME") "/Library/Fonts/")))
  )

; predict - closure or function output nil or non-nil value
; downloader - closure or function download the font files
(defun install-fonts-internal (font-name downloader)
  "A general function for downloading fonts"
  (interactive)

  (let* ((font-dest (tizee/font-dir))
         (known-dest? (stringp font-dest))
         (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

    (unless (file-directory-p font-dest) (mkdir font-dest t))

    (funcall downloader)

    (when known-dest?
      (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
      (shell-command-to-string (format "fc-cache -f -v")))

    (message "Successfully %s %s fonts to `%s'!"
             (if known-dest? "installed" "downloaded")
             font-name
             font-dest))
)


(defun tizee-install-all-the-icons ()
  "Install font for all-the-icons."
  (interactive)
  (install-fonts-internal "all-the-icons" (lambda () (when (bound-and-true-p all-the-icons-font-names)
      (mapc (lambda (font)
              (url-copy-file (format "https://raw.githubusercontent.com/domtronn/all-the-icons.el/master/fonts/%s" font) (expand-file-name font font-dest) t))
            all-the-icons-font-names))))
  )

(provide 'init-funcs)
