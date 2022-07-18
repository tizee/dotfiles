;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-globals.el - Global constants

(defconst *is-mac* (eq system-type 'darwin) "Const value for macOS check")
(defconst *is-linux* (eq system-type 'gnu/linux) "Const value for Linux check")
(defconst *is-win* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)) "Const value for Windows check")

(defconst *emacs26*
          (>= emacs-major-version 26)
          "Emacs version >= 26")

(defconst *emacs27*
          (>= emacs-major-version 27)
          "Emacs version >= 27")

(defconst *emacs27*
          (>= emacs-major-version 28)
          "Emacs version >= 28")

(defconst *mac-with-cocoa*
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")
(defconst elpa-use-tsinghua-mirror
          1
          "Use Tsinghua Tuna Mirror")

(provide 'init-globals)
