;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-globals.el - Global values

(defconst *is-mac* (eq system-type 'darwin) "Const value for macOS check")
(defconst *is-linux* (eq system-type 'gnu/linux) "Const value for Linux check")
(defconst *is-win* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)) "Const value for Windows check")

(provide 'init-globals)
