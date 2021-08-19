
; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
; (setq package-enable-at-startup nil)

; Do not resize the frame
(setq frame-inhibit-implied-resize t)

