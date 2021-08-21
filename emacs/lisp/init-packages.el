;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; vim: foldmethod=marker foldmarker={{{,}}}

;;; init-packages.el - Package management

; Common packages {{{
; update keyring from gnu elpa
(use-package gnu-elpa-keyring-update)

; wakatime
(use-package wakatime-mode
             :hook (after-init . global-wakatime-mode))

; recent files
(use-package recentf-settings
  :ensure nil
  :init
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (global-set-key (kbd "C-c f") 'recentf-open-files)
  (recentf-mode 1))

(use-package diminish)
(use-package restart-emacs)
(use-package gruvbox-theme
             :init (load-theme 'gruvbox-dark-soft t))
; benckmarking
 (use-package benchmark-init
              :config (benchmark-init/activate)
              :hook (after-init . benchmark-init/deactivate))
; ivy-counsel-swiper for better searching
(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config (ivy-mode 1)
          (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
         )

(use-package counsel 
             :after ivy
             :bind (("M-x" . counsel-M-x)
                    ("C-x C-f" . counsel-find-file)
                    ("C-c g" . counsel-git)
                    ("C-h f" . counsel-describe-function)
                    ("C-h v" . counsel-describe-variable))
             )

(use-package swiper
             :after ivy
             :bind (("C-s" . swiper)
                    ("C-r" . swiper-isearch-backward))
             :config (setq swiper-include-line-number-in-search t
                           swiper-action-recenter t)
             )

(use-package which-key
              :defer nil
              :config (which-key-mode)
	      )

;;; idea from:
;;; https://andreyorst.gitlab.io/posts/2020-07-21-programming-ligatures-in-emacs/
;;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;; https://github.com/JetBrains/JetBrainsMono/wiki/List-of-supported-symbols
;;; and centaur emacs: https://github.com/seagle0128/.emacs.d/issues/245
(use-package composite
  :ensure nil
  :init (defvar composition-ligature-table (make-char-table nil))
  :hook (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
          . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; use hard-coded value instead of regexp-opt to prevent hang
  (when (>= emacs-major-version 27)
    (let ((alist 
        '((33 . "\\(?:!\\(?:==\\|[!=]\\)\\)") ;;; (?!  . ,(regexp-opt '("!==" "!!" "!=")))
          (35 . "\\(?:#\\(?:###?\\|_(\\|[!#(:=?[_{]\\)\\)") ;;; (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
          (36 . "\\(?:\\$>\\)") ;;;(?$  . ,(regexp-opt '("$>")))
          (38 . "\\(?:&&&?\\)") ;;;(?&  . ,(regexp-opt '("&&&" "&&")))
          (42 . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)") ;;;(?*  . ,(regexp-opt '("*>" "***" "*/")))
          (43 . "\\(?:\\+\\(?:\\+\\+\\|[+>]\\)\\)") ;;;(?+  . ,(regexp-opt '("+++" "+>" "++")))
          (45 . "\\(?:-\\(?:-[>-]\\|<<\\|>>\\|[<>|~-]\\)\\)") ;;; (?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
          (46 . "\\(?:\\.\\(?:\\.[.<]\\|[.=?-]\\)\\)") ;;;(?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
          (47 . ("/**" "/*" "///" "/=" "/==" "/>" "//")) ;;;(?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
          (58 . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)") ;;;(?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
          (59 . "\\(?:;;\\)") ;;;(?\; . ,(regexp-opt '(";;")))
          (60 . "\\(?:<\\(?:!--\\|\\$>\\|\\*>\\|\\+>\\|-[<>|]\\|/>\\|<[<=-]\\|=\\(?:=>\\|[<=>|]\\)\\||\\(?:||::=\\|[>|]\\)\\|~[>~]\\|[$*+/:<=>|~-]\\)\\)") ;;;(?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
          (61 . "\\(?:=\\(?:!=\\|/=\\|:=\\|=[=>]\\|>>\\|[=>]\\)\\)") ;;;(?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
          (62 . "\\(?:>\\(?:=>\\|>[=>-]\\|[]:=-]\\)\\)") ;;; (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
          (63 . "\\(?:\\?[.:=?]\\)") ;;;(?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
          (91 . "\\(?:\\[\\(?:||]\\|[<|]\\)\\)") ;;;(?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
          (92 . "\\(?:\\\\/?\\)") ;;;(?\\ . ,(regexp-opt '("\\" "\\/")))
          (93 . "\\(?:]#\\)") ;;;(?\] . ,(regexp-opt '("]#")))
          (94 . "\\(?:\\^=\\)") ;;;(?^  . ,(regexp-opt '("^=")))
          (95 . "\\(?:_\\(?:|?_\\)\\)") ;;;(?_  . ,(regexp-opt '("_|_" "__")))
          (123 . "\\(?:{|\\)") ;;;(?\{ . ,(regexp-opt '("{|")))
          (124 . "\\(?:|\\(?:->\\|=>\\||\\(?:|>\\|[=>-]\\)\\|[]=>|}-]\\)\\)") ;;; (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
          (126 . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)")) ;;;                 (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
          ))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

; company - auto-completion system
(use-package company
  :diminish (company-mode " Cmp.")
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (after-init . global-company-mode)
  :config (setq company-dabbrev-code-everywhere t
		        company-dabbrev-code-modes t
		        company-dabbrev-code-other-buffers 'all
		        company-dabbrev-downcase nil
		        company-dabbrev-ignore-case t
		        company-dabbrev-other-buffers 'all
		        company-require-match nil
		        company-minimum-prefix-length 1
		        company-show-numbers t
		        company-tooltip-limit 20
		        company-idle-delay 0
		        company-echo-delay 0
		        company-tooltip-offset-display 'scrollbar
		        company-begin-commands '(self-insert-command))
  (eval-after-load 'company
    '(add-to-list 'company-backends
                  '(company-abbrev company-yasnippet company-capf))))

; company enhancement for sorting and filtering
(use-package company-prescient
  :init (company-prescient-mode 1))

; yasnippet - snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs (concat
        (file-name-directory user-emacs-directory)
        "snippets"))
  (use-package yasnippet-snippets
    :after yasnippet)

  (use-package auto-yasnippet
    :bind (("C-o" . aya-open-line)
           ("H-w" . aya-create)
           ("H-y" . aya-expand))))

; colored delimiter
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

; highlight parentheses
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

; Indent grade guide line
(use-package indent-guide
  :hook (after-init . indent-guide-global-mode))

; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  ;; Prettify the process list
  (with-no-warnings
    (define-derived-mode process-menu-mode tabulated-list-mode "Process Menu"
      "Major mode for listing the processes called by Emacs."
      (setq tabulated-list-format `[("" ,(if (icons-displayable-p) 2 0))
                                    ("Process" 25 t)
			                        ("PID"      7 t)
			                        ("Status"   7 t)
                                    ;; 25 is the length of the long standard buffer
                                    ;; name "*Async Shell Command*<10>" (bug#30016)
			                        ("Buffer"  25 t)
			                        ("TTY"     12 t)
			                        ("Thread"  12 t)
			                        ("Command"  0 t)])
      (make-local-variable 'process-menu-query-only)
      (setq tabulated-list-sort-key (cons "Process" nil))
      (add-hook 'tabulated-list-revert-hook 'list-processes--refresh nil t))

    (defun list-processes--refresh ()
      "Recompute the list of processes for the Process List buffer.
Also, delete any process that is exited or signaled."
      (setq tabulated-list-entries nil)
      (dolist (p (process-list))
        (cond ((memq (process-status p) '(exit signal closed))
	           (delete-process p))
	          ((or (not process-menu-query-only)
	               (process-query-on-exit-flag p))
	           (let* ((icon
                       (or
                        (and (icons-displayable-p)
                             (all-the-icons-octicon "zap"
                                                    :height 1.0 :v-adjust -0.05
                                                    :face 'all-the-icons-lblue))
                        ""))
                      (buf (process-buffer p))
		              (type (process-type p))
		              (pid  (if (process-id p) (format "%d" (process-id p)) "--"))
		              (name (process-name p))
                      (status (process-status p))
		              (status `(,(symbol-name status)
                                face ,(if (memq status '(stop exit closed failed))
                                          'error
                                        'success)))
		              (buf-label (if (buffer-live-p buf)
				                     `(,(buffer-name buf)
				                       face link
				                       help-echo ,(format-message
					                               "Visit buffer `%s'"
					                               (buffer-name buf))
				                       follow-link t
				                       process-buffer ,buf
				                       action process-menu-visit-buffer)
			                       "--"))
		              (tty `(,(or (process-tty-name p) "--")
                             face font-lock-doc-face))
		              (thread
                       `(,(cond
                           ((or
                             (null (process-thread p))
                             (not (fboundp 'thread-name))) "--")
                           ((eq (process-thread p) main-thread) "Main")
		                   ((thread-name (process-thread p)))
		                   (t "--"))
                         face font-lock-doc-face))
		              (cmd
		               `(,(if (memq type '(network serial pipe))
		                      (let ((contact (process-contact p t t)))
			                    (if (eq type 'network)
			                        (format "(%s %s)"
				                            (if (plist-get contact :type)
					                            "datagram"
				                              "network")
				                            (if (plist-get contact :server)
					                            (format
                                                 "server on %s"
					                             (if (plist-get contact :host)
                                                     (format "%s:%s"
						                                     (plist-get contact :host)
                                                             (plist-get
                                                              contact :service))
					                               (plist-get contact :local)))
				                              (format "connection to %s:%s"
					                                  (plist-get contact :host)
					                                  (plist-get contact :service))))
			                      (format "(serial port %s%s)"
				                          (or (plist-get contact :port) "?")
				                          (let ((speed (plist-get contact :speed)))
				                            (if speed
					                            (format " at %s b/s" speed)
				                              "")))))
		                    (mapconcat 'identity (process-command p) " "))
                         face completions-annotations)))
	             (push (list p (vector icon name pid status buf-label tty thread cmd))
		               tabulated-list-entries)))))
      (tabulated-list-init-header))))

(use-package time
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))
; }}}

; macOS packages {{{
; (when *is-mac*)
; }}}

; Linux packages {{{
; (when *is-linux*  )
; }}}

(provide 'init-packages)
