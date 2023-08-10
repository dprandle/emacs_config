(load-file "~/.emacs.d/lisp/custom_funcs.el")

;; All of these bindings are for vanilla emacs functions, or functions I wrote that work with vanilla stuff
;; These are first so that when emacs doesn't load - don't lose all my key bindings and stuff
;; C-x
(global-unset-key (kbd "C-z"))
(define-key ctl-x-map (kbd "C-S-f") 'my-find-file-other-window)
(define-key ctl-x-map (kbd "b") 'list-buffers)
(define-key ctl-x-map (kbd ".") 'next-buffer)
(define-key ctl-x-map (kbd "C-.") 'next-buffer)
(define-key ctl-x-map (kbd ",") 'previous-buffer)
(define-key ctl-x-map (kbd "C-,") 'previous-buffer)

;; C-
(global-set-key (kbd "C-|") 'balance-windows)
(global-set-key (kbd "C-\\") 'split-window-right)
(global-set-key (kbd "C-S-m") 'add-line-below)
(global-set-key (kbd "C-S-v") 'scroll-other-window)

;; M-
(global-set-key (kbd "M-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-K") 'kill-other-buffer)
(global-set-key (kbd "M-j") 'xref-find-definitions)
(global-set-key (kbd "M-J") 'xref-find-definitions-other-window)
(global-set-key (kbd "M-\\") 'mark-paragraph)
(global-set-key (kbd "M-|") 'my-reset-splits)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-M") 'add-line-above)
(global-set-key (kbd "M-+") 'move-buffer-other-window)
(global-set-key (kbd "M-[") 'scroll-up-line)
(global-set-key (kbd "M-]") 'scroll-down-line)
(global-set-key (kbd "M-V") 'scroll-other-window-down)
(global-set-key (kbd "M-S-v") 'scroll-other-window-down)
(global-set-key (kbd "M-{") 'move-text-up)
(global-set-key (kbd "M-}") 'move-text-down)

;; C-c
(global-set-key (kbd "C-c C-;") 'comment-dwim)
(global-set-key (kbd "C-c C-k") 'my-kill-line)
(global-set-key (kbd "C-c C-l") 'copy-line)

;; C-z
(global-set-key (kbd "C-z C-m") 'open-scratch-buffer)
(global-set-key (kbd "C-z n g") 'open-global-notes)
(global-set-key (kbd "C-z n j") 'open-journal-notes)
(global-set-key (kbd "C-z n l") 'open-local-notes)
(global-set-key (kbd "C-z n p") 'open-project-todo)
(global-set-key (kbd "C-z C-,") 'open-settings)
(global-set-key (kbd "C-z ,") 'open-settings-funcs)
(global-set-key (kbd "C-z C-r") 'reload-current-buffer)
(global-set-key (kbd "C-z t t") #'(lambda () (interactive) (new-window-func-split-vert 'multi-term)))
(global-set-key (kbd "C-z t e") #'(lambda () (interactive) (new-window-func-split-vert 'eshell)))
(global-set-key (kbd "C-z t s") #'(lambda () (interactive) (new-window-func-split-vert 'shell)))
(global-set-key (kbd "C-z C-t") 'multi-term-dedicated-toggle)

;; Function
(global-set-key (kbd "<f4>") 'my-open-with-designer)
(global-set-key (kbd "<f5>") 'proj-comp)
(global-set-key (kbd "<f6>") 'proj-conf)
(global-set-key (kbd "<f7>") 'proj-run)
(global-set-key (kbd "<f8>") 'proj-install)
(global-set-key (kbd "<f9>") 'proj-package)
(global-set-key (kbd "C-<f7>") 'launch-qt-creator)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

;; Friggin annoying accidental page up/page down
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))
                       
;; Unset arrow keys and undo key so that I stop using them by accident
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))
(global-unset-key (kbd "C-x <C-left>"))
(global-unset-key (kbd "C-x <C-right>"))
(global-unset-key (kbd "C-x <left>"))
(global-unset-key (kbd "C-x <right>"))


;; Make two splits and fullscreen
(if (eq (length command-line-args) 1)
    (progn 
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      (my-reset-splits)
      (find-file "~/"))
  (progn
    (add-to-list 'default-frame-alist '(height . 60))
    (add-to-list 'default-frame-alist '(width . 150))))

(global-subword-mode 1) ;; Turn on superword mode by defaul
(show-paren-mode 1) ;; highlight matching paren
(electric-pair-mode) ;; Auto close brackets

;; Reuse the same buffer - don't make new ones
(add-to-list 'same-window-buffer-names "*compilation*")
(add-to-list 'same-window-buffer-names "*shell*")
(add-to-list 'same-window-buffer-names "*GNU Emacs*")

(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")

(defconst my-c-style
  '((c-basic-offset . 4)     ; Guessed value
    (c-hanging-braces-alist . ((substatement-open after)
                               (brace-list-open)))
    (c-hanging-colons-alist . ((member-init-intro before)
                               (inher-intro)
                               (case-label after)
                               (label after)
                               (access-label after)))
    (c-cleanup-list . (scope-operator
                       empty-defun-braces
                       defun-close-semi))
    (c-offsets-alist
     (arglist-cont . 0)      ; Guessed value
     (arglist-intro . +)     ; Guessed value
     (block-close . 0)       ; Guessed value
     (brace-entry-open . 0)  ; Guessed value
     (brace-list-close . 0)  ; Guessed value
     (brace-list-entry . 0)  ; Guessed value
     (brace-list-intro . +)  ; Guessed value
     (brace-list-open . 0)   ; Guessed value
     (class-close . 0)       ; Guessed value
     (class-open . 0)        ; Guessed value
     (cpp-define-intro . +)  ; Guessed value
     (defun-block-intro . +) ; Guessed value
     (defun-close . 0)       ; Guessed value
     (defun-open . 0)        ; Guessed value
     (else-clause . 0)       ; Guessed value
     (inclass . +)           ; Guessed value
     (inline-close . 0)      ; Guessed value
     (statement . 0)         ; Guessed value
     (statement-block-intro . +) ; Guessed value
     (statement-cont . +)    ; Guessed value
     (substatement . +)      ; Guessed value
     (substatement-open . 0) ; Guessed value
     (topmost-intro . 0)     ; Guessed value
     (topmost-intro-cont . 0) ; Guessed value
     (access-label . -)
     (annotation-top-cont . 0)
     (annotation-var-cont . +)
     (arglist-close . c-lineup-close-paren)
     (arglist-cont-nonempty . c-lineup-arglist)
     (block-open . 0)
     (c . c-lineup-C-comments)
     (case-label . 0)
     (catch-clause . 0)
     (comment-intro . c-lineup-comment)
     (composition-close . 0)
     (composition-open . 0)
     (cpp-macro . -1000)
     (cpp-macro-cont . +)
     (do-while-closure . 0)
     (extern-lang-close . 0)
     (extern-lang-open . 0)
     (friend . 0)
     (func-decl-cont . +)
     (incomposition . +)
     (inexpr-class . +)
     (inexpr-statement . +)
     (inextern-lang . +)
     (inher-cont . c-lineup-multi-inher)
     (inher-intro . +)
     (inlambda . 0)
     (inline-open . +)
     (inmodule . +)
     (innamespace . +)
     (knr-argdecl . 0)
     (knr-argdecl-intro . +)
     (label . 2)
     (lambda-intro-cont . +)
     (member-init-cont . c-lineup-multi-inher)
     (member-init-intro . +)
     (module-close . 0)
     (module-open . 0)
     (namespace-close . 0)
     (namespace-open . 0)
     (objc-method-args-cont . c-lineup-ObjC-method-args)
     (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
     (objc-method-intro .
                        [0])
     (statement-case-intro . +)
     (statement-case-open . 0)
     (stream-op . c-lineup-streamop)
     (string . -1000)
     (substatement-label . 2)
     (template-args-cont c-lineup-template-args +)))
  "My C Style")

(c-add-style "my-c-style" my-c-style)

;; Set the default vars
(setq backup-directory-alist `((".*" . , "~/.emacs.d/backup/"))
      auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup/" t))
      split-window-preferred-function '(lambda () (nil)) ;; Don't ever split windows
      vc-follow-symlinks t
      qt-version-in-use "6.3.0"
      org-latex-to-pdf-process (list "latexmk -pdf %f")
      c-default-style '((c-mode . "my-c-style") (c++-mode . "my-c-style"))) ;; Always follow symlinks and edit the source file without asking

(setq-default qt-version-in-use "6.3.0")

;; Do all package stuff last so we don't poop our pants if it fails
;; Install use-package which is the main thing needed for all the other packages we are going to get
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This is self-explanitory - just saves a bit of time
(use-package restart-emacs
  :bind (:map ctl-x-map ("z" . restart-emacs))
  :ensure t
  )

;; Setup helm which provides much better auto complete interface for emacs
(use-package helm
  :init
  (setq helm-split-window-in-side-p t)
  :config
  (helm-mode)
  :bind (("C-c C-f" . helm-imenu)
         ("C-S-s" . helm-occur)
	 ([remap find-file] . helm-find-files)
	 ([remap execute-extended-command] . helm-M-x)
	 ([remap switch-to-buffer] . helm-mini)
	 :map ctl-x-map
	 ("C-b" . helm-mini)
	 ("C-S-b" . my-helm-mini-other-window)
	 :map helm-map
	 ;; The persistant action is basically tab complete - change it so it matches what terms use
	 ("C-i" . helm-execute-persistent-action)
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-x C-a" . helm-select-action))
  :demand t
  :ensure t)

;; Projectile for project management - use helm-projectile for the extra actions on files so only a few key bindings here
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-z C-." . projectile-edit-dir-locals)
	 ("M-h" . projectile-find-other-file)
	 ("M-H" . open-header-other-window)
         :map projectile-command-map
         ("n" . create-cmake-project))
  :init
  (setq projectile-project-search-path '("~/projects")
	projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-discover-projects-in-search-path)
  (projectile-register-project-type 'cmake '("CMakeList.txt")
                                    :project-file "CMakeLists.txt"
                                    :compile 'my-cmake-get-compile-command
				    :configure 'my-cmake-get-config-command
				    :run 'my-cmake-get-run-command
                                    :install 'my-cmake-get-install-command
                                    :package 'my-cmake-get-package-command
                                    :src-dir "src/")
  :demand t
  :ensure t)

;; Iedit for fase multi-reference edits - defaults to C-; which works well
(use-package iedit
  :ensure t)

(use-package glsl-mode
  :ensure t)

;; Only git interface worth its salt (and vscode's)
(use-package magit
  :commands (magit-status)
  :ensure t)

;; Show error squiggles for a bunch of langauges
(use-package flycheck
  :ensure t)

;; Brings up a thingy with which keys you can use for a given prefix (after you type the prefix)
(use-package which-key
  :config
  (which-key-mode)
  :ensure t)

;; Auto completion interface
(use-package company
  :init
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  :bind (("C-c C-." . company-complete))
  :config
  (global-company-mode)
  :ensure t)

;; Add multiple cursors - similar to iedit but can be more useful at times
(use-package multiple-cursors
  :bind (("M-S" . mc/mark-next-like-this)
	 ("M-R" . mc/mark-previous-like-this)
	 ("C-<next>" . mc/mark-next-lines)
	 ("C-<prior>" . mc/mark-previous-lines))
  :ensure t)

(use-package company-glsl
  :after (company glsl-mode)
  :ensure t)

;; Some snippets for various languages (and ability to create our own snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1)
  :ensure t)

;; Main mode for intelligent auto completion. Uses clangd and a generated compile-commands.
;; Only setup needed is clangd installed (and in path) and a generated compile-commands.json file somewhere in the build tree.
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-semantic-tokens-enable t)
  :bind (("C-c C-a" . lsp-execute-code-action)
	 :map lsp-mode-map
	 ([remap xref-find-definitions] . lsp-find-definition)
	 ([remap xref-find-references] . lsp-find-references)
         ("M-;" . lsp-ui-peek-find-references)
	 ("C-:" . lsp-rename)
	 ("C-c d q" . open-symbol-at-point-in-qt-web-doc)
	 ("C-c d m" . open-symbol-at-point-in-mongo-doc)
	 ("C-c d b" . open-symbol-at-point-in-bson-doc)
	 ("C-c C-d" . lsp-ui-doc-glance)
         ("C-z m p" . cpp-make-pup-member)
         ("C-z m =" . cpp-pup-to-operator-equals)
         ("C-z m +" . cpp-pup-to-operator-decimal-equals)
         ("C-z m m a" . cpp-create-method-def-after)
         ("C-z m m e" . cpp-create-method-def-at-end)
         ("C-z m f a" . cpp-create-func-def-after)
         ("C-z m f e" . cpp-create-func-def-at-end)
         ("C-c f" . helm-lsp-workspace-symbol)
	 ("C-c a" . helm-lsp-code-actions)
         ("M-i" . lsp-format-region)
         ("M-I" . lsp-format-buffer)
         ("C-." . c-ptr-insert))
  :hook ((c++-mode . lsp)
	 (c-mode . lsp)
	 (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :ensure t)

;; Basically use this for the peek window - and the documentation window.
(use-package lsp-ui
  :commands lsp-ui-mode
  :after (lsp)
  :ensure t)

(use-package helm-lsp
  :commands (helm-lsp-workspace-symbol helm-lsp-code-actions)
  :ensure t)

;; Honestly, not too sure how this changes the xref displays but being consistant and sticking with helm for everything.
(use-package helm-xref
  :after (helm)
  :ensure t)

;; Awesome interface to find anything in the directory files super super fast. Needs riggrep installed and added to the path.
(use-package helm-rg
  :bind ("C-c s" . helm-rg)
  :after (helm)
  :ensure t)

;; Adds the helm actions to the projectile functions. Also gives us helm-projectile-rg which runs the helm-rg for the projectile project
;; files 
(use-package helm-projectile
  :commands (helm-projectile-switch-project helm-projectile-ag helm-projectile-find-file)
  :bind (("C-c C-s" . helm-projectile-rg)
	 ("C-c C-p" . helm-projectile-switch-project)
	 :map ctl-x-map
	 ("f" . helm-projectile-find-file)
	 ("F" . my-helm-projectile-find-file-other-window))
  :config
  (helm-projectile-on)
  :after (helm projectile)
  :ensure t)

;; Create TODO and other such tags. Tags can be added by customizing hl-todo-keyword-faces.
(use-package hl-todo
  :config
  (global-hl-todo-mode)
  :ensure t)

;; Shows the generated tags from hl-todo in magit status
(use-package magit-todos
  :commands (helm-magit-todos magit-todos-mode)
  :bind ("C-c t" . helm-magit-todos)
  :hook (magit-mode . magit-todos-mode)
  :after (helm magit)
  :ensure t)

;; Use cmake-mode to highlight cmake sources
(use-package cmake-mode
  :ensure t)

(use-package org
  :hook (org-mode . (lambda ()
                      (set (make-local-variable 'split-window-preferred-function) 'split-window-sensibly)
                      (visual-line-mode)))
  :ensure t)

(use-package w3m
  :bind
  (:map w3m-mode-map
        ("M-k" . nil)
        ("M-n" . nil))
  :ensure t)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :ensure t
  )

(use-package multi-term
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package wgrep-helm
  :after (helm wgrep)
  :ensure t)

(use-package dprandle-dark-theme
  :load-path "lisp/themes"
  :hook (after-init . (lambda ()
                        (load-theme 'dprandle-dark t)
                        (if (equal system-name "dprandle-hp")
                            (set-face-attribute 'default nil :height 132))
                        (if (equal system-name "dprandle-HP-laptop")
                            (set-face-attribute 'default nil :height 132)) ;; 180 for normal laptop
                        (if (eq system-type 'windows-nt)
                            (set-face-attribute 'default nil :family "Courier New" :height 100))
                        (if (eq system-type 'darwin)
                            (set-face-attribute 'default nil :family "Menlo" :height 112)))))

;; Here are all of the hoods - the functions are in custom funcs file
(add-hook 'kill-buffer-query-functions 'my-unkillable-scratch-buffer)
(add-hook 'c-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'find-file-hook 'ui-file-handler)
(add-hook 'term-exec-hook 'set-no-process-query-on-exit)
(add-hook 'shell-mode-hook 'my-shell-mode-setup-function)
(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-clang-arguments nil)
 '(company-idle-delay 0.0)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/lisp/themes")
 '(dired-dwim-target 'dired-dwim-target-next)
 '(display-line-numbers nil)
 '(fancy-splash-image nil)
 '(fill-column 120)
 '(git-commit-fill-column 70)
 '(global-company-mode t)
 '(global-display-line-numbers-mode t)
 '(global-hl-todo-mode t)
 '(hl-todo-color-background nil)
 '(hl-todo-highlight-punctuation ":")
 '(hl-todo-keyword-faces
   '(("BUG" . "red")
     ("TODO" . "yellow")
     ("NOTE" . "#cc9393")
     ("HACK" . "orange red")
     ("TEMP" . "violet")
     ("FIXME" . "dark orange")
     ("DEBUG" . "steel blue")))
 '(hl-todo-require-punctuation nil)
 '(ignored-local-variable-values '((qt-version-in-use . "6.5.2")))
 '(indent-tabs-mode nil)
 '(ispell-dictionary nil)
 '(line-spacing 4)
 '(lsp-clangd-binary-path "~/.emacs.d/.cache/lsp/clangd/clangd_13.0.0/bin/clangd")
 '(lsp-clangd-version "13.0.0")
 '(lsp-clients-clangd-args '("--header-insertion=never"))
 '(lsp-ui-doc-position 'at-point)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(magit-todos-mode t nil (magit-todos))
 '(magit-todos-scanner 'magit-todos--scan-with-rg)
 '(mc--reset-read-variables
   '(mc--read-char-from-minibuffer mc--register-read-with-preview mc--read-quoted-char mc--read-char))
 '(mc/always-run-for-all t)
 '(menu-bar-mode nil)
 '(multi-term-dedicated-close-back-to-open-buffer-p nil)
 '(multi-term-dedicated-max-window-height 40)
 '(multi-term-dedicated-select-after-open-p t)
 '(multi-term-dedicated-skip-other-window-p t)
 '(multi-term-dedicated-window-height 40)
 '(multi-term-scroll-show-maximum-output nil)
 '(multi-term-switch-after-close nil)
 '(ns-command-modifier 'control)
 '(ns-control-modifier 'super)
 '(org-agenda-files '("~/org" "~/org-local"))
 '(org-agenda-window-frame-fractions '(0.1 . 0.75))
 '(org-agenda-window-setup 'current-window)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-with-sub-superscripts '{})
 '(org-image-actual-width nil)
 '(org-latex-listings 'minted)
 '(org-latex-minted-options
   '(("style" "perldoc")
     ("frame" "lines")
     ("framesep" "2mm")
     ("breaklines" "")
     ("fontsize" "\\scriptsize")))
 '(org-latex-pdf-process '("latexmk -f -pdf -shell-escape -output-directory=%o %f"))
 '(org-todo-keyword-faces
   '(("REQUEST" . "orange")
     ("IN-PROGRESS" . "violet")
     ("DONE" . "green")
     ("TODO" . "yellow")
     ("CANCELLED" . "steel blue")
     ("ISSUE" . "#ff2222")
     ("BUG" . "red")
     ("FIXED" . "green")
     ("NOTE" . "#cc9393")))
 '(org-todo-keywords
   '((sequence "REQUEST(q)" "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)" "BACKBURNER(l)")
     (sequence "ISSUE(i)" "BUG(b)" "|" "FIXED(f)" "RESOLVED(r)")
     (sequence "NOTE(n)")))
 '(package-selected-packages '(vscode-dark-plus-theme use-package))
 '(qthelp-online-help nil nil nil "Customized with use-package qthelp")
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((qt-version-in-use . 6\.5\.0)
     (qt-version-in-use . 6\.4\.2)
     (qt-version-in-use . "6.2.3")
     (qt-version-in-use . "6.3.0")
     (qt-version-in-use . "6.3.1")))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(term-unbind-key-list '("C-z" "C-x" "C-h" "C-y" "<ESC>"))
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(warning-suppress-types '((use-package) (use-package) (use-package) (use-package)))
 '(window-combination-resize t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-todo ((t (:weight bold))))
 '(mode-line ((t (:background "orange" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :foreground "dark orange"))))
 '(show-paren-match ((t (:foreground "green"))))
 '(show-paren-mismatch ((t (:foreground "red"))))
 '(term-color-black ((t (:background "dim gray" :foreground "dim gray"))))
 '(term-color-blue ((t (:background "dodger blue" :foreground "dodger blue")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
