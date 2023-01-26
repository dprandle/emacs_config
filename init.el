(load-file "~/.emacs.d/lisp/custom_funcs.el")

;; All of these bindings are for vanilla emacs functions, or functions I wrote that work with vanilla stuff
;; These are first so that when emacs doesn't load - don't lose all my key bindings and stuff
;; C-x
(define-key ctl-x-map (kbd "C-S-f") 'find-file-other-window)
(define-key ctl-x-map (kbd "b") 'list-buffers)
(define-key ctl-x-map (kbd ".") 'next-buffer)
(define-key ctl-x-map (kbd "C-.") 'next-buffer)
(define-key ctl-x-map (kbd ",") 'previous-buffer)
(define-key ctl-x-map (kbd "C-,") 'previous-buffer)

;; C-
(global-set-key (kbd "C-|") 'balance-windows)
(global-set-key (kbd "C-\\") 'split-window-right)
(global-set-key (kbd "C-S-m") 'add-line-below)
(global-set-key (kbd "C->") 'next-error)
(global-set-key (kbd "C-<") 'previous-error)

;; M-
(global-set-key (kbd "M-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-K") 'kill-other-buffer)
(global-set-key (kbd "M-:") 'xref-find-references)
(global-set-key (kbd "M-j") 'xref-find-definitions)
(global-set-key (kbd "M-J") 'find-definition-other-window)
(global-set-key (kbd "M-\\") 'mark-paragraph)
(global-set-key (kbd "M-|") 'my-reset-splits)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-M") 'add-line-above)
(global-set-key (kbd "M-+") 'windmove-swap-states-right)
(global-set-key (kbd "M-_") 'windmove-swap-states-left)
(global-set-key (kbd "M-[") 'scroll-up-line)
(global-set-key (kbd "M-]") 'scroll-down-line)
(global-set-key (kbd "M-{") 'move-text-up)
(global-set-key (kbd "M-}") 'move-text-down)

;; C-c
(global-set-key (kbd "C-c C-m") 'open-scratch-buffer)
(global-set-key (kbd "C-c C-n") 'open-shared-notes)
(global-set-key (kbd "C-c C-,") 'open-settings)
(global-set-key (kbd "C-c ,") 'open-settings-funcs)
(global-set-key (kbd "C-c C-r") 'reload-current-buffer)
(global-set-key (kbd "C-c C-t") 'eshell)

;; Function
(global-set-key (kbd "<f4>") 'open-with-designer)
(global-set-key (kbd "<f5>") 'proj-comp)
(global-set-key (kbd "<f6>") 'proj-conf)
(global-set-key (kbd "<f7>") 'proj-run)
(global-set-key (kbd "C-<f7>") 'launch-qt-creator)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

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
(global-unset-key (kbd "C-z"))

;; Make two splits and fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(my-reset-splits)

(global-subword-mode 1) ;; Turn on superword mode by defaul
(show-paren-mode 1) ;; highlight matching paren
(electric-pair-mode) ;; Auto close brackets

;; Reuse the same buffer - don't make new ones
(add-to-list 'same-window-buffer-names "*compilation*")
(add-to-list 'same-window-buffer-names "*shell*")
(add-to-list 'same-window-buffer-names "*GNU Emacs*")

;; Since clang format is used, really don't need much other than 4 space tab
(defconst my-c-style
  '((c-basic-offset . 4)))
(c-add-style "my-c-style" my-c-style)

;; Set the default vars
(setq backup-directory-alist `((".*" . , "~/.emacs.d/backup/"))
      auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup/" t))
      split-window-preferred-function '(lambda () (nil)) ;; Don't ever split windows
      vc-follow-symlinks t
      c-default-style '((c-mode . "my-c-style") (c++-mode . "my-c-style"))) ;; Always follow symlinks and edit the source file without asking

;; Do all package stuff last so we don't poop our pants if it fails
;; Install use-package which is the main thing needed for all the other packages we are going to get
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This is self-explanitory - just saves a bit of time
(use-package restart-emacs
  :ensure t
  )

;; Setup helm which provides much better auto complete interface for emacs
(use-package helm
  :init
  (setq helm-split-window-in-side-p t)
  :config
  (helm-mode)
  :bind (("C-c C-f" . helm-imenu)
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
	 ("C-z" . helm-select-action))
  :demand t
  :ensure t)

;; Projectile for project management - use helm-projectile for the extra actions on files so only a few key bindings here
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-c C-." . projectile-edit-dir-locals)
	 ("M-h" . projectile-find-other-file)
	 ("M-H" . open-header-other-window))
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
                                    :src-dir "src/")
  :demand t
  :ensure t)

;; Iedit for fase multi-reference edits - defaults to C-; which works well
(use-package iedit
  :ensure t)

;; This just adds some modern cpp keywords to syntax highlighting
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode)
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
  :config
  (global-company-mode)
  :ensure t)

;; Use .clang-format file (if found somewhere in dir structure for file being edited) and clang-format to format the code.
(use-package clang-format
  :bind (("M-i" . clang-format)
	 ("M-I" . clang-format-buffer))
  :ensure t)

;; Add multiple cursors - similar to iedit but can be more useful at times
(use-package multiple-cursors
  :bind (("M-S" . mc/mark-next-like-this)
	 ("M-R" . mc/mark-previous-like-this)
	 ("C-<next>" . mc/mark-next-lines)
	 ("C-<prior>" . mc/mark-previous-lines))
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
	 ( "C-:" . lsp-rename)
	 ("C-c C-d" . lsp-ui-doc-glance))
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

;; This gives us the JIT autocompleteino for symbols in the entire workspace instead of just the file
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :bind (("C-c f" . helm-lsp-workspace-symbol)
	 ("C-c a" . heml-lsp-code-actions))
  :after (helm lsp)
  :ensure t)

;; Honestly, not too sure how this changes the xref displays but being consistant and sticking with helm for everything.
(use-package helm-xref
  :after (helm)
  :ensure t)

;; Awesome interface to find anything in the directory files super super fast. Needs riggrep installed and added to the path.
(use-package helm-rg
  :after (helm)
  :ensure t)

;; Adds the helm actions to the projectile functions. Also gives us helm-projectile-rg which runs the helm-rg for the projectile project
;; files 
(use-package helm-projectile
  :commands (helm-projectile-switch-project helm-projectile-ag helm-projectile-find-file)
  :bind (("C-S-s" . helm-projectile-rg)
	 ("C-c C-p" . helm-projectile-switch-project)
	 :map ctl-x-map
	 ("f" . helm-projectile-find-file)
	 ("F" . my-helm-projectile-find-file-other-window))
  :config
  (helm-projectile-on)
  :after (helm projectile helm-rg)
  :ensure t)

;; Create TODO and other such tags. Tags can be added by customizing hl-todo-keyword-faces.
(use-package hl-todo
  :config
  (global-hl-todo-mode)
  :ensure t)

;; Shows the generated tags from hl-todo in magit status
(use-package magit-todos
  :commands (helm-magit-todos magit-todos-mode)
  :bind ("C-c t t" . helm-magit-todos)
  :hook (magit-mode . magit-todos-mode)
  :after (helm magit)
  :ensure t)

;; Here are all of the hoods - the functions are in custom funcs file
(add-hook 'kill-buffer-query-functions 'my-unkillable-scratch-buffer)
(add-hook 'c-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'find-file-hook 'ui-file-handler)
(add-hook 'term-exec-hook 'set-no-process-query-on-exit)
(add-hook 'shell-mode-hook 'set-no-process-query-on-exit)
(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.0)
 '(company-minimum-prefix-length 1)
 '(display-line-numbers nil)
 '(fill-column 140)
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
 '(ispell-dictionary nil)
 '(line-spacing 4)
 '(lsp-ui-doc-position 'at-point)
 '(menu-bar-mode nil)
 '(modern-c++-preprocessors
   '("__STDCPP_STRICT_POINTER_SAFETY__" "#pragma STDC CX_LIMITED_RANGE" "__STDC_MB_MIGHT_NEQ_WC__" "#pragma STDC FP_CONTRACT" "#pragma STDC FENV_ACCESS" "__has_cpp_attribute" "__STDC_ISO_10646__" "__STDCPP_THREADS__" "__STDC_VERSION__" "__STDC_HOSTED__" "__has_include" "#pragma pack" "#pragma once" "__cplusplus" "__VA_ARGS__" "__VA_OPT__" "__TIME__" "__STDC__" "__LINE__" "__FILE__" "__DATE__" "#include" "#defined" "_Pragma" "#pragma" "#ifndef" "#define" "#undef" "#ifdef" "#error" "#endif" "#line" "#else" "#elif" "#if"))
 '(ns-command-modifier 'control)
 '(ns-control-modifier 'meta)
 '(package-selected-packages '(use-package))
 '(qthelp-online-help nil nil nil "Customized with use-package qthelp")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(window-combination-resize t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1e1e1e" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 126 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(button ((t (:inherit nil :underline t))))
 '(company-tooltip ((t (:background "#252525"))))
 '(company-tooltip-selection ((t (:background "#04395e"))))
 '(cursor ((t (:background "green2"))))
 '(diff-file-header ((t (:inherit region :extend t :weight bold))))
 '(diff-header ((t (:inherit region :extend t))))
 '(ediff-current-diff-C ((t (:extend t :background "DarkOrange4"))))
 '(ediff-even-diff-A ((t (:inherit highlight :extend t))))
 '(ediff-even-diff-Ancestor ((t (:inherit highlight :extend t))))
 '(ediff-even-diff-B ((t (:inherit highlight :extend t))))
 '(ediff-even-diff-C ((t (:inherit highlight :extend t))))
 '(ediff-odd-diff-A ((t (:inherit region :extend t))))
 '(ediff-odd-diff-Ancestor ((t (:inherit highlight :extend t))))
 '(ediff-odd-diff-B ((t (:inherit region :extend t))))
 '(ediff-odd-diff-C ((t (:inherit region :extend t))))
 '(font-lock-builtin-face ((t (:foreground "orchid"))))
 '(font-lock-comment-face ((t (:foreground "#6A9955"))))
 '(font-lock-constant-face ((t (:foreground "#4FC1FF"))))
 '(font-lock-function-name-face ((t (:foreground "salmon" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#569cd6" :weight normal))))
 '(font-lock-negation-char-face ((t (:foreground "#9cdcfe"))))
 '(font-lock-string-face ((t (:foreground "#CE9178"))))
 '(font-lock-type-face ((t (:foreground "#4ec9b0"))))
 '(font-lock-variable-name-face ((t (:foreground "wheat1"))))
 '(highlight ((t (:foreground "orchid" :underline t))))
 '(hl-todo ((t (:weight bold))))
 '(lsp-face-highlight-read ((t (:foreground "#9cdcfe" :underline t))))
 '(lsp-face-semhl-default-library ((t (:inherit nil))))
 '(lsp-face-semhl-interface ((t nil)))
 '(lsp-face-semhl-keyword ((t nil)))
 '(lsp-face-semhl-method ((t (:inherit lsp-face-semhl-function :weight normal))))
 '(lsp-face-semhl-parameter ((t (:inherit font-lock-variable-name-face :foreground "#9cdcfe"))))
 '(lsp-ui-peek-filename ((t (:foreground "seashell1" :weight bold))))
 '(lsp-ui-peek-header ((t (:background "#1a1a1a" :foreground "seashell1" :box (:line-width 2 :color "#3794FF") :weight bold))))
 '(lsp-ui-peek-highlight ((t (:background "DarkOrange4"))))
 '(lsp-ui-peek-line-number ((t (:foreground "gray70"))))
 '(lsp-ui-peek-list ((t (:background "#191919"))))
 '(lsp-ui-peek-peek ((t (:background "#001F33"))))
 '(lsp-ui-peek-selection ((t (:inherit region))))
 '(magit-blame-highlight ((t (:inherit region :extend t))))
 '(magit-diff-hunk-heading ((t (:inherit region :extend t))))
 '(magit-diff-hunk-heading-highlight ((t (:inherit highlight :extend t))))
 '(magit-section-highlight ((t (:inherit region))))
 '(mc/cursor-face ((t (:inherit cursor :foreground "black"))))
 '(region ((t (:extend t :background "#264f78"))))
 '(show-paren-match ((t (:foreground "green"))))
 '(show-paren-mismatch ((t (:foreground "red")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; These need to go last because the above changes default font - this will change for mac and linux
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Menlo" :height 112))
