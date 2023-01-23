(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(use-package lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode multiple-cursors modern-cpp-font-lock clang-format))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'use-package)
(use-package lsp-ui)

(require 'iedit)

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(define-key global-map [remap other-window] #'ace-select-window)
(define-key global-map [remap delete-other-windows] #'ace-delete-other-windows)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; Turn on superword mode by defaul
(global-subword-mode 1)

;; highlight matching paren
(show-paren-mode 1)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

(defun reset-splits ()
  "Reset windows to default state"
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun open-settings ()
  (interactive)
  (find-file "~/.emacs"))

(defun open-shared-notes()
  (interactive)
  (find-file "~/.emacs.d/notes.md"))

(defun open-header-other-window()
  "Open header in other window"
  (interactive)
  (setq fname (buffer-file-name))
  (other-window 1)
  (find-file fname)
  (helm-projectile-find-other-file))

(defun add-line-above()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (c-indent-line-or-region))

(defun add-line-below()
  (interactive)
  (end-of-line)
  (newline)
  (c-indent-line-or-region)) 

(defun open-with-designer()
  (interactive)
  (shell-command (format "nohup designer %s </dev/null &>/dev/null &" (buffer-file-name))))

(defun open-scratch-buffer()
  "Create or get scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (lisp-interaction-mode))

(defun restart-debug()
  "Restart the debug session"
  (interactive)
  (dap-debug-last))

(defun end-debug()
  "End the debug session and restore the windows"
  (interactive)
  (dap-disconnect (dap--cur-session))
  (reset-splits))

(defun ui-file-handler ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.ui\\'" buffer-file-name))
    (switch-to-prev-buffer)
    (shell-command (format "nohup designer %s </dev/null &>/dev/null &" (buffer-file-name)))
    (kill-buffer "*Async Shell Command*")
    (kill-buffer (get-file-buffer buffer-file-name))))
(add-hook 'find-file-hook 'ui-file-handler)

;; Make the scratch buffer just switch to whatever the previous buffer was instead of killing
(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (switch-to-prev-buffer)
	      nil)
	  t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)


(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(defun find-definition-other-window()
  "Follow definition under cursor in other window"
  (interactive)
  (setq fname (buffer-file-name))
  (other-window 1)
  (find-file fname)
  (lsp-find-definition))

(defun casey-never-split-a-window()
    "Never, ever split a window.  Why would anyone EVER want you to do that??"
    nil)
(setq split-window-preferred-function 'casey-never-split-a-window)

(add-to-list 'same-window-buffer-names "*compilation*")
(add-to-list 'same-window-buffer-names "*shell*")

;; Dont whine if there is a terminal open
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-exec-hook 'set-no-process-query-on-exit)
(add-hook 'shell-mode-hook 'set-no-process-query-on-exit)

;; define custom keys..
(global-set-key (kbd "M-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-:") 'xref-find-references)
(global-set-key (kbd "M-j") 'xref-find-definitions)
(global-set-key (kbd "M-J") 'find-definition-other-window)
(global-set-key (kbd "C-|") 'balance-windows)
(global-set-key (kbd "C-\\") 'split-window-right)
(global-set-key (kbd "M-|") 'reset-splits)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C->") 'next-error)
(global-set-key (kbd "C-<") 'previous-error)
(global-set-key (kbd "M-i") 'clang-format)
(global-set-key (kbd "M-I") 'clang-format-buffer)
(global-set-key (kbd "M-M") 'add-line-above)
(global-set-key (kbd "C-S-m") 'add-line-below)

;; Unset arrow keys and undo key so that I stop using them by accident
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "C-z"))

;; map M-h [mark-paragraph] to M-backslash
(global-set-key (kbd "M-\\") 'mark-paragraph)

;; Rebind/add bindings to a couple ctrl-x items - ones that seem like they should be there from the start
(define-key ctl-x-map (kbd "C-S-f") 'find-file-other-window)
(define-key ctl-x-map (kbd "C-b") 'helm-mini)
(define-key ctl-x-map (kbd "b") 'list-buffers)
(define-key ctl-x-map (kbd "f") 'projectile-find-file)
(define-key ctl-x-map (kbd "F") 'projectile-find-file-other-window)

(global-set-key (kbd "C-c C-f") 'helm-imenu)
(global-set-key (kbd "C-c C-m") 'open-scratch-buffer)
(global-set-key (kbd "C-c C-n") 'open-shared-notes)
(global-set-key (kbd "C-c C-,") 'open-settings)
(global-set-key (kbd "C-c C-.") 'projectile-edit-dir-locals)
(global-set-key (kbd "C-c C-p") 'projectile-switch-project)
(global-set-key (kbd "C-c C-t") 'shell)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)

;; Multi cursor select
(global-set-key (kbd "M-S") 'mc/mark-next-like-this)
(global-set-key (kbd "M-R") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<next>") 'mc/mark-next-lines)
(global-set-key (kbd "C-<prior>") 'mc/mark-previous-lines)

(global-set-key (kbd "M-+") 'windmove-swap-states-right)
(global-set-key (kbd "M-_") 'windmove-swap-states-left)

(defun proj-comp()
  (interactive)
  (call-interactively #'projectile-compile-project)
  (switch-to-prev-buffer)
  (switch-to-buffer-other-window "*compilation*"))

(defun proj-conf()
  (interactive)
  (call-interactively #'projectile-configure-project)
  (switch-to-prev-buffer)
  (switch-to-buffer-other-window "*compilation*"))

(defun proj-run()
  (interactive)
  (call-interactively #'projectile-run-project)
  (switch-to-prev-buffer)
  (switch-to-buffer-other-window "*compilation*"))

(global-set-key (kbd "<f4>") 'open-with-designer)

(global-set-key (kbd "<f5>") 'proj-comp)
(global-set-key (kbd "<f6>") 'proj-conf)
(global-set-key (kbd "<f7>") 'proj-run)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

(define-key lsp-mode-map (kbd "M-;") 'lsp-ui-peek-find-references)
(define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
(define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)
(define-key lsp-mode-map (kbd "C-:") 'lsp-rename)
(define-key lsp-mode-map (kbd "C-S-s") 'helm-projectile-grep)
(define-key lsp-mode-map (kbd "M-h") 'helm-projectile-find-other-file)
(define-key lsp-mode-map (kbd "M-H") 'open-header-other-window)

;; Add debug actions to keymap
(define-key lsp-mode-map (kbd "<f9>") 'dap-breakpoint-toggle)
(define-key lsp-mode-map (kbd "M-<f9>") 'dap-breakpoint-condition)
(define-key lsp-mode-map (kbd "M-S-<f9>") 'dap-breakpoint-hit-condition)
(define-key lsp-mode-map (kbd "C-<f9>") 'dap-ui-breakpoints-browse)
(define-key lsp-mode-map (kbd "C-S-<f9>") 'dap-ui-breakpoints-list)
(define-key lsp-mode-map (kbd "C-M-S-<f9>") 'dap-breakpoint-delete-all)
(define-key lsp-mode-map (kbd "C-<f10>") 'start-debug)
(define-key lsp-mode-map (kbd "C-<f10>") 'dap-continue)
(define-key lsp-mode-map (kbd "C-S-<f10>") 'restart-debug)
(define-key lsp-mode-map (kbd "<f10>") 'dap-next)
(define-key lsp-mode-map (kbd "<f11>") 'dap-step-in)
(define-key lsp-mode-map (kbd "<f12>") 'dap-step-out)
(define-key lsp-mode-map (kbd "M-<f10>") 'end-debug)

(setq projectile-project-search-path '("~/projects"))
(projectile-discover-projects-in-search-path)
(setq projectile-switch-project-action #'projectile-dired)

(require 'cc-mode)
(electric-pair-mode)

;; Move text and selected regions up and down
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))
 
(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "M-[") 'scroll-up-line)
(global-set-key (kbd "M-]") 'scroll-down-line)
(global-set-key (kbd "M-{") 'move-text-up)
(global-set-key (kbd "M-}") 'move-text-down)

(set-cursor-color "#00ff00")

(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup/" t)))


(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(reset-splits)

(setq command_default_list (list "debug" "release"))
(setq command_hist_list (list))

(defun get-config-from-user ()
  (interactive)
  (read-string "Enter config name: " nil 'command_hist_list command_default_list))

(defun get-compile-command ()
  (interactive)
  (format "./.emacs/build.sh %s" (get-config-from-user)))

(defun get-config-command ()
  (interactive)
  (format "./.emacs/configure.sh %s" (get-config-from-user)))

(defun get-run-command ()
  (interactive)
  (format "./.emacs/run.sh %s" (get-config-from-user)))


(projectile-register-project-type 'cmake '("CMakeList.txt")
                                  :project-file "CMakeLists.txt"
                                  :compile 'get-compile-command
				  :configure 'get-config-command
				  :run 'get-run-command
                                  :src-dir "src/"
				  )

(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "Ubuntu Mono" :height 132))

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Menlo" :height 112))

;; Show elapsed compile time when compilation finishes
(make-variable-buffer-local 'my-compilation-start-time)

(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(defun my-compilation-start-hook (proc) 
  (setq my-compilation-start-time (current-time)))

(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
         (msg (format "Elapsed: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))

(defun add-func-calls-to-name-face()
  (font-lock-add-keywords
   nil
   '(("\\<\\(\\sw+\\)?(" 1 'font-lock-function-name-face))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state -1)
  ;; Remove the C-c C-p binding in c++-mode-map
  (define-key c++-mode-map (kbd "C-c C-p") nil)
  (add-func-calls-to-name-face))

(add-hook 'c-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(c-add-style "MyStyle"
	     '("my-style"
	       (c-basic-offset . 4)	; Guessed value
	       (c-offsets-alist
		(block-close . 0)	; Guessed value
		(class-close . 0)	; Guessed value
		(class-open . 0)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(defun-close . 0)	; Guessed value
		(defun-open . 0)	; Guessed value
		(else-clause . 0)	; Guessed value
		(inclass . +)		; Guessed value
		(statement . 0)		    ; Guessed value
		(statement-block-intro . +) ; Guessed value
		(substatement . +)	    ; Guessed value
		(substatement-open . 0)	; Guessed value
		(topmost-intro . 0)	; Guessed value
		(access-label . -)
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont c-lineup-gcc-asm-reg 0)
		(arglist-cont-nonempty . c-lineup-arglist)
		(arglist-intro . +)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
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
		(inline-close . 0)
		(inline-open . +)
		(inmodule . +)
		(innamespace . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . +)
		(label . 0)
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
		(statement-cont . +)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement-label . 0)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont . c-lineup-topmost-intro-cont))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   '("78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "e09401ab2c457e2e4d8b800e1c546dbc8339dc33b2877836ba5d9b6294ae6e55" "0ed28b0694dd2c7a2407598e63650a8562b9e833a1a136ee74790a74d3776d3b" default))
 '(dap-mode t nil (dap-mode))
 '(ispell-dictionary nil)
 '(menu-bar-mode nil)
 '(modern-c++-preprocessors
   '("__STDCPP_STRICT_POINTER_SAFETY__" "#pragma STDC CX_LIMITED_RANGE" "__STDC_MB_MIGHT_NEQ_WC__" "#pragma STDC FP_CONTRACT" "#pragma STDC FENV_ACCESS" "__has_cpp_attribute" "__STDC_ISO_10646__" "__STDCPP_THREADS__" "__STDC_VERSION__" "__STDC_HOSTED__" "__has_include" "#pragma pack" "#pragma once" "__cplusplus" "__VA_ARGS__" "__VA_OPT__" "__TIME__" "__STDC__" "__LINE__" "__FILE__" "__DATE__" "#include" "#defined" "_Pragma" "#pragma" "#ifndef" "#define" "#undef" "#ifdef" "#error" "#endif" "#line" "#else" "#elif" "#if" "intern" "ilog" "dlog" "wlog" "elog" "tlog"))
 '(ns-command-modifier 'control)
 '(ns-control-modifier 'meta)
 '(package-selected-packages
   '(iedit ace-window treemacs-projectile modern-cpp-font-lock clang-format lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode multiple-cursors))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1e1e1e" :foreground "wheat1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 132 :width normal :foundry "nil" :family "Ubuntu Mono"))))
 '(button ((t (:inherit link :foreground "#ce9178"))))
 '(error ((t (:foreground "light pink" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "#4ec9b0"))))
 '(font-lock-function-name-face ((t (:foreground "orange" :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#569cd6"))))
 '(font-lock-type-face ((t (:foreground "#4ec9b0" :weight normal))))
 '(font-lock-variable-name-face ((t (:foreground "#9cdcfe" :weight normal))))
 '(highlight ((t (:foreground "orchid" :underline t))))
 '(lsp-face-highlight-read ((t (:foreground "#9cdcfe" :underline t))))
 '(success ((t (:foreground "lime green" :weight bold))))
 '(warning ((t (:foreground "orange" :weight bold)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; These need to go last because the above changes default font - this will change for mac and linux
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Menlo" :height 112))
