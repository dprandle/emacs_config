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

;; Always follow symlinks and edit the source file without asking
(setq vc-follow-symlinks t)

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

(defun helm-mini-other-window()
  (interactive)
  (other-window 1)
  (call-interactively #'helm-mini))

(defun open-shared-notes()
  (interactive)
  (find-file "~/.emacs.d/notes.org"))

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

(defun launch-qt-creator()
  (interactive)
  (shell-command (format "wmctrl -s 2 && nohup qtcreator %s </dev/null &>/dev/null &" (buffer-file-name)))
  (kill-buffer "*Async Shell Command*"))

(defun open-scratch-buffer()
  "Create or get scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (lisp-interaction-mode))

(defun kill-other-buffer()
  "Kill the buffer in the other split"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(setq currently-debugging nil)
(defun start-or-continue-debug()
  "Restart the debug session"
  (interactive)
  (if (equal currently-debugging nil)
      (progn 
	(dap-delete-all-sessions)
	(call-interactively #'dap-debug)
	(setq currently-debugging t))
    (call-interactively #'dap-continue)))

(defun end-debug()
  "End the debug session and restore the windows"
  (interactive)
  (dap-delete-all-sessions)
  (setq currently-debugging nil)
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
(global-set-key (kbd "M-K") 'kill-other-buffer)
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
(define-key ctl-x-map (kbd "C-S-b") 'helm-mini-other-window)
(define-key ctl-x-map (kbd "b") 'list-buffers)
(define-key ctl-x-map (kbd "f") 'projectile-find-file)
(define-key ctl-x-map (kbd "F") 'projectile-find-file-other-window)

(global-set-key (kbd "C-c C-f") 'helm-imenu)
(global-set-key (kbd "C-c C-m") 'open-scratch-buffer)
(global-set-key (kbd "C-c C-n") 'open-shared-notes)
(global-set-key (kbd "C-c C-,") 'open-settings)
(global-set-key (kbd "C-c C-.") 'projectile-edit-dir-locals)
(global-set-key (kbd "C-c C-p") 'projectile-switch-project)
(global-set-key (kbd "C-c C-t") 'eshell)
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
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))

(defun proj-conf()
  (interactive)
  (call-interactively #'projectile-configure-project)
  (switch-to-prev-buffer)
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))

(defun proj-run()
  (interactive)
  (call-interactively #'projectile-run-project)
  (switch-to-prev-buffer)
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))

(global-set-key (kbd "<f4>") 'open-with-designer)

(global-set-key (kbd "<f5>") 'proj-comp)
(global-set-key (kbd "<f6>") 'proj-conf)
(global-set-key (kbd "<f7>") 'proj-run)
(global-set-key (kbd "C-<f7>") 'launch-qt-creator)
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
(define-key lsp-mode-map (kbd "C-<f10>") 'start-or-continue-debug)
(define-key lsp-mode-map (kbd "C-S-<f10>") 'end-debug)
(define-key lsp-mode-map (kbd "<f10>") 'dap-next)
(define-key lsp-mode-map (kbd "<f11>") 'dap-step-in)
(define-key lsp-mode-map (kbd "<f12>") 'dap-step-out)

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

;;(lsp-semantic-tokens-mode)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-semantic-tokens-enable t)

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state -1)
  ;; Remove the C-c C-* bindings in c++-mode-map that aren't needed
  (define-key c++-mode-map (kbd "C-c C-n") nil) ;; go forward in preprocessor conditional
  (define-key c++-mode-map (kbd "C-c C-l") nil) ;; toggle electric indentation mode
  (define-key c++-mode-map (kbd "C-c C-k") nil) ;; toggle comment type // or /**/
  (define-key c++-mode-map (kbd "C-c C-p") nil)) ;; don't remember

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
   '("443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "ce4234c32262924c1d2f43e6b61312634938777071f1129c7cde3ebd4a3028da" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "e09401ab2c457e2e4d8b800e1c546dbc8339dc33b2877836ba5d9b6294ae6e55" "0ed28b0694dd2c7a2407598e63650a8562b9e833a1a136ee74790a74d3776d3b" default))
 '(dap-mode t nil (dap-mode))
 '(ispell-dictionary nil)
 '(menu-bar-mode nil)
 '(modern-c++-preprocessors
   '("__STDCPP_STRICT_POINTER_SAFETY__" "#pragma STDC CX_LIMITED_RANGE" "__STDC_MB_MIGHT_NEQ_WC__" "#pragma STDC FP_CONTRACT" "#pragma STDC FENV_ACCESS" "__has_cpp_attribute" "__STDC_ISO_10646__" "__STDCPP_THREADS__" "__STDC_VERSION__" "__STDC_HOSTED__" "__has_include" "#pragma pack" "#pragma once" "__cplusplus" "__VA_ARGS__" "__VA_OPT__" "__TIME__" "__STDC__" "__LINE__" "__FILE__" "__DATE__" "#include" "#defined" "_Pragma" "#pragma" "#ifndef" "#define" "#undef" "#ifdef" "#error" "#endif" "#line" "#else" "#elif" "#if" "intern" "ilog" "dlog" "wlog" "elog" "tlog"))
 '(ns-command-modifier 'control)
 '(ns-control-modifier 'meta)
 '(package-selected-packages
   '(spacemacs-theme doom-themes zenburn-theme iedit ace-window treemacs-projectile modern-cpp-font-lock clang-format lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode multiple-cursors))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1e1e1e" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 132 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(button ((t (:inherit nil :underline t))))
 '(error ((t (:foreground "light pink" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "#a6acd6"))))
 '(font-lock-function-name-face ((t (:foreground "salmon" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#569cd6" :weight normal))))
 '(font-lock-negation-char-face ((t (:foreground "#9cdcfe"))))
 '(font-lock-type-face ((t (:foreground "#4ec9b0"))))
 '(font-lock-variable-name-face ((t (:foreground "wheat1"))))
 '(highlight ((t (:foreground "orchid" :underline t))))
 '(lsp-face-highlight-read ((t (:foreground "#9cdcfe" :underline t))))
 '(lsp-face-semhl-default-library ((t (:inherit nil))))
 '(lsp-face-semhl-interface ((t nil)))
 '(lsp-face-semhl-method ((t (:inherit lsp-face-semhl-function :weight normal))))
 '(lsp-face-semhl-parameter ((t (:inherit font-lock-variable-name-face :foreground "#9cdcfe"))))
 '(success ((t (:foreground "lime green" :weight bold))))
 '(warning ((t (:foreground "orange" :weight bold)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; These need to go last because the above changes default font - this will change for mac and linux
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Menlo" :height 112))
