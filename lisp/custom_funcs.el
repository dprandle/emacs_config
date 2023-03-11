(defun my-reset-splits ()
  "Reset windows to default state"
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun open-settings ()
  (interactive)
  (find-file "~/.emacs"))

;; overriding image.el function image-type-available-p until fix is out in emacs 29 for mac 
(if (eq system-type 'darwin)
    (defun image-type-available-p (type)
      "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
      (if (eq 'svg type)
          nil
        (and (fboundp 'init-image-library)
             (init-image-library type)))))

(defun open-settings-funcs ()
  (interactive)
  (find-file "~/.emacs.d/lisp/custom_funcs.el"))

(defun my-kill-line ()
  (interactive)
  (let ((cur-col (current-column)))
    (kill-whole-line)
    (move-to-column cur-col)))

(defun copy-line ()
  (interactive)
  (let ((cur-point (point)))
    (previous-line)
    (move-end-of-line nil)
    (set-mark-command nil)
    (next-line)
    (move-end-of-line nil)
    (call-interactively 'kill-ring-save)
    (message "Line Copied")
    (goto-char cur-point)))

(defun new-window-func-split-vert (func)
  (split-window-below -25)
  (other-window 1)
  (funcall func))

(defun my-helm-mini-other-window()
  (interactive)
  (other-window 1)
  (call-interactively #'helm-mini))

(defun c-ptr-insert ()
  "Type the ptr symbol"
  (interactive)
  (setq unread-command-events (listify-key-sequence "->")))

(defun open-global-notes()
  (interactive)
  (find-file "~/org/notes.org"))

(defun open-local-notes()
  (interactive)
  (find-file "~/org-local/notes.org"))

(defun open-project-notes()
  (interactive)
  (let* ((proj-root (projectile-project-root))
         (proj-name (projectile-default-project-name proj-root))
         (sym-fname (format "%s.emacs/notes.org" proj-root proj-name))
         (src-fname (expand-file-name (format "~/org/%s.org" proj-name))))
    (when (not (file-exists-p src-fname))
      (message "Creating new project notes file at %s" src-fname)
      (make-empty-file src-fname))
    (when (not (file-exists-p sym-fname))
      (message "Creating symbolic link from %s to %s for project notes" sym-fname src-fname)
      (make-symbolic-link src-fname sym-fname t))
    (find-file sym-fname)))

(defun create-cmake-project(proj_name)
  (interactive "sEnter project name: ")
  (copy-directory "~/.emacs.d/project_templates/cmake_unix" (concat "~/projects/" proj_name))
  (find-file (concat "~/projects/" proj_name "/CMakeLists.txt"))
  (while (re-search-forward "PROJ_NAME" nil t)
    (replace-match proj_name t)))

(defun my-find-file-other-window ()
  (interactive)
  (call-interactively 'helm-find-files)
  (move-buffer-other-window))

(defun open-header-other-window()
  "Open header in other window"
  (interactive)
  (helm-projectile-find-other-file)
  (move-buffer-other-window)
  (other-window 1))

(defun move-buffer-other-window()
  "Open header in other window"
  (interactive)
  (let ((cur-buf (window-buffer)))
    (previous-buffer)
    (other-window 1)
    (switch-to-buffer cur-buf)))

(defun my-helm-projectile-find-file-other-window()
  "Open file in other window"
  (interactive)
  (other-window 1)
  (helm-projectile-find-file))

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
  (let ((ui-fname (concat (projectile-project-root) "ui/" (file-name-base (buffer-file-name)) ".ui")))
    (message "Opening ui file %s in qt designer" ui-fname)
      (if (eq system-type 'darwin)
          (shell-command (format "open %s -a $HOME/Qt/%s/macos/bin/Designer.app/Contents/MacOS/Designer" ui-fname qt-version-in-use))
        (progn
          (async-shell-command (format "nohup designer %s &> /dev/null &" ui-fname)) ;; &> /dev/null redirects stdout and errout to null
          (kill-buffer "*Async Shell Command*")))))

(defun launch-qt-creator()
  (interactive)
  (message "Opening Qt creator for debug")
  (if (eq system-type 'darwin)
      (shell-command (format "open %s -a \"$HOME/Qt/Qt Creator.app/Contents/MacOS/Qt Creator\"" (buffer-file-name)))
    (progn 
      (shell-command (format "nohup bash -c 'wmctrl -s 2 && nohup qtcreator %s' </dev/null &>/dev/null &" (buffer-file-name)))
      (kill-buffer "*Async Shell Command*"))))

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

(defun reload-current-buffer()
  "Reload the buffer currently being worked on - this is mostly so clang will reload when it bugs out"
  (interactive)
  (let ((cur-buf-fname buffer-file-name) (cur-point (point)))
    (message "Current fname: %s" cur-buf-fname)
    (kill-this-buffer)
    (find-file cur-buf-fname)
    (set-window-point (selected-window) cur-point)))

(defun ui-file-handler ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.ui\\'" buffer-file-name))
    (switch-to-prev-buffer)
    (async-shell-command (format "nohup designer %s &>/dev/null" (buffer-file-name)))
    (kill-buffer "*Async Shell Command*")
    (kill-buffer (get-file-buffer buffer-file-name))))

;; Make the scratch buffer just switch to whatever the previous buffer was instead of killing
(defun my-unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (switch-to-prev-buffer)
	      nil)
	  t))

(defun my-c++-mode-hook ()
;;  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state -1)
  ;; Remove the C-c C-* bindings in c++-mode-map that aren't needed
  (define-key c++-mode-map (kbd "C-c :") nil)
  (define-key c++-mode-map (kbd "C-c .") nil)
  (define-key c++-mode-map (kbd "C-c C-w") nil)
  (define-key c++-mode-map (kbd "C-c C-u") nil)
  (define-key c++-mode-map (kbd "C-c C-s") nil)
  (define-key c++-mode-map (kbd "C-c C-q") nil)
  (define-key c++-mode-map (kbd "C-c C-o") nil)
  (define-key c++-mode-map (kbd "C-c C-d") nil)
  (define-key c++-mode-map (kbd "C-c C-c") nil)
  (define-key c++-mode-map (kbd "C-c C-b") nil)
  (define-key c++-mode-map (kbd "C-c C-n") nil) ;; go forward in preprocessor conditional
  (define-key c++-mode-map (kbd "C-c C-l") nil) ;; toggle electric indentation mode
  (define-key c++-mode-map (kbd "C-c C-k") nil) ;; toggle comment type // or /**/
  (define-key c++-mode-map (kbd "C-c C-a") nil) ;; toggle auto line break - hate this mode and would never use it
  (define-key c++-mode-map (kbd "C-c C-p") nil)) ;; don't remember

(make-variable-buffer-local 'my-compilation-start-time)
(defun my-compilation-start-hook (proc) 
  (setq my-compilation-start-time (current-time)))

(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
         (msg (format "Elapsed: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))

(setq command_default_list (list "debug" "release"))
(setq command_hist_list (list))

(defun my-cmake-get-config-from-user ()
  (interactive)
  (read-string "Enter config name: " nil 'command_hist_list command_default_list))

(defun my-cmake-get-compile-command ()
  (interactive)
  (format "./.emacs/build.sh %s" (my-cmake-get-config-from-user)))

(defun my-cmake-get-config-command ()
  (interactive)
  (format "./.emacs/configure.sh %s" (my-cmake-get-config-from-user)))

(defun my-cmake-get-run-command ()
  (interactive)
  (format "./.emacs/run.sh %s" (my-cmake-get-config-from-user)))


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

(defun proj-helper (projectile-func)
  (let ((cur-buf-name (buffer-name)))
    (call-interactively projectile-func)
    (message "Compilation called with current buffer set as %s" cur-buf-name)
    (if (equal cur-buf-name "*compilation*")
        (progn
          (message "Already in comp buffer - moving to end")
          (end-of-buffer))
      (progn ;; (else)
        (message "Moving compilation to other window")
        (move-buffer-other-window)
        (end-of-buffer)
        (other-window 1)))))

(defun proj-comp()
  (interactive)
  (proj-helper #'projectile-compile-project))

(defun proj-conf()
  (interactive)
  (proj-helper #'projectile-configure-project))

(defun proj-run()
  (interactive)
  (proj-helper #'projectile-run-project))

;; Dont whine if there is a terminal open
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(defun my-ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun open-symbol-at-point-in-mongo-doc ()
    (interactive)
    (let ((cur-symbol (symbol-at-point)))
      (if cur-symbol
          (progn
            (message "Loading documentation for %s" cur-symbol)
            (other-window 1)
            (w3m-browse-url (concat "http://mongoc.org/libmongoc/current/" (downcase (format "%s" cur-symbol)) ".html")))
        (message "No symbol at point"))))

(defun open-symbol-at-point-in-bson-doc ()
    (interactive)
    (let ((cur-symbol (symbol-at-point)))
      (if cur-symbol
          (progn
            (message "Loading documentation for %s" cur-symbol)
            (other-window 1)
            (w3m-browse-url (concat "http://mongoc.org/libbson/current/" (downcase (format "%s" cur-symbol)) ".html")))
        (message "No symbol at point"))))

;; (defun open-symbol-at-point-in-qt-web-doc ()
;;     (interactive)
;;     (let ((cur-symbol (symbol-at-point)))
;;       (if cur-symbol
;;           (progn
;;             (message "Loading documentation for %s" cur-symbol)
;;             (other-window 1)
;;             (w3m-browse-url (concat "https://doc.qt.io/qt-6/" (downcase (format "%s" cur-symbol)) ".html")))
;;         (message "No symbol at point"))))

(defun open-symbol-at-point-in-qt-web-doc ()
    (interactive)
    (let ((cur-symbol (symbol-at-point)))
      (if cur-symbol
          (progn
            (let* ((cur-symbol-str (downcase (format "\\`%s\\.html\\_>" cur-symbol)))
                   (flist (directory-files-recursively (format "~/Qt/Docs/Qt-%s" qt-version-in-use) cur-symbol-str)))
              (if flist
                  (progn
                    (message "Loading documentation for %s" cur-symbol)
                    (other-window 1)
                    (w3m-browse-url (concat "file://" (expand-file-name (nth 0 flist)))))
                (message "Symbol at point %s not found in Qt docs" cur-symbol))))
        (message "No symbol at point"))))

(defun start-trello-mode-if-needed ()
  (interactive)
   (let ((filename (buffer-file-name (current-buffer))))
     (when (and filename (string= "trello" (file-name-extension filename)))
       (org-trello-mode))))

;; Here are all my custom macros

(fset 'cpp-create-method-def-after
   (kmacro-lambda-form [?\C-a tab ?\M-a ?\C-e ?\C-\M-p 134217802 ?\C-  ?\C-\M-b ?\M-w ?\C-\M-e return ?\C-y ?\C-x ?o ?\M-e ?\M-e ?\M-a ?\C-  ?\C-\M-f ?\M-w ?\C-x ?o ?\C-y ?\C-/ ?\C-a ?\C-y ?  ?\C-e ?\C-x ?o ?\C-f ?\C-  ?\C-e ?\M-w ?\C-a tab ?\C-x ?o ?\C-y backspace return ?\{ return ?\C-\M-e return ?\C-\M-a ?\C-n ?\C-n tab] 0 "%d"))

(fset 'cpp-create-method-def-at-end
      (kmacro-lambda-form [134217800 ?\C-a tab ?\C-  ?\C-e ?\M-w ?\C-\M-r ?^ ?c ?l ?a ?s ?s ?\\ ?| ?s ?t ?r ?u ?c ?t return ?\C-x ?o ?\M-> return ?\C-y backspace return ?\{ return ?\C-\M-a ?\C-x ?o ?\C-\M-f ?\C-f ?\C-  ?\C-\M-f ?\M-w ?\C-x ?o ?\C-\M-f ?\C-f ?\C-y ?: ?: ?\C-  ?\C-e ?\M-w ?\C-x ?o ?\C-s ?\C-y return ?\C-a tab ?\C-x ?o ?\C-a ?\C-n ?\C-n tab] 0 "%d"))

(fset 'cpp-create-func-def-at-end
   (kmacro-lambda-form [?\C-a tab 134217800 ?\C-  ?\C-e ?\M-w ?\C-a ?\C-x ?o ?\M-> return ?\C-y backspace return ?\{ return] 0 "%d"))

(fset 'cpp-create-func-def-after
   (kmacro-lambda-form [?\C-a tab ?\M-a ?\C-e ?\C-\M-p 134217802 ?\C-\M-e return ?\C-x ?o ?\M-e ?\M-e ?\M-a ?\C-  ?\C-e ?\M-w ?\C-a ?\C-x ?o ?\C-y backspace return ?\{ return ?\C-\M-e return ?\C-\M-a ?\C-n ?\C-n tab] 0 "%d"))
