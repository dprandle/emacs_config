(defun my-reset-splits ()
  "Reset windows to default state"
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun open-settings ()
  (interactive)
  (find-file "~/.emacs"))

(defun open-settings-funcs ()
  (interactive)
  (find-file "~/.emacs.d/lisp/custom_funcs.el"))

(defun my-helm-mini-other-window()
  (interactive)
  (other-window 1)
  (call-interactively #'helm-mini))

(defun open-shared-notes()
  (interactive)
  (find-file "~/.emacs.d/notes.org"))

(defun open-header-other-window()
  "Open header in other window"
  (interactive)
  (projectile-find-other-file-other-window)
  (other-window 1))

(defun my-helm-projectile-find-file-other-window()
  "Open file in other window"
  (interactive)
  (other-window)
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
  (shell-command (format "nohup designer %s </dev/null &>/dev/null &" (buffer-file-name))))

(defun launch-qt-creator()
  (interactive)
  (shell-command (format "nohup bash -c 'wmctrl -s 2 && nohup qtcreator %s' </dev/null &>/dev/null &" (buffer-file-name)))
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
    (shell-command (format "nohup designer %s </dev/null &>/dev/null &" (buffer-file-name)))
    (kill-buffer "*Async Shell Command*")
    (kill-buffer (get-file-buffer buffer-file-name))))

;; Make the scratch buffer just switch to whatever the previous buffer was instead of killing
(defun my-unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (switch-to-prev-buffer)
	      nil)
	  t))

(defun find-definition-other-window()
  "Follow definition under cursor in other window"
  (interactive)
  (setq fname (buffer-file-name))
  (other-window 1)
  (find-file fname)
  (xref-find-definitions))

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

;; Dont whine if there is a terminal open
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(defun my-ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; Do this at a later time
;; (defun send-current-point-to-qt-assistant
;;     (let ((cur-process (get-process "assistant")))      
;;       (if (cur-process) (progn)
;; 	(progn
;; 	  (make-process
;; 	   :name "assistant"
;; 	   :buffer nil
;; 	   :command '("assistant" "-enableRemoteControl"))
	  
