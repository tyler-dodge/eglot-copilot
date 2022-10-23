;;; -*- lexical-binding: t -*-

(require 'rx)
(require 'ht)
(require 'dash)

(defcustom copilot-node-program "node"
  "Path to node executable used to run the copilot agent.
As of this writing, Node version must be less than 18."
  :type '(file))

(defcustom copilot-node-agent-script "dist/agent.js"
  "Script that runs the copilot agent."
  :type '(file))

(defvar copilot-panel-buffer nil
  "A reference to the buffer that's displaying the copilot panel")

(defvar copilot-panel-solutions--accumulator nil
  "Accumulates the temporary results while generating a panel.
Can be used to get the most up to dat suggestions,
but the availability is intermittent.")

(defvar copilot-panel-solutions--ids (ht)
  "A hashtable of the solution ids already seen.
Used to dedupe the `copilot-panel-solutions--accumulator'.")

(defvar copilot-panel-solutions nil
  "The fully generated list of solutions.")

(defvar copilot-panel-id-to-buffer (ht)
  "Maps the given panel id to a buffer object.")

(defvar copilot-panel--idle-timer nil
  "Timer that handles refreshing the copilot panel.")

(defvar-local copilot--shadow-buffer nil
  "Reference to the shadow buffer for the current buffer.")

(defvar copilot--result-token-cache (ht)
  "Cache of tokens from copilot for use with autocomplete.")

(defconst copilot--token-delimiter-regexp (rx (or " " "." "(" ")" "{" "}" "[" "]" ?\" ))
  "Regexp for spliting copilot results into tokens for search.")

(defvar-local copilot-panel--target-buffer nil
  "The target buffer for the current copilot-panel")

;;;###autoload
(defun copilot-setup ()
  "Setup the copilot integration to ensure the best user experience."
  (add-to-list 'eglot-server-programs '(copilot-shadow-mode . copilot-shadow-mode-server-command))
  (push #'copilot-sort-results-company-transformer company-transformers)
  (push 'copilot-shadow-kill-buffer-hook kill-buffer-hook)
  (copilot-install-eglot-extension)
  (copilot-start-panel-refresh-timer))

;;;###autoload
(defun counsel-copilot ()
  "Runs ivy to preview results from copilot."
  (interactive)
  (ivy-read
   "Copilot:"
   (->> copilot-panel-solutions
        (--sort (> (car  it) (car other)))
        (-map #'cdr)
        (--map (apply #'propertize
                      (s-join "\n" (-take 3 (s-lines it)))
                      (text-properties-at 0 it))))
   :require-match t
   :action #'counsel-copilot--preview-select-action
   :caller 'counsel-copilot))

;;;###autoload
(defun copilot-sign-in ()
  "Shows the sign-in form for authenticating with copilot."
  (interactive)
  (jsonrpc-async-request
   (copilot-shadow-eglot-server) :signInInitiate (eglot--TextDocumentPositionParams)
   :success-fn (lambda (response &rest arg)
                 (message "User Code: %s" (plist-get response :userCode))
                 (browse-url (plist-get response :verificationUri)))
   :error-fn (lambda (&rest arg) (message "%S" arg))))

;;;###autoload
(defun copilot-check-status ()
  "Checks the login status for copilot authentication."
  (interactive)
  (jsonrpc-async-request (copilot-shadow-eglot-server) :checkstatus
                         (eglot--textdocumentpositionparams)
                         :success-fn (lambda (response &rest arg)
                                       (message "%s" response))
                         :error-fn (lambda (&rest arg) (message "%s" arg))))

;;;###autoload
(defun company-copilot (command &optional arg &rest ignored)
  "Company backend for completing using copilot suggestions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-copilot))
    (match t)
    (prefix (copilot--company-prefix))
    (no-cache t)
    (sorted t)
    (post-completion
     (let* ((inhibit-modification-hooks t))
       (delete-char (- (length arg)))
       (atomic-change-group
         (let* ((range (get-text-property 0 :range arg))
                (inhibit-modification-hooks t)
                (start-pt (point))
                (end (copilot--lsp-position-to-point (plist-get range :end)))
                (start (copilot--lsp-position-to-point (plist-get range :start))))
           (goto-char start-pt)
           (save-excursion
             (delete-region
              start-pt
              end)) 
           (goto-char start)
           (let ((start-pt (point)))
             (insert (get-text-property 0 :insertText arg))
             (save-excursion
               (indent-region start-pt (point))))))))
    (candidates
     (let ((server (copilot-shadow-eglot-server))
           (current-pt (point))
           (shadow-buffer (copilot--shadow-buffer (current-buffer))))
       (with-current-buffer shadow-buffer
         (goto-char current-pt))
       (cons :async
             (lambda (callback)
               (with-current-buffer shadow-buffer
                 (eglot--signal-textDocument/didChange)
                 (jsonrpc-async-request
                  (eglot-current-server)
                  :getCompletions
                  (append
                   (list
                    :doc
                    (list :path (buffer-file-name)
                          :position (eglot--pos-to-lsp-position)
                          :uri (eglot--path-to-uri (buffer-file-name)))
                    )
                   (eglot--TextDocumentPositionParams) nil)
                  :success-fn
                  (lambda (response &rest arg)
                    (funcall
                     callback
                     (->> (append (plist-get response :completions) nil)
                          (--map
                           (propertize
                            (s-truncate 100 (plist-get it :displayText))
                            :range (plist-get it :range)
                            :insertText (plist-get it :text))))))
                  :error-fn (lambda (&rest arg) (-message arg))))))))))

;;;###autoload
(defun copilot-panel-refresh ()
  "Refresh the copilot suggestions buffer for the current buffer."
  (interactive)
  (when (buffer-file-name)
    (unless (and copilot-panel-buffer (buffer-live-p copilot-panel-buffer)) (setq copilot-panel-buffer (generate-new-buffer "*copilot*")))
    (let ((target-buffer (current-buffer)))
      (with-current-buffer copilot-panel-buffer
        (setq copilot-panel--target-buffer target-buffer)))
    (setq copilot-panel-solutions--accumulator nil)
    (setq copilot-panel-solutions--ids (ht))
    (-some--> (copilot-shadow-eglot-server)
      (jsonrpc-async-request it :getPanelCompletions
                             (append
                              (list
                               :panelId "copilot:///1"
                               :doc
                               (list :path (buffer-file-name)
                                     :position (eglot--pos-to-lsp-position)
                                     :uri (eglot--path-to-uri (buffer-file-name)))
                               )
                              (eglot--TextDocumentPositionParams) nil)
                             :deferred :getPanelCompletions))))


(defun copilot--first-token (text)
  (or (-some--> (string-match copilot--token-delimiter-regexp text) (substring text 0 it))
      text))

;;;###autoload
(defun copilot-sort-results-company-transformer (results)
  "Company transformer for sorting results based on the output from copilot.
See `company-transformers'."
  (when results
    (->> results
         (--map (cons (ht-get copilot--result-token-cache (copilot--first-token it)) it))
         (--sort
          (let ((it-in-cache (car it))
                (other-in-cache (car other)))
            (cond
             ((and it-in-cache other-in-cache) nil)
             (it-in-cache t)
             (other-in-cache nil)
             (t nil))))
         (--map (if (car it)
                    (cdr it)
                  (cdr it))))))

(define-derived-mode copilot-shadow-mode fundamental-mode "copilot-shadow"
  "Mode used by shadow buffers for copilot.")

(defun copilot-shadow-eglot-server ()
  "Return the eglot server used by the shadow buffer for the current buffer."
  (with-current-buffer (copilot--shadow-buffer (current-buffer))
    (eglot-current-server)))

(defun copilot-shadow-kill ()
  "Kills the shadow buffer for the current buffer."
  (interactive)
  (when copilot--shadow-buffer
    (kill-buffer copilot--shadow-buffer)
    (setq copilot--shadow-buffer nil)))

(defun copilot-shadow-clean ()
  "Kills all copilot buffers and corresponding eglot processes"
  (interactive)
  (let ((processes nil))
    (->> (buffer-list)
         (--filter (eq (buffer-local-value 'major-mode it) 'copilot-shadow-mode))
         (--map (progn
                  (-some--> (get-buffer-process it) (setq processes (cons it processes)))
                  (kill-buffer it))))
    (->> processes (--filter (eq (process-status it) 'run)) (--map (kill-process it)))))

(defun copilot--company-prefix ()
  "Return the prefix at point for the copilot company-backend."
  (and (-some--> (progn (save-excursion (re-search-backward copilot--token-delimiter-regexp nil t)
                                        (1+ (point))))
         (if (< it (point)) (cons (buffer-substring it (max it (point))) t)
           (cons "" t)))))

(defun copilot-panel--generate-buffer ()
  "Generates the copilot panel solutions buffer based on a list of solutions."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (cl-loop for completion in (--sort (> (car it) (car other)) copilot-panel-solutions)
             do
             (insert (cdr completion))
             (insert "\n---\n"))))

(defun copilot--shadow-buffer (current-buffer)
  "Return the shadow buffer for the current buffer. Creates it if it does not exist."
  (save-mark-and-excursion
    (with-current-buffer current-buffer
      (setq-local before-change-functions (-uniq (cons #'copilot--before-change-hook before-change-functions)))
      (setq-local after-change-functions (-uniq (cons #'copilot--after-change-hook after-change-functions)))
      (if (and copilot--shadow-buffer (buffer-live-p copilot--shadow-buffer)
               (with-current-buffer copilot--shadow-buffer
                 (copilot-shadow--eglot-ensure (-some--> (eglot-current-server) (eglot--language-id it)))))
          copilot--shadow-buffer
        (let (
              (language-id (-some--> (eglot-current-server) (eglot--language-id it)))
              (copied
               (progn
                 (widen)
                 (buffer-string)))
             (file-name (buffer-file-name)))
          (unless (f-exists-p copilot-node-agent-script)
            (user-error (concat "`copilot-node-agent-script': " (format "%S" copilot-node-agent-script) " Does Not Exist.")))
          (unless (executable-find copilot-node-program)
            (user-error (concat "`copilot-node-program': " (format "%S" copilot-node-program) " Not Found in `exec-path'.")))
          (let ((node-version
                 (-some-->
                     (shell-command-to-string (s-join " " (list copilot-node-program "--version")))
                     (string-trim it)
                     (s-split-up-to (rx ".") it 1)
                     (car it)
                     (substring it 1)
                     (string-to-number it))))
            (when (> node-version 18) (user-error "Node version should be less than v18"))
            (setq copilot--shadow-buffer
                  (with-current-buffer
                      (generate-new-buffer (concat " *copilot shadow " (buffer-name) "*"))
                    (insert copied)
                    (copilot-shadow-mode)
                    (set-buffer-modified-p nil)
                    (setq-local buffer-file-name file-name)
                    (copilot-shadow--eglot-ensure language-id)
                    (eglot--maybe-activate-editing-mode)
                    (set-buffer-modified-p nil)
                    (current-buffer)))))))))

(defun copilot-shadow--eglot-ensure (language-id)
  "Ensures that eglot is enabled. If it isn't, then start it for `copilot-shadow-mode'."
  (or (eglot-managed-p)
      (and (eglot-current-server) (prog1 t (eglot--maybe-activate-editing-mode)))
      (eglot
       #'copilot-shadow-mode
       (cons 'projectile
             (project-root (project-current)))
       #'eglot-lsp-server
       (copilot-shadow-mode-server-command)
       (or language-id "copilot") t)))

(defun copilot--before-change-hook (beg end)
  "before-change-functions hook for forwarding `eglot--before-change' to the shadow buffer."
  (let ((start-buffer (current-buffer))
        (copied (buffer-substring beg end)))
    (with-current-buffer (copilot--shadow-buffer start-buffer)
      (ignore-errors (eglot--before-change beg end)))))

(defun copilot--after-change-hook (beg end length)
  "after-change-functions hook for forwarding `eglot--after-change' to the shadow buffer."
  (let ((start-buffer (current-buffer))
        (copied (buffer-substring beg end)))
    (with-current-buffer (copilot--shadow-buffer start-buffer)
      (let ((pt (point)))
        (save-excursion
          (let ((inhibit-modification-hooks t)
                (inhibit-point-motion-hooks t)
                (inhibit-read-only t))
            (goto-char beg)
            (delete-char length)
            (insert copied)
            (ignore-errors (eglot--after-change beg end length))
            (set-buffer-modified-p nil)))
        (goto-char pt)))))

(defun copilot-shadow-kill-buffer-hook ()
  "Handles killing the copilot shadow buffer when the owner buffer is killed."
  (when copilot--shadow-buffer
    (kill-buffer copilot--shadow-buffer)))

(defun copilot-shadow-mode-server-command (&rest arg)
  "Return the command to run copilot for eglot.
Designed to worked with `eglot-server-programs'."
  (list copilot-node-program
        copilot-node-agent-script))

(defun copilot-start-panel-refresh-timer ()
  "Starts the timer responsible for updating the copilot panel."
  (when copilot-panel--idle-timer (cancel-timer copilot-panel--idle-timer))
  (setq copilot-panel--idle-timer
        (run-with-idle-timer
         0.5 t
         (lambda (&rest arg)
           (copilot-panel-refresh)))))

(defun copilot-install-eglot-extension ()
  "Defines the method overrides to work with eglot for the
additional copilot methods like PanelSolution."
  (cl-defmethod eglot-handle-notification
    (server (_method (eql PanelSolution)) &rest params)
    "Handle server request PanelSolution for copilot panel."
    (unless (ht-get copilot-panel-solutions--ids (plist-get params :solutionId))
      (ht-set copilot-panel-solutions--ids (plist-get params :solutionId) t)
      (push
       (cons (plist-get params :score)
             (propertize (concat (format "%.2f: " (plist-get params :score)) (plist-get params :displayText))
                         :id (plist-get params :solutionId)
                         :range (plist-get params :range)
                         :insertText (plist-get params :completionText)))
       copilot-panel-solutions--accumulator)
      (copilot--update-result-cache)))

  (cl-defmethod eglot-handle-notification
    (server (_method (eql PanelSolutionsDone)) &rest params)
    "Handle server request PanelSolution for copilot panel."
    (when copilot-panel-solutions--accumulator
      (setq copilot-panel-solutions (append copilot-panel-solutions--accumulator nil))
      (setq copilot-panel-solutions--accumulator nil)
      )
    (copilot--update-result-cache)
    (when copilot-panel-buffer
      (with-current-buffer copilot-panel-buffer
        (copilot-panel--generate-buffer)))))

(defun copilot-panel-select-at-point ()
  "Selects the entry at point in the copilot panel, and performs
the replacement in the target buffer for the panel."
  (interactive)
  (let ((range (get-text-property (point) :range))
        (insert-text (get-text-property (point) :insertText)))
    (with-current-buffer copilot-panel--target-buffer
      (goto-char (copilot--lsp-position-to-point (plist-get range :start)))
      (delete-region (point)
                     (copilot--lsp-position-to-point (plist-get range :end)))
      (insert insert-text))))

(defun copilot--lsp-position-to-point (lsp-pt)
  "Converts an lsp position to a buffer point."
  (ignore-errors
    (or
     (eglot--lsp-position-to-point
      lsp-pt)
     (point-max))))

(defun copilot--update-result-cache ()
  "Refresh the `copilot--result-token-cache' based on
the current `copilot-panel-solutions--accumulator' and `copilot-panel-solutions'."
  (setq copilot--result-token-cache (ht))
  (setq copilot-panel-solutions--accumulator (-uniq copilot-panel-solutions--accumulator))
  (cl-loop for solution in (append copilot-panel-solutions copilot-panel-solutions--accumulator nil)
           do
           (cl-loop 
            for word in (when solution (s-split copilot--token-delimiter-regexp (get-text-property 0 :insertText (cdr solution)) t))
            do
            (ht-set copilot--result-token-cache (s-trim word) t))))

(defun copilot--replace-text (selection &optional highlight-replacement)
  "Replaces the text referenced by SELECTION. If HIGHLIGHT-REPLACEMENT is non-nil,
add a face to the replacement."
  (let* ((range (get-text-property 0 :range selection))
         (inhibit-modification-hooks t)
        (insert-text (get-text-property 0 :insertText selection)))
    (when insert-text
      (let* ((start-pt (save-excursion (goto-char (copilot--lsp-position-to-point (plist-get range :start))) (forward-line 0) (point)))
             (start-window-pt (save-excursion (goto-char start-pt) (ignore-errors (forward-line -3)) (point)))
             (end-pt (save-excursion (goto-char (copilot--lsp-position-to-point (plist-get range :end))) (end-of-line) (1+ (point)))))
        (goto-char start-pt)
        (atomic-change-group
          (delete-region start-pt end-pt)
          (insert (if highlight-replacement (propertize insert-text 'face '(:background "#FAFAFA"))
                    insert-text)))
        (when highlight-replacement
          (-some-->
              (display-buffer (current-buffer))
            (progn
              (set-window-point it start-pt)
              (set-window-start it start-window-pt))))))))

(defun counsel-copilot--preview-select-action (selection)
  "Counsel action for selecting a copilot preview."
  (let* ((buffer-name "*copilot-preview*")
        (buffer-string
         (with-current-buffer copilot-panel-buffer
           (with-current-buffer copilot-panel--target-buffer
             (buffer-string)))))
    (with-current-buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name))
      (erase-buffer)
      (insert buffer-string)
      (copilot--replace-text selection t))))

(provide 'copilot)
