;;; eglot-copilot.el --- Copilot integration with EGLOT and Company -*- lexical-binding: t -*-

;; Author: Tyler Dodge (tyler@tdodge.consulting)
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "28.2") (f "0.20.0") (bash-completion "3.1.0") (projectile "2.6.0-snapshot") (s "1.12.0") (company "0.9.13") (eglot-copilot "0.1") (dash "2.19.1") (ht "2.4"))
;; URL: https://github.com/tyler-dodge/eglot-copilot
;; Git-Repository: git://github.com/tyler-dodge/eglot-copilot.git
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;;;
;;; Commentary:
;; `eglot-copilot-setup' Handles integration with emacs. 
;; <example>
;; (eglot-copilot-setup)
;; </example>
;; `eglot-copilot-node-agent-script' will need to be updated to point to an copilot agent.js.
;; An example can be found at https://github.com/github/copilot.vim/blob/release/copilot/dist/agent.js
;;
;; This package provides a company transformer named `eglot-copilot-sort-results-company-transformer' 
;; which takes the results of the copilot panel, and sorts the company candidates based on their presence in
;; the copilot panel. 
;; It is added to the transformer list by default in `eglot-copilot-setup'
;;; Code:


(require 'rx)
(require 'ht)
(require 'dash)
(require 'eieio)
(require 'eglot)

(defcustom eglot-copilot-node-program "node"
  "Path to node executable used to run the copilot agent.
As of this writing, Node version must be less than 18."
  :type '(file))

(defcustom eglot-copilot-node-agent-script "dist/agent.js"
  "Script that runs the copilot agent."
  :type '(file))

(defvar eglot-copilot-panel-buffer nil
  "A reference to the buffer that's displaying the copilot panel")

(defvar eglot-copilot-panel-solutions--accumulator nil
  "Accumulates the temporary results while generating a panel.
Can be used to get the most up to dat suggestions,
but the availability is intermittent.")

(defvar eglot-copilot-panel-solutions--state "Not Initialized"
  "The current refresh state of the panel solutions.
Used mainly for signalling to the end user the current state of
the panel.")

(defvar eglot-copilot-panel-solutions--ids (ht)
  "A hashtable of the solution ids already seen.
Used to dedupe the `eglot-copilot-panel-solutions--accumulator'.")

(defvar eglot-copilot-panel-solutions nil
  "The fully generated list of solutions.")

(defvar eglot-copilot-buffer-to-panel-id (ht)
  "Maps a buffer object to its panel id.")

(defvar eglot-copilot-panel-id-counter 1
  "Counter for the next panel id")

(defvar eglot-copilot-panel--idle-timer nil
  "Timer that handles refreshing the copilot panel.")

(defvar eglot-copilot--shadow-buffer-map (ht)
  "Map of copilot buffers to their corresponding shadow buffers")

(defvar eglot-copilot--result-token-cache (ht)
  "Cache of tokens from copilot for use with autocomplete.")

(defconst eglot-copilot--token-delimiter-regexp
  (rx (or " " "." "(" ")" "{" "}" "[" "]" ?\" ))
  "Regexp for spliting copilot results into tokens for search.")

(defvar-local eglot-copilot-panel--target-buffer nil
  "The target buffer for the current copilot-panel")

(defclass eglot-copilot-lsp-server (eglot-lsp-server)
  nil
  :documentation "The LSP Server used by eglot-copilot to handle output from copilot.")

;;;###autoload
(defun eglot-copilot-setup ()
  "Setup the copilot integration to ensure the best user experience."
  (add-to-list 'eglot-server-programs '(eglot-copilot-shadow-mode . eglot-copilot-shadow-mode-server-command))
  (push #'eglot-copilot-sort-results-company-transformer company-transformers)
  (add-hook 'eglot-copilot-shadow-kill-buffer-hook kill-buffer-hook)
  (when (fboundp #'ivy-set-actions)
    (ivy-set-actions 'eglot-copilot-counsel '(("c" eglot-copilot-counsel--commit-action "Commit"))))
  (eglot-copilot--start-panel-refresh-timer))

;;;###autoload
(defun eglot-copilot-counsel ()
  "Runs ivy to preview results from copilot."
  (interactive)
  (unless (eq (buffer-local-value 'eglot-copilot-panel--target-buffer eglot-copilot-panel-buffer)
              (current-buffer))
    (user-error "No copilot results yet for current buffer."))
  (ivy-read
   "Copilot:"
   (->> eglot-copilot-panel-solutions
        (--sort (> (car  it) (car other)))
        (-map #'cdr)
        (--map (apply #'propertize
                      (s-join "\n" (-take 3 (s-lines it)))
                      (text-properties-at 0 it))))
   :require-match t
   :action #'eglot-copilot-counsel--preview-select-action
   :caller 'eglot-copilot-counsel))

;;;###autoload
(defun eglot-copilot-sign-in ()
  "Shows the sign-in form for authenticating with copilot."
  (interactive)
  (jsonrpc-async-request
   (eglot-copilot-shadow-eglot-server) :signInInitiate (eglot--TextDocumentPositionParams)
   :success-fn (lambda (response &rest arg)
                 (message "User Code: %s" (plist-get response :userCode))
                 (browse-url (plist-get response :verificationUri)))
   :error-fn (lambda (&rest arg) (message "%S" arg))))

;;;###autoload
(defun eglot-copilot-check-status ()
  "Checks the login status for copilot authentication."
  (interactive)
  (jsonrpc-async-request (eglot-copilot-shadow-eglot-server) :checkStatus
                         (eglot--TextDocumentPositionParams)
                         :success-fn (lambda (response &rest arg)
                                       (message "%s" response))
                         :error-fn (lambda (&rest arg) (message "%s" arg))))

;;;###autoload
(defun eglot-copilot-company (command &optional arg &rest ignored)
  "Company backend for completing using copilot suggestions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'eglot-copilot-company))
    (match t)
    (prefix (eglot-copilot--company-prefix))
    (no-cache t)
    (sorted t)
    (post-completion
     (let* ((inhibit-modification-hooks t))
       (atomic-change-group
         (delete-char (- (length arg)))
         (let* ((range (get-text-property 0 :range arg))
                (inhibit-modification-hooks t)
                (start-pt (point))
                (end (eglot-copilot--lsp-position-to-point (plist-get range :end)))
                (start (eglot-copilot--lsp-position-to-point (plist-get range :start))))
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
     (-some--> (eglot-copilot-shadow-eglot-server)
       (let ((server it)
             (current-pt (point))
             (shadow-buffer (eglot-copilot--ensure-shadow-buffer (current-buffer))))
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
                    :error-fn (lambda (&rest arg) (message "%S" arg)))))))))))

(defun eglot-copilot--enabled-in-buffer-p ()
  (and (derived-mode-p #'prog-mode)
       (not buffer-read-only)
       (buffer-file-name)
       (projectile-project-root)
       (not (minibufferp))
       (not (string= "class" (file-name-extension (buffer-file-name))))
       (condition-case err (eglot-copilot--ensure-shadow-buffer (current-buffer))
         (error nil))))

;;;###autoload
(defun eglot-copilot-panel-refresh ()
  "Refresh the copilot suggestions buffer for the current buffer."
  (interactive)

  (when (eglot-copilot--enabled-in-buffer-p)
    (setq eglot-copilot-panel-solutions--state "REFRESHING")
    (unless (and eglot-copilot-panel-buffer
                 (buffer-live-p eglot-copilot-panel-buffer))
      (setq eglot-copilot-panel-buffer (generate-new-buffer "*copilot*")))
    (let ((target-buffer (current-buffer)))
      (with-current-buffer eglot-copilot-panel-buffer
        (when (not (eq eglot-copilot-panel--target-buffer target-buffer))
          (erase-buffer)
          (setq eglot-copilot-panel--target-buffer target-buffer))))
    (setq eglot-copilot-panel-solutions--accumulator nil)
    (setq eglot-copilot-panel-solutions--ids (ht))
    (-some--> (eglot-copilot-shadow-eglot-server)
      (when (process-live-p (jsonrpc--process it))
        (jsonrpc-async-request it :getPanelCompletions
                               (append
                                (list
                                 :panelId
                                 (or
                                  (ht-get eglot-copilot-buffer-to-panel-id (current-buffer))
                                  (--doto (concat "copilot:///" (number-to-string eglot-copilot-panel-id-counter))
                                    (setq eglot-copilot-panel-id-counter (1+ eglot-copilot-panel-id-counter))
                                    (ht-set eglot-copilot-buffer-to-panel-id (current-buffer) it)))
                                 :doc
                                 (list :path (buffer-file-name)
                                       :position (eglot--pos-to-lsp-position)
                                       :uri (eglot--path-to-uri (buffer-file-name))))
                                (eglot--TextDocumentPositionParams)
                                )
                               :deferred :getPanelCompletions)))
    (with-current-buffer eglot-copilot-panel-buffer
      (eglot-copilot-panel--generate-buffer))))


(defun eglot-copilot--first-token (text)
  (or (-some--> (string-match eglot-copilot--token-delimiter-regexp text) (substring text 0 it))
      text))

(defun eglot-copilot-tokenize (text)
  (let ((target text)
        (acc nil))
    (while (> (length target) 0)
      (if-let ((index (string-match eglot-copilot--token-delimiter-regexp target)))
          (progn
            (if (eq index 0)
                (while (eq (string-match eglot-copilot--token-delimiter-regexp target) 0)
                  (setq target (substring target 1)))
              (push (substring target 0 index) acc)
              (setq target (substring target index))))
        (push target acc) 
        (setq target nil)))
    acc))

;;;###autoload
(defun eglot-copilot-sort-results-company-transformer (results)
  "Company transformer for sorting results based on the output from copilot.
See `company-transformers'."
  (when results
    (let ((copilot-results nil)
          (normal-results nil))
      (cl-loop for result in results
               do
               (if (ht-get eglot-copilot--result-token-cache (eglot-copilot--first-token result))
                   (push result copilot-results)
                 (push result normal-results)))
      (append copilot-results normal-results nil))))

(define-derived-mode eglot-copilot-shadow-mode fundamental-mode "copilot-shadow"
  "Mode used by shadow buffers for copilot.")

(defun eglot-copilot-shadow-eglot-server ()
  "Return the eglot server used by the shadow buffer for the current buffer."
  (-some-->
      (eglot-copilot--ensure-shadow-buffer (current-buffer))
      (with-current-buffer it
        (eglot-current-server))))

(defun eglot-copilot-shadow-kill ()
  "Kills the shadow buffer for the current buffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
     (-some--> (ht-get eglot-eglot-copilot--ensure-shadow-buffer-map (buffer-file-name))
      (progn
        (kill-buffer it)
        (ht-remove eglot-eglot-copilot--ensure-shadow-buffer-map file-name)))))

(defun eglot-copilot-shadow-clean ()
  "Kills all copilot buffers and corresponding eglot processes"
  (interactive)
  (let ((processes nil))
    (->> (ht-values eglot-eglot-copilot--ensure-shadow-buffer-map)
         (--map (progn
                  (-some--> (get-buffer-process it) (setq processes (cons it processes)))
                  (kill-buffer it))))
    (ht-clear eglot-eglot-copilot--ensure-shadow-buffer-map)
    (->> (ht-values eglot--servers-by-project)
         (--filter (eglot-copilot-lsp-server-p (car it)))
         (--map (eglot-shutdown (car it))))
    (->> processes (--filter (eq (process-status it) 'run)) (--map (kill-process it)))))

(defun eglot-copilot--company-prefix ()
  "Return the prefix at point for the copilot company-backend."
  (and (-some--> (progn (save-excursion (re-search-backward eglot-copilot--token-delimiter-regexp nil t)
                                        (1+ (point))))
         (if (< it (point)) (cons (buffer-substring it (max it (point))) t)
           (cons "" t)))))

(defun eglot-copilot-panel--generate-buffer ()
  "Generates the copilot panel solutions buffer based on a list of solutions."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (concat eglot-copilot-panel-solutions--state "\n"))
    (cl-loop for completion in (--sort (> (car it) (car other))
                                       (or
                                        eglot-copilot-panel-solutions--accumulator
                                        eglot-copilot-panel-solutions))
             do
             (insert (cdr completion))
             (insert "\n---\n"))))

(defun eglot-copilot--ensure-shadow-buffer (current-buffer)
  "Return the shadow buffer for the current buffer.
Creates it if it does not exist."
  (when (and current-buffer (buffer-live-p current-buffer))
    (save-mark-and-excursion
      (with-current-buffer current-buffer
        (setq-local before-change-functions
                    (-uniq (cons #'eglot-copilot--before-change-hook
                                 before-change-functions)))
        (setq-local after-change-functions
                    (-uniq (cons #'eglot-copilot--after-change-hook
                                 after-change-functions)))
        (or (let ((language-id "copilot"))
              (-some-->
                  (ht-get eglot-copilot--shadow-buffer-map (buffer-file-name current-buffer))
                (when (buffer-live-p it) it)))
            (let (
                  (language-id "copilot")
                  (copied
                   (progn
                     (widen)
                     (buffer-string)))
                  (file-name (buffer-file-name)))
              (unless (f-exists-p eglot-copilot-node-agent-script)
                (user-error (concat "`eglot-copilot-node-agent-script': "
                                    (format "%S" eglot-copilot-node-agent-script)
                                    " Does Not Exist.")))
              (unless (executable-find eglot-copilot-node-program)
                (user-error (concat "`eglot-copilot-node-program': "
                                    (format "%S" eglot-copilot-node-program)
                                    " Not Found in `exec-path'.")))
              (let ((node-version
                     (-some-->
                         (shell-command-to-string
                          (s-join " " (list eglot-copilot-node-program "--version")))
                       (string-trim it)
                       (s-split-up-to (rx ".") it 1)
                       (car it)
                       (substring it 1)
                       (string-to-number it))))
                (when (> node-version 18) (user-error "Node version should be less than v18"))
                (-some--> (ht-get eglot-copilot--shadow-buffer-map file-name)
                  (when (buffer-live-p it) (kill-buffer it)))
                (let ((buffer (with-current-buffer
                                  (generate-new-buffer (concat " *copilot shadow " (buffer-name) "*"))
                                (insert copied)
                                (eglot-copilot-shadow-mode)
                                (set-buffer-modified-p nil)
                                (setq-local buffer-file-name file-name)
                                (eglot-copilot-shadow--eglot-ensure language-id)
                                (eglot--maybe-activate-editing-mode)
                                (set-buffer-modified-p nil)
                                (current-buffer))))
                  (ht-set eglot-copilot--shadow-buffer-map
                          file-name
                          buffer)))))))))

;; Complete this comment: 
(defun eglot-copilot-force-start ()
  (interactive)
  (with-current-buffer (eglot-copilot--ensure-shadow-buffer (current-buffer))
    (eglot
     #'eglot-copilot-shadow-mode
     (cons 'projectile
           (projectile-project-root))
     #'eglot-copilot-lsp-server
     (eglot-copilot-shadow-mode-server-command)
     "copilot" t)))

(defun eglot-copilot-shadow--eglot-ensure (language-id)
  "Ensures that eglot is enabled. If it isn't, then start it for `eglot-copilot-shadow-mode'."
  (when language-id
    (or (and (eglot-managed-p)
             (string= language-id (eglot--language-id (eglot-current-server))))
        (and (eglot-current-server)
             (string= language-id (eglot--language-id (eglot-current-server)))
             (prog1 t (eglot--maybe-activate-editing-mode)))
        (eglot
         #'eglot-copilot-shadow-mode
         (cons 'projectile
               (projectile-project-root))
         #'eglot-copilot-lsp-server
         (eglot-copilot-shadow-mode-server-command)
         "copilot" t))))

(defun eglot-copilot--before-change-hook (beg end)
  "before-change-functions hook for forwarding `eglot--before-change' to the shadow buffer."
  (when (eglot-copilot--enabled-in-buffer-p)
    (-some--> (eglot-copilot--ensure-shadow-buffer (current-buffer))
      (let ((shadow it)
            (copied (buffer-substring beg end)))
        (with-current-buffer shadow
          (ignore-errors (eglot--before-change beg end)))))))

(defun eglot-copilot--after-change-hook (beg end length)
  "after-change-functions hook for forwarding `eglot--after-change' to the shadow buffer."
  (when (eglot-copilot--enabled-in-buffer-p)
    (-some--> (eglot-copilot--ensure-shadow-buffer (current-buffer))
      (let ((shadow it)
            (copied (buffer-substring beg end)))
        (with-current-buffer shadow
          (let ((pt (point)))
            (save-excursion
              (let ((inhibit-modification-hooks t)
                    (inhibit-point-motion-hooks t)
                    (inhibit-read-only t))
                (goto-char beg)
                (condition-case nil
                    (delete-char length)
                  (end-of-buffer nil))
                (insert copied)
                (ignore-errors (eglot--after-change beg end length))
                (set-buffer-modified-p nil)))
            (goto-char pt)))))))

(defun eglot-copilot-shadow-kill-buffer-hook ()
  "Handles killing the copilot shadow buffer when the owner buffer is killed."
  (let ((file-name (buffer-file-name)))
    (-some--> file-name
      (ht-get eglot-copilot--shadow-buffer-map it)
      (progn
        (ht-remove eglot-copilot--shadow-buffer-map file-name)
        (when (buffer-live-p it) (kill-buffer it))))))

(defun eglot-copilot-shadow-mode-server-command (&rest arg)
  "Return the command to run copilot for eglot.
Designed to worked with `eglot-server-programs'."
  (list eglot-copilot-node-program
        eglot-copilot-node-agent-script))

(defun eglot-copilot--start-panel-refresh-timer ()
  "Starts the timer responsible for updating the copilot panel."
  (when eglot-copilot-panel--idle-timer (cancel-timer eglot-copilot-panel--idle-timer))
  (setq eglot-copilot-panel--idle-timer
        (run-with-idle-timer
         0.0 t
         (lambda (&rest arg)
           (eglot-copilot-panel-refresh)))))

(cl-defmethod eglot-handle-notification
  ((server eglot-copilot-lsp-server) (_method (eql PanelSolution)) &rest params)
  "Handle server request PanelSolution for copilot panel."
  (when (string= (plist-get params :panelId)
                 (ht-get eglot-copilot-buffer-to-panel-id
                         (buffer-local-value 'eglot-copilot-panel--target-buffer
                                             eglot-copilot-panel-buffer)))
    (unless (ht-get eglot-copilot-panel-solutions--ids (plist-get params :solutionId))
      (ht-set eglot-copilot-panel-solutions--ids (plist-get params :solutionId) t)
      (push
       (cons (plist-get params :score)
             (propertize (concat (format "%.2f: " (plist-get params :score))
                                 (plist-get params :displayText))
                         :id (plist-get params :solutionId)
                         :range (plist-get params :range)
                         :insertText (plist-get params :completionText)))
       eglot-copilot-panel-solutions--accumulator)
      (eglot-copilot--update-result-cache))
    (when eglot-copilot-panel-buffer
      (with-current-buffer eglot-copilot-panel-buffer
        (eglot-copilot-panel--generate-buffer)))))

(cl-defmethod eglot-handle-notification
  ((server eglot-copilot-lsp-server) (_method (eql PanelSolutionsDone)) &rest params)
  "Handle server request PanelSolution for copilot panel."
  (when (string= (plist-get params :panelId)
                 (ht-get eglot-copilot-buffer-to-panel-id
                         (buffer-local-value 'eglot-copilot-panel--target-buffer
                                             eglot-copilot-panel-buffer)))
    (when eglot-copilot-panel-solutions--accumulator
      (setq eglot-copilot-panel-solutions (append eglot-copilot-panel-solutions--accumulator nil))
      (setq eglot-copilot-panel-solutions--state "COMPLETE")
      (setq eglot-copilot-panel-solutions--accumulator nil)
      )
    (eglot-copilot--update-result-cache)
    (when eglot-copilot-panel-buffer
      (with-current-buffer eglot-copilot-panel-buffer
        (eglot-copilot-panel--generate-buffer)))))
(defun eglot-copilot-panel-select-at-point ()
  "Selects the entry at point in the copilot panel, and performs
the replacement in the target buffer for the panel."
  (interactive)
  (let ((range (get-text-property (point) :range))
        (insert-text (get-text-property (point) :insertText)))
    (with-current-buffer eglot-copilot-panel--target-buffer
      (goto-char (eglot-copilot--lsp-position-to-point (plist-get range :start)))
      (delete-region (point)
                     (eglot-copilot--lsp-position-to-point (plist-get range :end)))
      (insert insert-text))))

(defun eglot-copilot--lsp-position-to-point (lsp-pt)
  "Converts an lsp position to a buffer point."
  (ignore-errors
    (or
     (eglot--lsp-position-to-point
      lsp-pt)
     (point-max))))

(defun eglot-copilot--update-result-cache ()
  "Refresh the `eglot-copilot--result-token-cache' based on
the current `eglot-copilot-panel-solutions--accumulator' and `eglot-copilot-panel-solutions'."
  (setq eglot-copilot--result-token-cache (ht))
  (setq eglot-copilot-panel-solutions--accumulator (-uniq eglot-copilot-panel-solutions--accumulator))
  (cl-loop for solution in (append eglot-copilot-panel-solutions eglot-copilot-panel-solutions--accumulator nil)
           do
           (cl-loop 
            for word in (when solution (eglot-copilot-tokenize (get-text-property 0 :insertText (cdr solution))))
            do
            (ht-set eglot-copilot--result-token-cache word t))))

(defun eglot-copilot--replace-text (selection &optional highlight-replacement)
  "Replaces the text referenced by SELECTION. If HIGHLIGHT-REPLACEMENT is non-nil,
add a face to the replacement."
  (let* ((range (get-text-property 0 :range selection))
         (inhibit-modification-hooks t)
        (insert-text (get-text-property 0 :insertText selection)))
    (when insert-text
      (let* ((start-pt (save-excursion
                         (goto-char (eglot-copilot--lsp-position-to-point (plist-get range :start)))
                         (forward-line 0)
                         (point)))
             (start-window-pt (save-excursion (goto-char start-pt)
                                              (ignore-errors (forward-line -3))
                                              (point)))
             (end-pt (save-excursion (goto-char (eglot-copilot--lsp-position-to-point (plist-get range :end)))
                                     (end-of-line)
                                     (1+ (point)))))
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

(defun eglot-copilot-counsel--preview-select-action (selection)
  "Counsel action for selecting a copilot preview."
  (let* ((buffer-name "*copilot-preview*")
        (buffer-string
         (with-current-buffer eglot-copilot-panel-buffer
           (with-current-buffer eglot-copilot-panel--target-buffer
             (buffer-string)))))
    (with-current-buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name))
      (erase-buffer)
      (insert buffer-string)
      (eglot-copilot--replace-text selection t))))

(defun eglot-copilot-counsel--commit-action (selection)
  "Counsel action for selecting a copilot preview."
  (eglot-copilot--replace-text selection t))

(provide 'eglot-copilot)
;;; eglot-copilot.el ends here
