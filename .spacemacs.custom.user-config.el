;;; .spacemacs.custom.user-config.el --- User configuration

(defun dotspacemacs/user-config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  (defun indent-buffer ()
    "Indent the currently visited buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun indent-region-or-buffer ()
    "Indent a region if selected, otherwise the whole buffer."
    (interactive)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))))

  (defvar-local whitespace-cleanup-disabled nil
    "True if `whitespace-cleanup' must not be performed.")

  (defun do-whitespace-cleanup ()
    "Perform `whitespace-cleanup' unless in insert mode."
    (when (and (not (evil-insert-state-p))
               (not whitespace-cleanup-disabled))
      (let ((whitespace-style '(face
                                tabs spaces trailing lines space-before-tab newline
                                indentation space-after-tab
                                space-mark tab-mark newline-mark)))
        (whitespace-cleanup))))

  (defun update-whitespace-hooks ()
    "Updates the white space hooks based on the current major mode."
    (let ((modes '(emacs-lisp-mode js2-mode scss-mode react-mode java-mode))
          (whitespace-hooks '(auto-save-hook before-save-hook)))
      (if (member major-mode modes)
          (mapcar (lambda (hook) (add-hook hook 'do-whitespace-cleanup nil t))
                  whitespace-hooks)
        (mapcar (lambda (hook) (remove-hook hook 'do-whitespace-cleanup t))
                whitespace-hooks))))

  (defun delete-tern-process ()
    (interactive)
    (delete-process "Tern"))

  (evil-define-command evil-window-split-and-focus (&optional count file)
    "Splits the current window horizontally and sets the focus to it."
    :repeat nil
    (interactive "P<f>")
    (evil-window-split count file)
    (other-window 1))

  (evil-define-command evil-window-vsplit-and-focus (&optional count file)
    "Splits the current window vertically and sets the focus to it."
    :repeat nil
    (interactive "P<f>")
    (evil-window-vsplit count file)
    (other-window 1))

  (defun sort-words (reverse beg end)
    "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
    (interactive "*P\nr")
    (sort-regexp-fields reverse "\\w+" "\\&" beg end))

  (defun evil-window-decrease-height-by-3 ()
    (interactive)
    (evil-window-decrease-height 3))
  (defun evil-window-increase-width-by-3 ()
    (interactive)
    (evil-window-increase-width 3))
  (defun evil-window-increase-height-by-3 ()
    (interactive)
    (evil-window-increase-height 3))
  (defun evil-window-decrease-width-by-3 ()
    (interactive)
    (evil-window-decrease-width 3))

  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (defun what-face (pos)
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  (defun show-file-name()
    "Show the full path file name in the minibuffer."
    (interactive)
    (message (buffer-file-name)))

  (defun safe-run-fn (fn)
    "Run an function if it exists"
    (when (fboundp fn)
      (condition-case err (funcall fn)
        (error (message "%s" (error-message-string err))))))

  (defun haskell-on-auto-save()
    "Actions to be performed for haskell buffers on auto-save"
    (safe-run-fn 'hindent-reformat-buffer))

  (defun flycheck-pos-tip-mode-turn-off ()
    "Disable `flycheck-pos-tip-mode'"
    (interactive)
    (flycheck-pos-tip-mode -1)
    (setq flycheck-display-errors-function 'flycheck-display-error-messages))

  (defun intero-mode-turn-off ()
    "Turn off `intero-mode' in current buffer."
    (interactive)
    (intero-mode -1)
    (flycheck-mode -1)
    (company-mode -1))

  (defun elm-on-auto-save()
    "Actions to be performed for elm buffers on auto-save"
    (safe-run-fn 'elm-mode-format-buffer))

  (defun setup-js2-mode ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode nil)
    (setq-local comment-multi-line t)
    (local-set-key (kbd "RET") 'c-indent-new-comment-line))

  (defun setup-org-mode ()
    (make-variable-buffer-local 'yas/trigger-key)
    (setq yas/trigger-key [tab])
    (setq flyspell-mode -1)
    (setq org-src-preserve-indentation nil
          org-edit-src-content-indentation 2
          org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
    (define-key yas/keymap [tab] 'yas/next-field))

  (defun setup-term-mode ()
    (setq term-buffer-maximum-size 30000)
    (setq term-scroll-show-maximum-output t)
    (setq multi-term-scroll-show-maximum-output t)
    (setq multi-term-scroll-to-bottom-on-output t))

  ;; scratch buffer related
  (defvar scratch-default-directory
    (concat user-home-directory ".scratch-saved")
    "Directory used to save scratch buffers")

  (defun ensure-directory-exists (path)
    "Creates all directories in PATH if they don't exist."
    (unless (or (null path) (string-empty-p path) (file-exists-p path))
      (let ((parent (file-name-directory (directory-file-name path))))
        (ensure-directory-exists parent)
        (make-directory path))))

  (defun create-scratch-buffer ()
    "Create a scratch buffer."
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))

  (defun save-scratch-buffer ()
    "Saves the scratch buffer with a name that includes today's date.
The buffer will be saved to a file according to the path
at `scratch-default-directory'."
    (interactive)
    (let* ((original (current-buffer))
           (scratch (get-buffer "*scratch*"))
           (file-ext ".bak")
           (timestamp ".%m%d%Y")
           (base "scratch")
           (dest-dir (file-name-as-directory (or
                                              scratch-default-directory
                                              default-directory)))
           (file (concat dest-dir base (format-time-string timestamp) file-ext)))
      (progn
        (switch-to-buffer scratch)
        (ensure-directory-exists dest-dir)
        (write-file file)
        (create-scratch-buffer)
        (switch-to-buffer original))))

  (defun capitalize-and-insert ()
    "Capitalize the letter at cursor and enter insert mode."
    (interactive)
    (progn
      (evil-invert-char (point) (+ 1 (point)))
      (evil-insert 1)))

  (defun replace-multiple-regexp-in-string (xs str)
    "Replace all matches of (regex . replacement) for every pair in XS in STR."
    (if (null xs)
        str
      (let ((current (car xs))
            (rest (cdr xs)))
        (replace-multiple-regexp-in-string
         rest
         (replace-regexp-in-string (car current) (cdr current) str)))))

  (defun with-region (fun start end)
    "Run FUN over the region between START and END in current buffer"
    (save-excursion
      (let ((text (delete-and-extract-region start end)))
        (insert (funcall fun text)))))

  ;; URL encoding functions
  (defun encode-uri-component (start end)
    "Encode the region between START and END to be used within a URL as a component."
    (interactive "r")
    (with-region 'url-hexify-string start end))

  (defun decode-uri-component (start end)
    "Decode the region between START and END assume to be an encoded URL component."
    (interactive "r")
    (with-region 'url-unhex-string start end))

  (defun encode-uri (start end)
    "Encode the region between START and END to be used as a full URL."
    (interactive "r")
    (with-region 'org-link-escape start end))

  (defun decode-uri (start end)
    "Decode the region between START and END assumed to be a full URL encoded."
    (interactive "r")
    (with-region 'org-link-unescape start end))

  (defvar current-date-time-format "%Y.%m.%d-%H.%M.%S"
    "Format of date to insert with `insert-current-date-time' function.
See help of `format-time-string' for possible replacements.")

  (defvar current-time-format "%Y.%m.%d"
    "Format of date to insert with `insert-current-date' function.
 See help of `format-time-string' for possible replacements.")

  (defun insert-current-time-stamp (format)
    "Insert the current timestamp formatted according to FORMAT into the current buffer."
    (insert (format-time-string format (current-time))))

  (defun insert-current-date-time ()
    "Insert the current date and time into the current buffer.
Uses `current-date-time-format' for formatting the date."
    (interactive)
    (insert-current-time-stamp current-date-time-format))

  (defun insert-current-date ()
    "Insert the current date into the current buffer.
Uses `current-date-format' for formatting the date."
    (interactive)
    (insert-current-time-stamp current-date-format))

  ;; fonts
  (defun set-custom-font (font height)
    (when (member font (font-family-list))
      (set-face-attribute
       'default nil
       :stipple nil
       :height height
       :width 'normal
       :inverse-video nil
       :box nil
       :strike-through nil
       :overline nil
       :underline nil
       :slant 'normal
       :weight 'normal
       :foundry "outline"
       :family font)))

  (defun create-set-font (input)
    (let ((name-normal (intern (format "set-font-%s" (car input))))
          (name-large (intern (format "set-font-%s-large" (car input))))
          (name-medium (intern (format "set-font-%s-medium" (car input))))
          (name-small (intern (format "set-font-%s-small" (car input))))
          (font (cdr input)))
      `(progn
         (defun ,name-large ()
           (interactive) (set-custom-font ,font 170))
         (defun ,name-normal ()
           (interactive) (set-custom-font ,font 160))
         (defun ,name-medium ()
           (interactive) (set-custom-font ,font 150))
         (defun ,name-small ()
           (interactive) (set-custom-font ,font 140)))))

  (defmacro create-set-font-funs (funs)
    `(progn ,@(mapcar 'create-set-font funs)))

  ;; Create functions for setting fonts
  ;; The function names will be like `set-font-camingo' and `set-font-camingo-small'
  (macroexpand (create-set-font-funs (("camingo" . "CamingoCode")
                                      ("dejavu" . "DejaVu Sans Mono")
                                      ("input" . "Input Mono")
                                      ("source-code" . "Source Code Pro")
                                      ("monaco" . "Monaco")
                                      ("monacoB2" . "MonacoB2")
                                      ("monacoB2powerline" . "MonacoB2 for Powerline"))))

  ;; nxml mode
  (defun nxml-pretty-format ()
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
      (nxml-mode)
      (indent-region begin end)))

  ;; auto-save hooks
  (add-hook 'auto-save-hook
            (lambda ()
              (cond ((eq major-mode 'elm-mode)
                     (elm-mode-format-buffer)))))

  (setq-default windmove-wrap-around t)

  ;; indentation
  (global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

  ;; documentation
  (define-key evil-normal-state-map (kbd "C-h C-f") 'find-function)
  (define-key evil-normal-state-map (kbd "C-h C-v") 'find-variable)
  (define-key evil-normal-state-map (kbd "C-h C-l") 'find-library)

  ;; text movement
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
  (define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
  (define-key evil-normal-state-map (kbd "M-k") 'move-text-up)

  (define-key evil-normal-state-map (kbd ",i") 'capitalize-and-insert)

  ;; evil snipe
  (define-key evil-normal-state-map (kbd "\\") 'evil-snipe-repeat-reverse)
  (define-key evil-visual-state-map (kbd "\\") 'evil-snipe-repeat-reverse)

  ;; company mode
  (when (boundp 'company-active-map)
    (define-key company-active-map (kbd "C-h") 'delete-backward-char)
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))

  ;; helm
  (bind-key* "<f9>" 'helm-semantic-or-imenu) ;; see also SPC-s-j

  ;; nxml mode
  (evil-define-key 'normal nxml-mode-map (kbd "C-c C-f") 'nxml-pretty-format)

  ;; hooks
  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (add-hook 'after-change-major-mode-hook 'update-whitespace-hooks)

  ;; windows
  (define-key evil-visual-state-map (kbd "C-w s") 'evil-window-split-and-focus)
  (define-key evil-normal-state-map (kbd "C-w s") 'evil-window-split-and-focus)
  (define-key evil-visual-state-map (kbd "C-w v") 'evil-window-vsplit-and-focus)
  (define-key evil-normal-state-map (kbd "C-w v") 'evil-window-vsplit-and-focus)

  (define-key evil-normal-state-map (kbd "<up>")
    'evil-window-increase-height-by-3)
  (define-key evil-normal-state-map (kbd "<down>")
    'evil-window-decrease-height-by-3)
  (define-key evil-normal-state-map (kbd "<right>")
    'evil-window-increase-width-by-3)
  (define-key evil-normal-state-map (kbd "<left>")
    'evil-window-decrease-width-by-3)

  ;; avy
  (define-key evil-normal-state-map (kbd ",j") 'avy-goto-line-below)
  (define-key evil-normal-state-map (kbd ",k") 'avy-goto-line-above)
  (define-key evil-normal-state-map (kbd ",w") 'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "<f8>") 'avy-goto-char-timer)

  ;; abbrevs
  (setq abbrev-file-name (concat user-emacs-directory "abbrevs.el"))
  (setq save-abbrevs 'silently)
  (quietly-read-abbrev-file)
  (setq-default abbrev-mode t)

  ;; evil-mc
  (global-set-key (kbd "C-S-<mouse-1>") 'evil-mc-toggle-cursor-on-click)
  (global-set-key (kbd "C-S-<mouse-3>") 'evil-mc-toggle-cursor-on-click)

  ;; evil-jump
  (define-key evil-normal-state-map (kbd "<f11>") 'evil-jump-backward)
  (define-key evil-normal-state-map (kbd "<f12>") 'evil-jump-forward)

  ;; major mode hooks
  (add-hook 'js2-mode-hook 'setup-js2-mode)
  (add-hook 'org-mode-hook 'setup-org-mode);
  (add-hook 'term-mode-hook 'setup-term-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda () (aggressive-indent-mode 1)))

  ;; python mode
  (setq flycheck-flake8-maximum-line-length 120)

  ;; ruby mode
  (setq enh-ruby-program "/usr/bin/ruby")

  ;; org mode
  (setq org-ellipsis " ⤵")
  (evil-define-key 'normal evil-org-mode-map
    "J" 'org-forward-heading-same-level
    "K" 'org-backward-heading-same-level
    "-" 'dired-jump)

  ;; org packages
  (require 'org-agenda)
  (require 'ob-python)
  (require 'ob-dot)
  (require 'ob-org)
  (require 'ob-sh)
  (org-bullets-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t) (dot . t) (sh . t)))

  (add-to-list 'completion-styles 'initials t)
  (add-to-list 'auto-mode-alist '(".eslintrc" . json-mode))
  (add-to-list 'auto-mode-alist '(".jshintrc" . json-mode))

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 2
   jsx-indent-level 2
   js2-strict-inconsistent-return-warning nil
   js-switch-indent-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-attr-indent-offset 2)

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  ;; scala mode
  (setq ensime-sem-high-faces
        '((implicitConversion . (:underline (:color "#b58900")))
          (implicitParams (:underline (:color "#268bd2")))))

  ;; persistent undo
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; display the buffer path in the frame title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))

  ;; yasnippet
  ;; disallow nested expansions (can still be done with M-/)
  (setq yas-triggers-in-field nil)

  (setq auto-save-timeout 1)
  (setq global-mode-string nil)
  (setq mode-require-final-newline t)
  (setq powerline-default-separator 'arrow)
  (setq scroll-margin 1)
  (setq x-gtk-use-system-tooltips nil)

  (setq-default truncate-lines t)
  (setq create-lockfiles nil)

  (setq avy-all-windows nil)
  (setq avy-all-windows-alt nil)

  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  (setq evil-mc-mode-line-prefix "ⓜ")
  (setq evil-mc-mode-line-text-inverse-colors nil)
  (setq evil-mc-one-cursor-show-mode-line-text nil)

  (setq mouse-autoselect-window nil)

  (setq flycheck-display-errors-function 'flycheck-display-error-messages)

  (when (fboundp 'flycheck-pos-tip-mode) (flycheck-pos-tip-mode -1))
  (when (fboundp 'global-company-mode) (global-company-mode))
  (when (fboundp 'global-diff-hl-mode) (global-diff-hl-mode -1))
  (when (fboundp 'global-evil-mc-extras-mode) (global-evil-mc-extras-mode 1))
  (when (fboundp 'global-evil-mc-mode) (global-evil-mc-mode 1))
  (when (fboundp 'global-hl-line-mode) (global-hl-line-mode -1))
  (when (fboundp 'global-linum-mode) (global-linum-mode 1))
  (when (fboundp 'global-subword-mode) (global-subword-mode +1))
  (when (fboundp 'global-undo-tree-mode) (global-undo-tree-mode 1))
  (when (fboundp 'indent-guide-global-mode) (indent-guide-global-mode -1))
  (when (fboundp 'semantic-mode) (semantic-mode 1))
  (when (fboundp 'smartparens-global-mode) (smartparens-global-mode 1))
  (when (fboundp 'turn-on-smartparens-mode) (turn-on-smartparens-mode))

  (when (fboundp 'global-evil-search-highlight-persist)
    (global-evil-search-highlight-persist 1))
  (when (fboundp 'global-highlight-parentheses-mode)
    (global-highlight-parentheses-mode -1))

  (when (fboundp 'spaceline-compile) (spaceline-compile))

  (add-to-list 'auto-mode-alist '("\\.pdsc\\'" . json-mode))

  (let* ((parent (file-name-as-directory user-emacs-directory))
         (local (concat parent ".spacemacs.custom.user-config.local.el")))
    (when (file-exists-p local)
      (load-file local)
      (require 'spacemacs-custom-user-config-local)
      (dotspacemacs/user-config-local)))


  (require 'f)
  (require 'json)
  (require 'flycheck)
  (require 'helm-bookmark)

  (defun flycheck-parse-flow (output checker buffer)
    (let ((json-array-type 'list))
      (let ((o (json-read-from-string output)))
        (mapcar #'(lambda (errp)
                    (let ((err (cadr (assoc 'message errp))))
                      (flycheck-error-new
                       :line (cdr (assoc 'line err))
                       :column (cdr (assoc 'start err))
                       :level 'error
                       :message (cdr (assoc 'descr err))
                       :filename (f-relative
                                  (cdr (assoc 'path err))
                                  (f-dirname (file-truename
                                              (buffer-file-name))))
                       :buffer buffer
                       :checker checker)))
                (cdr (assoc 'errors o))))))

  (flycheck-define-checker javascript-flow
    "Javascript type checking using Flow."
    :command ("flow" "--json" source-original)
    :error-parser flycheck-parse-flow
    :modes (react-mode js2-mode javascript-mode)
    :next-checkers ((error . javascript-eslint))
    )

  (add-to-list 'flycheck-checkers 'javascript-flow)
  )

(message "user-config loaded")

(provide 'spacemacs-custom-user-config)
