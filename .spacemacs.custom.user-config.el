;;; .spacemacs.custom.user-config.el --- User configuration

(defun dotspacemacs/user-config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  (defun transpose-chars-before-point (arg)
    "Interchange the two characters before point."
    (interactive "*P")
    (transpose-chars -1))

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

  (setq-default windmove-wrap-around t)

  ;; indentation
  (global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

  ;; documentation
  (define-key evil-normal-state-map (kbd "C-h C-f") 'find-function)
  (define-key evil-normal-state-map (kbd "C-h C-v") 'find-variable)
  (define-key evil-normal-state-map (kbd "C-h C-l") 'find-library)

  ;; text movement
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-chars-before-point)
  (define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
  (define-key evil-normal-state-map (kbd "M-k") 'move-text-up)

  ;; evil snipe
  (define-key evil-normal-state-map (kbd "\\") 'evil-snipe-repeat-reverse)
  (define-key evil-visual-state-map (kbd "\\") 'evil-snipe-repeat-reverse)

  ;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t) (sh . t)))

  ;; magit mode
  (add-hook 'magit-mode-hook (lambda () (evil-mc-mode -1)))

  ;; company mode
  (define-key company-active-map (kbd "C-h") 'delete-backward-char)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

  ;; hooks
  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (add-hook 'after-change-major-mode-hook 'update-whitespace-hooks)
  (add-hook 'emacs-lisp-mode-hook (lambda () (aggressive-indent-mode 1)))

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

  (define-key evil-normal-state-map (kbd "M-l") 'helm-imenu)

  ;; abbrevs
  (setq abbrev-file-name (concat user-emacs-directory "abbrevs.el"))
  (setq save-abbrevs 'silently)
  (quietly-read-abbrev-file)
  (setq-default abbrev-mode t)

  ;; ruby mode
  (setq enh-ruby-program "/usr/bin/ruby")

  ;; term mode
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 30000)
              (setq term-scroll-show-maximum-output t)
              (setq multi-term-scroll-show-maximum-output t)
              (setq multi-term-scroll-to-bottom-on-output t)))

  ;; org mode
  (evil-define-key 'normal evil-org-mode-map
    "J" 'org-forward-heading-same-level
    "K" 'org-backward-heading-same-level
    "-" 'dired-jump)

  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))


  ;; js2-mode
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq-local comment-auto-fill-only-comments t)
              (auto-fill-mode 1)
              (setq-local comment-multi-line t)
              (local-set-key (kbd "RET") 'c-indent-new-comment-line)))

  (add-to-list 'completion-styles 'initials t)
  (add-to-list 'auto-mode-alist '(".eslintrc" . json-mode))
  (add-to-list 'auto-mode-alist '(".jshintrc" . json-mode))

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   jsx-indent-level 2
   js2-strict-inconsistent-return-warning nil
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
  (setq mode-require-final-newline 'visit-save)
  (setq powerline-default-separator 'arrow)
  (setq scroll-margin 1)

  (setq-default truncate-lines t)

  (flycheck-pos-tip-mode 1)
  (global-company-mode)
  (global-evil-mc-mode 1)
  (global-hl-line-mode -1)
  (global-linum-mode 1)
  (global-subword-mode +1)
  (global-undo-tree-mode 1)
  (indent-guide-global-mode 1)
  (linum-relative-mode 1)
  (semantic-mode 1)
  (smartparens-global-mode 1)
  (turn-on-smartparens-mode)

  (let* ((parent (file-name-as-directory user-emacs-directory))
         (local (concat parent ".spacemacs.custom.user-config.local.el")))
    (when (file-exists-p local)
      (load-file local)
      (require 'spacemacs-custom-user-config-local)
      (dotspacemacs/user-config-local))))

(provide 'spacemacs-custom-user-config)
