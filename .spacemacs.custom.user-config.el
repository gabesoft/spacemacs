;;; .spacemacs.custom.user-config.el --- User configuration

(defun dotspacemacs/user-config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  (defun copy-to-the-end-of-line ()
    "Copies from point to the end of line."
    (interactive)
    (evil-yank (point) (point-at-eol)))

  (defun paste-after-current-line (count &optional register yank-handler)
    "Pastes the latest yanked text after the current line."
    (interactive "P<x>")
    (evil-insert-newline-below)
    (evil-normal-state 1)
    (when evil-auto-indent (indent-according-to-mode))
    (evil-paste-after count register yank-handler))

  (defun paste-before-current-line (count &optional register yank-handler)
    "Pastes the latest yanked text before the current line."
    (interactive "P<x>")
    (evil-insert-newline-above)
    (evil-normal-state 1)
    (when evil-auto-indent (indent-according-to-mode))
    (evil-paste-after count register yank-handler))

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

  (defmacro rename-modeline (package-name mode new-name)
    `(eval-after-load ,package-name
       '(defadvice ,mode (after rename-modeline activate)
          (setq mode-name ,new-name))))

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
    (let ((modes '(emacs-lisp-mode js2-mode scss-mode react-mode))
          (whitespace-hooks '(auto-save-hook before-save-hook)))
      (if (member major-mode modes)
          (mapcar (lambda (hook) (add-hook hook 'do-whitespace-cleanup nil t))
                  whitespace-hooks)
        (mapcar (lambda (hook) (remove-hook hook 'do-whitespace-cleanup t))
                whitespace-hooks))))

  (rename-modeline "js2-mode" js2-mode "JS2")
  (rename-modeline "clojure-mode" clojure-mode "Clj")

  (global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-chars-before-point)
  (define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
  (define-key evil-normal-state-map (kbd "M-k") 'move-text-up)
  (define-key evil-normal-state-map (kbd "Y") 'copy-to-the-end-of-line)

  (define-key evil-normal-state-map (kbd "\\") 'evil-snipe-repeat-reverse)
  (define-key evil-visual-state-map (kbd "\\") 'evil-snipe-repeat-reverse)

  ;; provided by unimpaired now
  ;; (define-key evil-normal-state-map (kbd "]p") 'paste-after-current-line)
  ;; (define-key evil-normal-state-map (kbd "[p") 'paste-before-current-line)

  ;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t) (sh . t)))


  (define-key evil-normal-state-map (kbd "<up>") 'evil-window-increase-height)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-window-decrease-height)
  (define-key evil-normal-state-map (kbd "<right>") 'evil-window-increase-width)
  (define-key evil-normal-state-map (kbd "<left>") 'evil-window-decrease-width)

  ;; company mode
  (define-key company-active-map (kbd "C-h") 'delete-backward-char)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

  (add-to-list 'company-backends 'company-nim)

  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (add-hook 'after-change-major-mode-hook 'update-whitespace-hooks)
  (add-hook 'emacs-lisp-mode-hook (lambda () (aggressive-indent-mode 1)))

  ;; windows
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

  (define-key evil-visual-state-map (kbd "C-w s") 'evil-window-split-and-focus)
  (define-key evil-normal-state-map (kbd "C-w s") 'evil-window-split-and-focus)
  (define-key evil-visual-state-map (kbd "C-w v") 'evil-window-vsplit-and-focus)
  (define-key evil-normal-state-map (kbd "C-w v") 'evil-window-vsplit-and-focus)

  ;; abbrevs
  (setq abbrev-file-name (concat user-emacs-directory "abbrevs.el"))
  (setq save-abbrevs 'silently)
  (quietly-read-abbrev-file)
  (setq-default abbrev-mode t)

  (defun sort-words (reverse beg end)
    "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
    (interactive "*P\nr")
    (sort-regexp-fields reverse "\\w+" "\\&" beg end))

  ;; ruby mode
  (setq enh-ruby-program "/usr/bin/ruby")

  ;; js2-mode
  (defun delete-tern-process ()
    (interactive)
    (delete-process "Tern"))

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
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

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

  (setq global-mode-string nil)
  (setq mode-require-final-newline 'visit-save)

  (global-evil-mc-mode 1)
  (setq scroll-margin 1)
  (global-company-mode)
  (global-linum-mode 1)
  (linum-relative-mode 1)
  (global-undo-tree-mode 1)
  (global-hl-line-mode -1)
  (semantic-mode 1)
  (smartparens-global-mode)
  (global-subword-mode +1)
  (setq powerline-default-separator 'arrow)
  (setq auto-save-timeout 1)
  (indent-guide-global-mode 1)
  (flycheck-pos-tip-mode 1))

(provide 'spacemacs-custom-user-config)
