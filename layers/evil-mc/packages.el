(setq evil-mc-packages '(evil-mc))

(evil-define-local-var evil-mc-custom-paused nil
  "Paused functionality when there are multiple cursors active.")

(defun evil-mc-pause-smartchr-for-mode (mode)
  "Temporarily disables the smartchr keys for MODE."
  (let ((m-mode (if (atom mode) mode (car mode)))
        (s-mode (if (atom mode) mode (cdr mode))))
    (let ((init (intern (concat "smartchr/init-" (symbol-name s-mode))))
          (undo (intern (concat "smartchr/undo-" (symbol-name s-mode)))))
      (when (eq major-mode m-mode)
        (funcall undo)
        (push `(lambda () (,init)) evil-mc-custom-paused)))))

(defun evil-mc-before-cursors-setup-hook ()
  "Hook to run before any cursor is created.
Can be used to temporarily disable any functionality that doesn't
play well with `evil-mc'."
  (mapc 'evil-mc-pause-smartchr-for-mode
        '(web-mode
          js2-mode
          java-mode
          css-mode
          haskell-mode
          (react-mode . js2-mode)
          (enh-ruby-mode . ruby-mode)))
  (when (boundp whitespace-cleanup-disabled)
    (setq whitespace-cleanup-disabled t)
    (push (lambda () (setq whitespace-cleanup-disabled nil)) evil-mc-custom-paused)))

(defun evil-mc-after-cursors-teardown-hook ()
  "Hook to run after all cursors are deleted."
  (dolist (fn evil-mc-custom-paused) (funcall fn))
  (setq evil-mc-custom-paused nil))

(defvar evil-mc-mode-line-prefix "ⓜ"
  "Override of the default mode line string for `evil-mc-mode'.")

(defun evil-mc/init-evil-mc ()
  (use-package evil-mc
    :init
    (progn
      (add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)
      (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook))))
