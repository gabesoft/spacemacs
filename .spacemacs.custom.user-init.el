;;; .spacemacs.custom.user-init.el --- User init

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; evil cursors
  (defvar spacemacs-evil-cursors
    '(("normal" "#268bd2" box)
      ("insert" "#859900" (bar . 2))
      ("emacs"  "#d33682" box)
      ("hybrid" "SkyBlue2" (bar . 2))
      ("replace" "#dc322f" (hbar . 2))
      ("evilified" "#cb4b16" box)
      ("visual" "#b58900" box)
      ("motion" "#2aa198" box)
      ("lisp"   "#6c71c4" box)
      ("iedit"  "chocolate" box)
      ("iedit-insert"  "#dc322f" (bar . 2)))
    "Colors assigned to evil states with cursor definitions.")

  ;; solarized settings
  (setq
   solarized-distinct-fringe-background nil
   solarized-use-variable-pitch nil
   solarized-use-less-bold nil
   solarized-use-more-italic t
   solarized-height-minus-1 1
   solarized-height-plus-1 1
   solarized-height-plus-2 1
   solarized-height-plus-3 1
   solarized-height-plus-4 1
   solarized-high-contrast-mode-line t
   solarized-emphasize-indicators nil)

  (setq tags-add-tables nil)

  ;; some major mode hooks need to be defined here rather than in user-config
  ;; this is due to dotspacemacs-auto-resume-layouts which opens layout buffers
  ;; after user-init but before user-config
  ;; https://github.com/syl20bnr/spacemacs/issues/3881

  (defun setup-haskell-mode ()
    "Sets up haskell mode. For customizing the template inserted by
`haskell-auto-insert-module-template' see
`haskell-auto-insert-module-format-string'"
    (progn
      (when (fboundp 'haskell-auto-insert-module-template)
        (haskell-auto-insert-module-template))
      (when (boundp 'haskell-mode-map)
        (define-key haskell-mode-map (kbd "C-c C-f") 'hindent-reformat-buffer))
      (when (fboundp 'speedbar-add-supported-extension)
        (speedbar-add-supported-extension ".hs"))
      (setq haskell-auto-insert-module-format-string "-- ^ \n\nmodule %s where\n\n")
      (setq flycheck-display-errors-function 'flycheck-display-errors-function)
      (flycheck-pos-tip-mode -1)
      (haskell-indentation-mode -1)
      (haskell-indent-mode 1)
      (add-hook 'auto-save-hook 'haskell-sort-imports)))

  (add-hook 'haskell-mode-hook 'setup-haskell-mode)

  (with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  )

(provide 'spacemacs-custom-user-init)
