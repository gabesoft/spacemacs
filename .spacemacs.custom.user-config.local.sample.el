
(defun dotspacemacs/user-config-local ()
  (defun set-default-font-to-monaco ()
    "Sets the default font to Monaco 10"
    (interactive)
    (set-default-font "Monaco 10"))

  (defun setup-js2-mode-indent(indent)
    (setq-default
     js2-basic-offset indent
     js-indent-level indent
     jsx-indent-level indent
     js-switch-indent-offset indent))

  (defun setup-web-mode-indent(indent)
    (setq-default
     css-indent-offset indent
     web-mode-code-indent-offset indent
     web-mode-css-indent-offset indent
     web-mode-markup-indent-offset indent
     web-mode-attr-indent-offset indent))

  ;; (setq hindent-process-path (concat user-home-directory ".local/bin/hindent"))

  ;; path
  ;; (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

  (add-hook 'spacemacs-post-theme-change-hook
            'set-font-camingo-small)

  (gabesoft-solarized-enable-dark)

  ;; mac - remap command to ctrl
  (setq mac-command-modifier 'control)

  ;; scrolling
  (setq hscroll-step 1)
  (setq scroll-step 1)
  (setq scroll-conservatively 1000)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-follow-mouse 't)

  ;; evil-mc
  (setq evil-mc-enable-bar-cursor nil)

  ;; javascript setup
  (setq prettier-js-command (concat user-home-directory "quip/bin/prettier"))

  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'setup-tide-mode)

  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'setup-tide-mode)

  ;; js2-mode settings for improving performance in large files
  (setq js2-idle-timer-delay 2)
  (setq blink-matching-paren nil)

  ;; needed to avoid typing slowness in RJSX mode
  (add-hook 'rjsx-mode-hook
            (lambda() (setq-local intent-line-function
                                  'js-jsx-indent-line)))

  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'setup-tide-mode)

  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode  "=" 'prettier-js)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "=" 'prettier-js)
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode  "gd" 'tide-jump-to-definition)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "gd" 'tide-jump-to-definition)
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode  "gi" 'tide-jump-to-implementation)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "gi" 'tide-jump-to-implementation)
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode  "gb" 'tide-jump-back)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "gb" 'tide-jump-back)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "dd" 'tide-documentation-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode  "dd" 'tide-documentation-at-point)
  (setup-js2-mode-indent 4)
  (setup-web-mode-indent 4)

  ;; workaround for c-context-line-break being bound to Enter in js-mode
  (add-hook 'js2-mode-hook (lambda() (setq c-block-comment-start-regexp "/\\*")))
  (add-hook 'rjsx-mode-hook (lambda() (setq c-block-comment-start-regexp "/\\*")))

  ;; transparent titlebar on mac
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; emacs-plus settings (mac-os)
  ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (add-to-list 'default-frame-alist '(ns-appearance . light))

  ;; disable spacelpa if necessary
  ;; (setq configuration-layer-elpa-archives '(("melpa" . "melpa.org/packages/")
  ;;                                           ("org" . "orgmode.org/elpa/")
  ;;                                           ("gnu" . "elpa.gnu.org/packages/")))
  )

(message "user-config.local loaded")

(provide 'spacemacs-custom-user-config-local)

;; (require 'spacemacs-custom-user-config-local)