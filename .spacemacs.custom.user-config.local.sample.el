
(defun dotspacemacs/user-config-local ()
  (defun set-default-font-to-monaco ()
    "Sets the default font to Monaco 10"
    (interactive)
    (set-default-font "Monaco 10"))

  ;; (setq hindent-process-path (concat user-home-directory ".local/bin/hindent"))

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
  )

(message "user-config.local loaded")

(provide 'spacemacs-custom-user-config-local)

;; (require 'spacemacs-custom-user-config-local)
