
(defun dotspacemacs/user-config-local ()
  (defun set-default-font-to-monaco ()
    "Sets the default font to Monaco 10"
    (interactive)
    (set-default-font "Monaco 10"))

  ;; (setq hindent-process-path (concat user-home-directory ".local/bin/hindent"))

  (add-hook 'spacemacs-post-theme-change-hook
            'set-font-camingo-small)

  )

(provide 'spacemacs-custom-user-config-local)

;; (require 'spacemacs-custom-user-config-local)
