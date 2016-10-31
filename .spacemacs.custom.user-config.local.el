
(defun dotspacemacs/user-config-local ()
  (defun set-default-font-to-monaco ()
    "Sets the default font to Monaco 10"
    (interactive)
    (set-default-font "Monaco 10"))
  )

(provide 'spacemacs-custom-user-config-local)

;; (require 'spacemacs-custom-user-config-local)