(defun dotspacemacs/user-config-local ()

  (defun set-custom-font ()
    (interactive)
    (progn
      (set-frame-font "MonacoB2 for Powerline")
      (set-face-attribute 'default nil :height 120)))

  )


(provide 'spacemacs-custom-user-config-local)
