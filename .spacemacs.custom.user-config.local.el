
(defun dotspacemacs/user-config-local ()
  (defun set-default-font-to-monaco ()
    "Sets the default font to Monaco 10"
    (interactive)
    (set-default-font "Monaco 10"))
  (set-default-font "Monaco 10")

  ;; fonts
  (defun set-custom-font-local (font)
    (when (member font (font-family-list))
      (set-face-attribute
       'default nil
       :stipple nil
       :height 110
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

  (defun set-font-local-camingo ()
    (interactive)
    (set-custom-font-local "CamingoCode"))

  (defun set-font-local-dejavu ()
    (interactive)
    (set-custom-font-local "DejaVu Sans Mono"))

  (defun set-font-local-monaco ()
    (interactive)
    (set-custom-font-local "Monaco"))

  (defun set-font-local-monacoB2 ()
    (interactive)
    (set-custom-font-local "MonacoB2"))

  (defun set-font-local-input ()
    (interactive)
    (set-custom-font-local "Input Mono"))

  (defun set-font-local-monacoB2powerline ()
    (interactive)

    (set-custom-font-local "MonacoB2 for Powerline"))
  )

(provide 'spacemacs-custom-user-config-local)

;; (require 'spacemacs-custom-user-config-local)