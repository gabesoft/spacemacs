(setq evil-easymotion-packages '(evil-easymotion))

(defun evil-easymotion/init-evil-easymotion ()
  (use-package evil-easymotion
    :init
    (progn
      (evilem-default-keybindings "g"))))
