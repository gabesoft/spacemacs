(setq evil-easymotion-packages '(evil-easymotion avy))

(defun evil-easymotion/init-evil-easymotion ()
  (progn
    (use-package avy
      :init
      (setq avy-style 'de-bruijn)
      (setq avy-keys (append
                      (number-sequence ?a ?z)
                      (number-sequence ?A ?Z)
                      (list ?, ?. ?' ?-)))
      (define-key evil-motion-state-map (kbd "gj") 'avy-goto-line-below)
      (define-key evil-motion-state-map (kbd "gk") 'avy-goto-line-above))
    (use-package evil-easymotion
      :init
      (progn
        (evilem-default-keybindings "g")
        (let ((prefix "g"))
          (evilem-define (kbd (concat prefix " w")) 'evil-forward-word-begin)
          (evilem-define (kbd (concat prefix " W")) 'evil-forward-WORD-begin)
          (evilem-define (kbd (concat prefix " e")) 'evil-forward-word-end)
          (evilem-define (kbd (concat prefix " E")) 'evil-forward-WORD-end)
          (evilem-define (kbd (concat prefix " b")) 'evil-backward-word-begin)
          (evilem-define (kbd (concat prefix " B")) 'evil-backward-WORD-begin)
          (define-key evil-motion-state-map (kbd (concat prefix " ge")) nil)
          (define-key evil-motion-state-map (kbd (concat prefix " gE")) nil)
          (define-key evil-motion-state-map (kbd (concat prefix " g j")) nil)
          (define-key evil-motion-state-map (kbd (concat prefix " g k")) nil)
          (define-key evil-motion-state-map (kbd "gg") 'evil-goto-first-line))))))
