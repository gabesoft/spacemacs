(setq evil-easymotion-packages '(evil-easymotion))

(defun evil-easymotion/init-evil-easymotion ()
  :init
  :defer t
  (progn
    (evilem-default-keybindings "g")
    (let ((prefix "g"))
      (setq avy-keys (append
                      (number-sequence ?a ?z)
                      (number-sequence ?A ?Z)
                      (list ?, ?. ?' ?-)))
      (setq avy-style 'de-bruijn)
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
      (define-key evil-motion-state-map (kbd "gg") 'evil-goto-first-line))))
