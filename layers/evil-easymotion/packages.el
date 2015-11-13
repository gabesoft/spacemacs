(setq evil-easymotion-packages '(evil-easymotion))

(defun evil-easymotion/init-evil-easymotion ()
  (use-package evil-easymotion
    :init
    (progn
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
        (evilem-define (kbd (concat prefix " j")) 'next-line
                       (lambda ()
                         (setq evil-this-type 'line))
                       nil
                       ((temporary-goal-column (current-column))
                        (line-move-visual nil)))
        (evilem-define (kbd (concat prefix " k")) 'previous-line
                       (lambda ()
                         (setq evil-this-type 'line))
                       nil
                       ((temporary-goal-column (current-column))
                        (line-move-visual nil)))))))
