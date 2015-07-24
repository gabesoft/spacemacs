;;; packages.el --- smartchr layer packages file for spacemacs

;;; Commentary:

;;; Code:

(setq smartchr-post-extensions '(smartchr))

(load-file (concat user-emacs-directory "contrib/smartchr/extensions/smartchr.el"))

(setq
 smartchr-css-key-map
 '((":" ": " ":")
   ("&" "&:" "&." "&" "&&")
   ("+" "+" " + ")
   ("-" "-" " - ")
   ("," ", " ","))

 smartchr-js-key-map
 '(("=" " = " " === " " == " "=")
   ("!" " !== " " != " "!")
   ("+" "+" " + ")
   ("-" "-" " - ")
   ("?" "?" " ? ")
   (":" ":" " : ")
   ("," ", " ",")
   ("&" " && " "&" " & ")
   ("|" " || " "|" " | "))

 smartchr-ruby-key-map
 '(("=" " = " " => " " == " "=")
   ("," ", " ",")
   ("!" "!" " != "))

 smartchr-eruby-key-map
 '(("=" " = " " => " " == " "=")
   (">" ">" "%>")
   ("<" "<" "<%" "<%=")
   ("," ", " ","))
 )

(defun smartchr/init-smartchr ()
  "Initialize the smartchr layer."
  (use-package smartchr
    :config
    (progn
      (add-hook 'js2-mode-hook #'smartchr/init-js2-mode)
      (add-hook 'enh-ruby-mode-hook #'smartchr/init-ruby-mode)
      (add-hook 'web-mode-hook #'smartchr/init-eruby-mode))))

(defun smartchr/init-mode (mode-map)
  "Sets up the keys defined by MODE-MAP."
  (mapcar (lambda (m)
            (define-key evil-insert-state-local-map
              (kbd (car m))
              (smartchr (cdr m)))) mode-map))

(defun smartchr/init-js2-mode ()
  "Set up the keys for js2 mode."
  (smartchr/init-mode smartchr-js-key-map))

(defun smartchr/init-ruby-mode ()
  "Set up the keys for ruby mode."
  (smartchr/init-mode smartchr-ruby-key-map))

(defun smartchr/init-eruby-mode ()
  "Set up the keys for ruby mode."
  (smartchr/init-mode smartchr-eruby-key-map))

(defun smartchr/init-css-mode ()
  "Set up the keys for ruby mode."
  (smartchr/init-mode smartchr-css-key-map))

;;; packages.el ends here










