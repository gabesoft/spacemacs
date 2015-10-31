;;; packages.el --- smartchr layer packages file for spacemacs

;;; Commentary:

;;; Code:

(setq smartchr-post-extensions '(smartchr))

(load-file (concat user-emacs-directory "layers/smartchr/extensions/smartchr.el"))

(setq
 smartchr-css-key-map
 '((":" ": " ":")
   ("&" "&:" "&." "&" "&&")
   ("+" "+" " + ")
   ("-" "-" " - ")
   ("," ", " ","))

 smartchr-js-key-map
 '(("=" " = " " === " " => " " == " "=")
   ("!" "!" " !== " " != ")
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
 '(("=" "=" " = " " => " " == ")
   (">" ">" "%>")
   ("<" "<" "<%" "<%=")
   ("," ", " ","))

 smartchr-java-key-map
 '(("+" " + " "++" " += " "+")
   ("<" " < " " <= "  " << " "<")
   (">" " > " " >= " " >> " ">")
   ("-" " - " "-1" " -= " "--" "-")
   ("=" " = " " == " "=")
   ("|" " || " " | " "|")
   ("&" " && " " & " "&")
   ("!" "!" " != ")
   ("%" " % " "%")
   ("/" " / " "/")
   ("*" " * " "*")
   ("," ", " ","))

 )

(defun smartchr/init-smartchr ()
  "Initialize the smartchr layer."
  (use-package smartchr
    :config
    (progn
      (add-hook 'js2-mode-hook #'smartchr/init-js2-mode)
      (add-hook 'enh-ruby-mode-hook #'smartchr/init-ruby-mode)
      (add-hook 'web-mode-hook #'smartchr/init-web-mode)
      (add-hook 'java-mode-hook #'smartchr/init-java-mode))))

(defun smartchr/init-mode (mode-map)
  "Sets up the keys defined by MODE-MAP."
  (mapcar (lambda (m)
            (define-key evil-insert-state-local-map
              (kbd (car m))
              (smartchr (cdr m))))
          mode-map))

(defun smartchr/undo-mode (mode-map)
  "Undo the smartchr key maps defined in MODE-MAP."
  (mapcar (lambda (m)
            (define-key evil-insert-state-local-map
              (kbd (car m)) nil))
          mode-map))

(defun smartchr/undo-js2-mode ()
  "Undo the keys for js2 mode."
  (interactive)
  (smartchr/undo-mode smartchr-js-key-map))

(defun smartchr/init-js2-mode ()
  "Set up the keys for js2 mode."
  (interactive)
  (smartchr/init-mode smartchr-js-key-map))

(defun smartchr/undo-ruby-mode ()
  "Undo up the keys for ruby mode."
  (interactive)
  (smartchr/undo-mode smartchr-ruby-key-map))

(defun smartchr/init-ruby-mode ()
  "Set up the keys for ruby mode."
  (interactive)
  (smartchr/init-mode smartchr-ruby-key-map))

(defun smartchr/undo-web-mode ()
  "Undo up the keys for web mode."
  (interactive)
  (cond ((string= web-mode-engine "erb")
         (smartchr/undo-mode smartchr-eruby-key-map))))

(defun smartchr/init-web-mode ()
  "Set up the keys for web mode."
  (interactive)
  (cond ((string= web-mode-engine "erb")
         (smartchr/init-mode smartchr-eruby-key-map))))

(defun smartchr/undo-css-mode ()
  "Undo up the keys for css mode."
  (interactive)
  (smartchr/undo-mode smartchr-css-key-map))

(defun smartchr/init-css-mode ()
  "Set up the keys for css mode."
  (interactive)
  (smartchr/init-mode smartchr-css-key-map))

(defun smartchr/undo-java-mode ()
  "Undo up the keys for java mode."
  (interactive)
  (smartchr/undo-mode smartchr-java-key-map))

(defun smartchr/init-java-mode ()
  "Set up the keys for java mode."
  (interactive)
  (smartchr/init-mode smartchr-java-key-map))

;;; packages.el ends here
