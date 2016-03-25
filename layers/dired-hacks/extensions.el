;;; extensions.el --- dired-hacks layer for spacemacs

;;; Commentary:

;;; Code:

(setq dired-hacks-post-extensions '(dired-hacks))

(load-file (concat user-emacs-directory "layers/dired-hacks/extensions/dash.el"))
(load-file (concat user-emacs-directory "layers/dired-hacks/extensions/dired-hacks-utils.el"))
(load-file (concat user-emacs-directory "layers/dired-hacks/extensions/dired-rainbow.el"))
(load-file (concat user-emacs-directory "layers/dired-hacks/extensions/dired-subtree.el"))

(defun dired-hacks/init-dired-hacks ()
  "Initialize the dired-hacks layer."
  (progn
    (use-package dired-rainbow
      :init
      (progn
        (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
        (dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))
        (dired-rainbow-define js "#859900" ("js" "jsx" "json"))
        (dired-rainbow-define css "chocolate" ("css" "sass" "scss"))

        (dired-rainbow-define el "plum3" ("el"))

        (dired-rainbow-define document "#fce94f" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub"))
        (dired-rainbow-define excel "#3465a4" ("xlsx"))
        (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))

        (dired-rainbow-define log "#c17d11" ("log"))
        (dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "h" "java" "pl" "rb" "R" "php"))

        (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
        (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
        (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
        (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))

        (dired-rainbow-define makefile
                              "SkyBlue2"
                              "Makefile")

        (dired-rainbow-define gitignore (:inherit default :italic t) ".gitignore")
        (dired-rainbow-define gitconfig (:inherit default :italic t) ".gitconfig")

        (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")))

    (use-package dired-subtree
      :init
      (setq-default dired-subtree-use-backgrounds nil)
      (bind-keys :map dired-mode-map
                 :prefix ","
                 :prefix-map dired-subtree-map
                 :prefix-docstring "Dired subtree map."
                 ("j" . dired-subtree-insert)
                 ("C-/" . dired-subtree-apply-filter)
                 ("k" . dired-subtree-remove)
                 ("n" . dired-subtree-next-sibling)
                 ("p" . dired-subtree-previous-sibling)
                 ("u" . dired-subtree-up)
                 ("d" . dired-subtree-down)
                 ("a" . dired-subtree-beginning)
                 ("e" . dired-subtree-end)
                 ("c" . dired-subtree-cycle)
                 ("C-m" . dired-subtree-mark-subtree)
                 ("C-u" . dired-subtree-unmark-subtree)
                 ("C-o C-f" . dired-subtree-only-this-file)
                 ("C-o C-d" . dired-subtree-only-this-directory)))))
