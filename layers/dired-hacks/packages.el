;;; package.el --- dired-hacks layer for spacemacs

;;; Commentary:

;;; Code:

(setq dired-hacks-packages '((dired-hacks :location local)))

(load-file (concat user-emacs-directory "layers/dired-hacks/local/dash.el"))
(load-file (concat user-emacs-directory "layers/dired-hacks/local/dired-hacks-utils.el"))
(load-file (concat user-emacs-directory "layers/dired-hacks/local/dired-rainbow.el"))

(defun dired-hacks-home-directory ()
  "Open the a dired buffer in user's home directory."
  (interactive)
  (find-alternate-file user-home-directory))

(defun dired-hacks-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun dired-hacks-up-directory (&optional other-window)
  "Run Dired on parent directory of current directory."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (orig (current-buffer))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (kill-buffer orig)
          (dired up)
          (dired-goto-file dir)))))

(defun dired-hacks/init-dired-hacks ()
  "Initialize the dired-hacks layer."
  (progn
    (define-key evil-normal-state-map (kbd "-") 'dired-jump)
    (add-hook 'dired-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map
                  (kbd "gg") 'evil-goto-first-line)
                (define-key evil-normal-state-local-map
                  (kbd "G") 'evil-goto-line)
                (define-key evil-normal-state-local-map
                  (kbd "u") 'dired-undo)
                (define-key evil-normal-state-local-map
                  (kbd "-") 'dired-hacks-up-directory)
                (define-key evil-normal-state-local-map
                  (kbd "^") 'dired-hacks-up-directory)
                (define-key evil-normal-state-local-map
                  (kbd "I") 'dired-hacks-dotfiles-toggle)
                (define-key evil-normal-state-local-map
                  (kbd "J") 'dired-next-subdir)
                (define-key evil-normal-state-local-map
                  (kbd "K") 'dired-prev-subdir)
                (define-key evil-normal-state-local-map
                  (kbd "C-k") 'dired-do-kill-lines)
                (define-key evil-normal-state-local-map
                  (kbd "~") 'dired-hacks-home-directory)
                (define-key evil-normal-state-local-map
                  (kbd "j") 'dired-next-line)
                (define-key evil-normal-state-local-map
                  (kbd "k") 'dired-previous-line)
                (define-key evil-normal-state-local-map
                  (kbd "RET") 'dired-find-alternate-file)))

    (use-package dired-rainbow
      :init
      (progn
        (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
        (dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))
        (dired-rainbow-define js "#859900" ("js" "jsx" "json"))
        (dired-rainbow-define css "chocolate" ("css" "sass" "scss"))

        (dired-rainbow-define el "plum3" ("el"))
        (dired-rainbow-define org "#6c71c4" ("org"))

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

        (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")))))
