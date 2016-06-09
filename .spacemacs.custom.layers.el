;;; .spacemacs.custom.layers --- Layers configuration

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(

     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-help-tooltip nil
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)

     (colors :variables
             enable-rainbow-identifiers t
             colors-enable-nyan-cat-progress-bar nil)

     dired-hacks

     (elm :variables
          elm-reactor-port "8000"
          elm-reactor-address "127.0.0.1"
          elm-format-on-save t)
     emacs-lisp
     evil-commentary
     evil-easymotion
     evil-mc

     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)

     (eyebrowse :variables eyebrowse-display-help t)

     (git :variables
          git-magit-status-fullscreen nil
          git-enable-github-support t
          git-enable-magit-svn-plugin t
          git-gutter-use-fringe t)

     (haskell :variables
              haskell-enable-hindent-style "fundamental")
     (html :variables
           web-mode-code-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2)

     (ibuffer :variables ibuffer-group-buffers-by 'projects)

     java
     javascript
     markdown

     (org :variables org-enable-github-support t)

     osx
     react
     restclient

     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-enable-enh-ruby-mode t
           ruby-enable-ruby-on-rails-support t)
     ruby-on-rails

     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-enable-eldoc nil)

     (shell :variables
            shell-default-height 30
            shell-default-shell 'multi-term
            shell-enable-smart-eshell t
            shell-default-position 'bottom)

     smartchr
     syntax-checking
     spell-checking
     themes-megapack
     unimpaired

     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(tern-auto-complete nodejs-repl)

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-search-highlight-persist evil-escape hl-todo nyan iswitchb evil-jumper org-bullets)

   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(provide 'spacemacs-custom-layers)
