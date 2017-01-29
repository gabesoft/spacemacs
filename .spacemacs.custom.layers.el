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
             colors-colorize-identifiers t
             colors-enable-nyan-cat-progress-bar nil)

     dired-hacks

     (elm :variables
          elm-reactor-port "8000"
          elm-reactor-address "127.0.0.1"
          elm-format-on-save t)
     emacs-lisp
     evil-commentary
     evil-mc-extras

     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)

     gabesoft-solarized-theme
     (git :variables
          git-magit-status-fullscreen nil
          git-enable-github-support t
          git-enable-magit-svn-plugin t
          git-gutter-use-fringe t)

     (haskell :variables
              haskell-enable-hindent-style "johan-tibell"
              haskell-process-type 'stack-ghci
              haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=intero")
              haskell-completion-backend 'intero
              haskell-enable-ghci-ng-support nil
              haskell-enable-ghc-mod-support nil
              haskell-enable-shm-support nil)

     (html :variables
           web-mode-code-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2)

     (ibuffer :variables ibuffer-group-buffers-by 'projects)

     java
     javascript
     (markdown :variables
               markdown-command "multimarkdown")
     nginx
     nixos

     (org :variables
          org-enable-github-support t
          org-src-tab-acts-natively t)

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

     shell-scripts

     syntax-checking
     spell-checking
     typescript

     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(company-ghci
                                      creamsody-theme
                                      darktooth-theme
                                      nodejs-repl
                                      tern-auto-complete)

   ;; List of excluded packages
   dotspacemacs-excluded-packages '(evil-escape nyan iswitchb)
   ))

(provide 'spacemacs-custom-layers)
