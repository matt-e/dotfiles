;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/spacemacs/layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     lua
     csv
     python
     sql
     html
     yaml
     go
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     ; themes-megapack
     ;; gtags
     ;; cscope
     ;; (c-c++ :variables
     ;;        c-c++-default-mode-for-headers 'c++-mode
     ;;        c-c++-enable-clang-support t)
     java
     ruby
     ruby-on-rails
     ;; python
     rust
     ;; yaml
     ;; sql
     ;; haskell
     javascript
     ;; eyebrowse
     my-host-config
     ;; lua
     ;; office
     ;; research
     ;; dockerfile
     ;; dash
     ;; personal
     ;; rtags
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(
                                      visual-fill-column
                                      org-caldav
                                      web-mode
                                      gradle-mode
                                      groovy-mode
                                      groovy-imports
                                      all-the-icons
                                      yascroll
                                      dash-at-point
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(misterioso
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (setq-default ruby-version-manager 'rbenv)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (evil-leader/set-key "," 'org-capture)
  (setq visual-fill-column-width 120)
  (add-hook 'visual-line-mode-hook (lambda ()
                                     (visual-fill-column-mode)))
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(c-default-style
   (quote
    ((c++-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(evil-want-Y-yank-to-eol t)
 '(nxml-sexp-element-flag t)
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-files
   (quote
    ("~/Documents/org/" "~/Dropbox/org/" "~/Documents/org/scheduled/" "~/Documents/org/cm/")))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-todo-ignore-timestamp (quote all))
 '(org-capture-templates
   (quote
    (("j" "Journal" entry
      (file+datetree "~/Documents/org/journal.org")
      "* %?
%t
%i")
     ("r" "Review notes" entry
      (file+headline "~/Documents/org/review-notes.org" "Review notes")
      "* %t %?
       %t
       %i")
     ("p" "Project ideas" entry
      (file+headline "~/Documents/org/project-ideas.org" "Ideas")
      "* %?
        %i")
     ("t" "Todo" entry
      (file+headline "~/Documents/org/unfiled.org" "Unfiled tasks")
      "* TODO %?
          %t
          %i")
     ("n" "Note" entry
      (file+headline "~/Documents/org/notes.org" "Notes")
      "* %?
  %t
  %i")
     ("s" "Shopping list" entry
      (file+headline "~/Dropbox/org/shopping.org" "Shopping")
      "* %?
%i")
     ("l" "TIL" entry
      (file+datetree "~/Documents/org/til.org")
      "* %?
%t
%i"))))
 '(org-directory "~/Documents/org")
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 5))))
 '(org-refile-use-outline-path nil)
 '(org-todo-interpretation (quote type))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(@!)" "|" "DONE(d!)" "DEFERRED" "CANCELLED(c@)"))))
 '(package-selected-packages
   (quote
    (company-restclient restclient-helm restclient yascroll protobuf-mode scad-preview company-terraform terraform-mode all-the-icons csv-mode dockerfile-mode salt-mode scad-mode pcache company-web web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode web-completion-data groovy-imports go-guru go-eldoc company-go go-mode meghanada org-caldav groovy-mode gradle-mode markdown-mode smartparens evil helm helm-core async dash visual-fill-column zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode xterm-color web-beautify unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toml-mode tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme rvm ruby-tools ruby-test-mode rubocop rtags rspec-mode robe reverse-theme rbenv railscasts-theme racer pyvenv pytest pyenv-mode py-isort purple-haze-theme projectile-rails rake inflections professional-theme planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme orgit organic-green-theme org-ref pdf-tools ivy tablist org-projectile org-present org-pomodoro alert log4e gntp org-download omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mwim mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme minitest minimal-theme material-theme majapahit-theme magit-gitflow madhat2r-theme lush-theme lua-mode livid-mode skewer-mode simple-httpd live-py-mode light-soap-theme key-chord json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme intero interleave inkpot-theme hy-mode htmlize hlint-refactor hindent heroku-theme hemisu-theme helm-pydoc helm-hoogle helm-gtags helm-gitignore helm-dash helm-cscope xcscope helm-company helm-c-yasnippet helm-bibtex biblio biblio-core hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter ggtags gandalf-theme fuzzy flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip pos-tip flycheck-haskell flycheck flatui-theme flatland-theme firebelly-theme feature-mode farmhouse-theme evil-magit magit magit-popup git-commit with-editor espresso-theme eshell-z eshell-prompt-extras esh-help ebib parsebib seq dracula-theme django-theme disaster diff-hl dash-at-point darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-tern dash-functional tern company-statistics company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company-c-headers company-anaconda company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmm-mode cmake-mode clues-theme clang-format chruby cherry-blossom-theme cargo rust-mode busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet yasnippet auto-dictionary apropospriate-theme anti-zenburn-theme anaconda-mode pythonic ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text mmm-mode markdown-toc macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio gh-md flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(reftex-default-bibliography (quote ("~/Dropbox/Papers/bibliography"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c++-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-files
   (quote
    ("~/Documents/org/" "~/Dropbox/org/" "~/Documents/org/scheduled/" "~/Documents/org/cm/")))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-todo-ignore-timestamp (quote all))
 '(org-capture-templates
   (quote
    (("j" "Journal" entry
      (file+datetree "~/Documents/org/journal.org")
      "* %?
%t
%i")
     ("r" "Review notes" entry
      (file+headline "~/Documents/org/review-notes.org" "Review notes")
      "* %t %?
       %t
       %i")
     ("p" "Project ideas" entry
      (file+headline "~/Documents/org/project-ideas.org" "Ideas")
      "* %?
        %i")
     ("t" "Todo" entry
      (file+headline "~/Documents/org/unfiled.org" "Unfiled tasks")
      "* TODO %?
          %t
          %i")
     ("n" "Note" entry
      (file+headline "~/Documents/org/notes.org" "Notes")
      "* %?
  %t
  %i")
     ("s" "Shopping list" entry
      (file+headline "~/Dropbox/org/shopping.org" "Shopping")
      "* %?
%i")
     ("l" "TIL" entry
      (file+datetree "~/Documents/org/til.org")
      "* %?
%t
%i"))))
 '(org-directory "~/Documents/org")
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 5))))
 '(org-refile-use-outline-path nil)
 '(org-todo-interpretation (quote type))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(@!)" "|" "DONE(d!)" "DEFERRED" "CANCELLED(c@)"))))
 '(package-selected-packages
   (quote
    (groovy-imports pcache ensime sbt-mode scala-mode symon string-inflection ruby-refactor password-generator org-brain helm-purpose window-purpose imenu-list evil-org evil-lion editorconfig browse-at-remote meghanada org-caldav groovy-mode gradle-mode markdown-mode smartparens evil helm helm-core async dash visual-fill-column zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode xterm-color web-beautify unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toml-mode tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme rvm ruby-tools ruby-test-mode rubocop rtags rspec-mode robe reverse-theme rbenv railscasts-theme racer pyvenv pytest pyenv-mode py-isort purple-haze-theme projectile-rails rake inflections professional-theme planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme orgit organic-green-theme org-ref pdf-tools ivy tablist org-projectile org-present org-pomodoro alert log4e gntp org-download omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mwim mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme minitest minimal-theme material-theme majapahit-theme magit-gitflow madhat2r-theme lush-theme lua-mode livid-mode skewer-mode simple-httpd live-py-mode light-soap-theme key-chord json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme intero interleave inkpot-theme hy-mode htmlize hlint-refactor hindent heroku-theme hemisu-theme helm-pydoc helm-hoogle helm-gtags helm-gitignore helm-dash helm-cscope xcscope helm-company helm-c-yasnippet helm-bibtex biblio biblio-core hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter ggtags gandalf-theme fuzzy flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip pos-tip flycheck-haskell flycheck flatui-theme flatland-theme firebelly-theme feature-mode farmhouse-theme evil-magit magit magit-popup git-commit with-editor espresso-theme eshell-z eshell-prompt-extras esh-help ebib parsebib seq dracula-theme django-theme disaster diff-hl dash-at-point darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-tern dash-functional tern company-statistics company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company-c-headers company-anaconda company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmm-mode cmake-mode clues-theme clang-format chruby cherry-blossom-theme cargo rust-mode busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet yasnippet auto-dictionary apropospriate-theme anti-zenburn-theme anaconda-mode pythonic ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text mmm-mode markdown-toc macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio gh-md flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(reftex-default-bibliography (quote ("~/Dropbox/Papers/bibliography"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
