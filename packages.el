;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! all-the-icons-ivy-rich)
(package! ivy-posframe)
(package! all-the-icons-dired)
(package! bm)
(package! google-translate)
(package! vterm-toggle)
(package! rainbow-mode)
(package! heaven-and-hell)
(package! nyan-mode)
(package! wakatime-mode)
(package! indent-guide)
;;; Visual
(package! centaur-tabs)
;;; Completion
;; (package! company-tabnine)
;; (package! copilot :recipe (:host github :repo "zerolfx/copilot.el"))
;; (package! copilot :recipe (:host github :repo "fkr-0/flight-attendant.el"))
;; (package! flight-attendant :recipe (:host github :repo "fkr-0/flight-attendant.el"))
;; required for copilot
(package! editorconfig)
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("dist" "copilot.el")))

(package! company-posframe)
;;; Lsp
;; (package! lsp-mode)
(package! lsp-ui)
;; (package! lsp-dart :recipe (:host github :repo "emacs-lsp/lsp-dart" :pin "b99f743302a4e77b1e149bbb1b325b6f0bfc38ad"))
;; (package! lsp-dart :pin "4d28d13d8b468bfb8f992da0ea450269d672562c")
(package! lsp-dart :recipe (:host github :repo "emacs-lsp/lsp-dart"))
;; (package! lsp-sonarlint)
;; (package! exec-path-from-shell)


;;; JS/TS
(package! typescript-mode)
(package! npm)
(package! ng2-mode)
(package! nodejs-repl)

;;; Treesitter
;; (package! tree-sitter :pin "48b06796a3b2e76ce004972d929de38146eafaa0" :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter"))
(package! tree-sitter)
(package! tree-sitter-langs)
(package! msgu :recipe (:host github :repo "jcs-elpa/msgu"))
(package! ts-docstr
  :recipe (:host github :repo "emacs-vs/ts-docstr" :files (:defaults "langs/*.el")))
;; (package! tree-sitter
;;   :pin "c7a1c34549cad41a3618c6f17e0e9dabd3e98fe1")
;; (package! tree-sitter-langs
;;   :pin "e7b8db7c4006c04a4bc1fc6865ec31f223843192")
(package! tree-edit)
(package! evil-tree-edit)

;;; Languages
;;;; Flutter
(package! flutter)
(package! dart-mode)
;;;; CLojure
(package! cider)

;;;; Lua
(package! lua-mode)


;;; Templates
;;;; Markdown
(package! grip-mode)
;;;; Jinja
(package! jinja2-mode)
;;;; Python
(package! pipenv)
(package! python-mode)
;; (package! pyenv-mode)
(package! lsp-python-ms)
(package! lsp-pyright)
(package! lsp-jedi)

;;;; Go
(package! go-playground)
;;;; Other
(package! web-mode)
(package! forge)
(package! evil-leader)
(package! websocket)
(package! restclient)
(package! evil-matchit)
;;;; Vue volar lsp
(package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
;;;; Documentation
(package! zeal-at-point)

;;; Performance benchmark/startup
(package! explain-pause-mode :recipe (:host github :repo "lastquestion/explain-pause-mode"))
(package! benchmark-init)

;;; Compile
;; (package! compile-eslint :recipe (:host github :repo "Fuco1/compile-eslint" :files ("compile-eslint.el")))
;;; Markup
(package! pug-mode)
(package! auto-rename-tag)

;;; Spelling
;; (package! wucuo)
(package! spell-fu :recipe (:host gitlab :repo "ideasman42/emacs-spell-fu"))

;;; Infrastucture
(package! docker-compose-mode)
(package! dockerfile-mode)
(package! jenkinsfile-mode)
;; (package! company-nginx)
(package! nginx-mode)
(package! kubernetes)
(package! kubernetes-evil)
(package! k8s-mode)



(package! emmet-mode)
(package! turbo-log :recipe (:host github :repo "artawower/turbo-log"))
;; (package! turbo-log :recipe (:host github :repo "artawower/turbo-log" :branch "enchancement/jump-inserting"))
(package! dap-mode)
(package! reverse-im)
(package! prettier)


;;; Undo
(package! vundo)
(package! undo-fu-session)
(package! undo-tree :disable t)
;; (unpin! undo-tree)
;; (package! undo-tree)

(package! package-lint)
(package! package-build)
;;; GIT
;; (package! blamer :recipe (:host github :repo "artawower/blamer.el" :branch "enhancement/truncated-lines"))
(package! blamer)
(package! sideline :recipe (:host github :repo "emacs-sideline/sideline"))
(package! sideline-blame :recipe (:host github :repo "emacs-sideline/sideline-blame"))
(package! sideline-lsp)
(package! sideline-flycheck)
(package! gist)
;; (package! blamer :recipe (:host github :repo "artawower/blamer.el" :branch "enhancement/company-integration"))

(package! git-messenger)

;;; Themes
(package! atom-one-dark-theme)
(package! uwu-theme
   :recipe (:host github :repo "kborling/uwu-theme"))
(package! auto-dark)
;; (package! ewal)
;; (package! ewal-doom-themes)

;;; ~UNCATEGORIZED YET
(package! hydra)
;; (package! company-box)
(package! quicktype :recipe (:host github :repo "artawower/quicktype.el"))
(package! origami)
;; (package! ivy
;;   :pin "487e97a94a49ededc2485d845cc912a893e8cc72"
;;   :recipe (:host github :repo "abo-abo/swiper"))

;; TMP
;; Not its not so usefull like company mode. But performance is totally better!
;; (package! corfu)
;; (package! orderless)

(package! outline-minor-faces)

;;; Tools
(package! string-inflection) ;; PASCAL_CASE -> camelCase -> kebab-case
(package! aggressive-indent)

;;;; Brackets
(package! autopair :recipe (:host github :repo "joaotavora/autopair")) ;; Autorpair colors
(package! paren-face)
;;
;;; Org mode
;; (package! polymode)
;; (package! poly-org)
(package! ox-gfm)
(package! ob-dart)
(package! oauth2)
(package! org-caldav)
(package! org-superstar)
(unpin! org-roam)
(package! web-roam
  :recipe (:host github :repo "artawower/web-roam.el"))
(package! org-roam)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-fancy-priorities)
;; (package! org-modern)
;;;; Org babel
(package! ob-restclient)
(package! ob-async)
;;; Reading
;;;; Rss
(package! elfeed-score)
;;;; Pocket
(package! pocket-reader)
;;; Agenda

;;; Collaboration
(package! floobits)
;;; Temporary

(package! secret-mode :recipe (:host github :repo "bkaestner/secret-mode.el"))
(package! ranger)
(package! filetree)
(package! svg-tag-mode)

;;; Not a programming
;;;; Messanging
(package! telega)
;; (package! telega :recipe (:host github :repo "zevlg/telega.el" :branch "release-0.8.0"))
;;; Unused

;; (package! dumb-jump) -- really dumb
;; (package! code-review)

(package! dirvish) ;; -- so luggy
;; (package! vi-tilde-fringe :disable t)
;; (package! multi-vterm)
;; (package! pippel)
;; (package! org-yt :recipe (:host github :repo "TobiasZawada/org-yt")) Youtrack. Awfully.
;; (package! youtrack :recipe (:host github :repo "jaseemabid/elisp"))

;; (package! lsp-ivy)
;; (package! doct)
;; (package! org-super-agenda)
;; (package! org-fancy-priorities) ;; Nice icons..but can be replaced by pretty symbols alist
;; (package! tui :recipe (:host github :repo "ebpa/tui.el"))
;; (package! polymode)
;; (package! icons-in-terminal :recipe (:host github :repo "seagle0128/icons-in-terminal.el"))
;; (package! poly-vue :recipe (:host github :repo "akirak/poly-vue"))
;; (package! org-sticky-header) ;; Just unsused

;; (package! beacon)
;; (package! dash-docs)
;; (package! shikimori :recipe (:host nil :repo "https://git.sr.ht/~akagi/shikimori.el"))

;; (package! xwidgets-reuse)
(package! lsp-grammarly)

;; (package! modus-themes)

;; (package! counsel-dash)

;; (package! live-py-mode)

;;; Disable doom crap
(package! evil-goggles :disable t)
