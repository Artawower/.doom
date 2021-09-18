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
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; org mode
(package! org-superstar)
(package! ob-restclient)
(package! org-gcal)
(package! company-org-roam)
;; (package! org-roam)
(package! org-roam-server)
;; (package! org-pretty-tags) ;; ??


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  editor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; spellcheck
(package! wucuo)
(package! guess-language)

(package! tree-sitter)
(package! tree-sitter-langs)

;; (package! prism) ;; Colorized nested block [useless]
(package! ivy
  :pin "487e97a94a49ededc2485d845cc912a893e8cc72"
  :recipe (:host github :repo "abo-abo/swiper"))
;; themes
(package! atom-one-dark-theme)
(package! all-the-icons-ivy-rich)
(package! heaven-and-hell)
(package! nyan-mode)
(package! treemacs-all-the-icons)
;; common
(package! exec-path-from-shell)
(package! bm)
(package! google-translate)
(package! dogears
    :recipe (:host github :repo "alphapapa/dogears.el")
)


;; dired
(package! all-the-icons-dired)

;; Ivy
(package! ivy-posframe)

;; vim like navigation
(package! evil-leader)
(package! evil-easymotion)
(package! evil-matchit)

(package! wakatime-mode)

;; (package! origami) ; Package for good folding

;; ivy
(package! all-the-icons-ivy-rich)

;; indent
(package! indent-guide)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  languages/frameworks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AI completion
(package! company-tabnine)
(package! lsp-mode)
;; (package! company-web)
;; (package! company-posframe)

;; frontend
(package! ng2-mode)
(package! web-mode)
(package! prettier-js)

;; Golang
(package! company-go)
(package! go-eldoc)
(package! go-scratch)
(package! go-dlv)
(package! dap-mode)
(package! go-playground)

;; Python
(package! python-mode)
(package! lsp-pyright)
(package! lsp-python-ms)
(package! pipenv)
;; (package! pyenv-mode)
(package! pyimport)

(package! lua-mode)
(package! vue-mode)

;; (package! rust-mode)
(package! rustic)

;; Markup
(package! pug-mode)
(package! emmet-mode)
;; (package! ac-emmet) ;; autocomplete for emmet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(package! reverse-im)
;; (package! ranger) ;; TEST, nice but not so needfull
;; tools
(package! origami)
(package! restclient)
(package! floobits)

;; (package! nginx-mode)
;; (package! dockerfile-mode)
;; (package! docker-compose-mode)
;;
;; Database
;; (package! edbi)

;; Git
;; (package! forge) ;; remote access git
(package! git-messenger) ;; butiful popup
(package! pretty-hydra)

;; JSON exporter
(package! ox-json)

(when (featurep! :completion company)
  (package! company-tabnine))


(package! night-owl-theme)
(package! rebecca-theme)
(package! kaolin-themes)
