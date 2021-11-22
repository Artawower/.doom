;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "darkawower"
      user-mail-address "artawower@mail.ru")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-moonlight)
(setq fancy-splash-image "/Users/darkawower/.doom.d/icons/I-am-doom.png")
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)
(setq display-line-numbers nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (use-package doom-deep-atom-one-dark)
(load! "~/.doom.d/private.el")

;;; Variables
;;;; Additional colors
(setq +m-color-main "#61AFEF"
      +m-color-secondary "red")

;;; My custom functions
;;;; Switch to first finded buffer
(defun switch-to-first-matching-buffer (regex)
  (switch-to-buffer (car (remove-if-not (apply-partially #'string-match-p regex)
                                        (mapcar #'buffer-name (buffer-list))))));;;

;;;; Maximize current buffer
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

;;; Transparent bg
(progn
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90))))

;;; Fonts

(set-frame-font "JetBrainsMono Nerd Font 15" nil t)
(custom-set-faces
 `(font-lock-comment-face ((t (:font "ChalkBoard SE" :italic t :height 1.0))))
 `(font-lock-string-face ((t (:italic t :height 1.0)))))
;; `(font-lock-string-face ((t (:font "ChalkBoard SE" :italic t :height 136)))))
;; `(doom-themes-treemacs-file-face ((t (:font "JetBrainsMono Nerd Font 14" :italic t))))
;; `(doom-themes-treemacs-file-face ((t (:inherit all-the-icons-ivy-rich-icon-face)))))


(defun correct-my-fringe (&optional ignore)
  (unless (eq fringe-mode '16)
    (fringe-mode '16)))

(add-hook 'after-init-hook #'correct-my-fringe)
(add-hook 'buffer-list-update-hook #'correct-my-fringe)
;; (set-frame-font "Fira Code 15" nil t)
;; (set-frame-font "Ligamonacop Nerd Font 15" nil t)


(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
                        (aref pat 0)
                        (nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
                                             0
                                             'compose-gstring-for-graphic)))))

;;; Core
(setq warning-minimum-level :emergency)
(setq read-process-output-max (* 1024 1024))
(setq-default left-margin-width 1 right-margin-width 2) ; Define new widths.
(set-window-buffer nil (current-buffer))
(setenv "zstd" "/usr/local/bin/zstd")



;; Spaces insted of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;; Completion
(use-package ivy
  :defer t
  :bind (:map ivy-mode-map ("C-<return>" . ivy-immediate-done)))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :after ivy
  :config)

(use-package counsel-projectile
  :defer 0.5)

(use-package all-the-icons-ivy-rich
  :defer 0.5)

(use-package all-the-icons-ivy-rich
  :after (all-the-icons ivy-rich counsel-projectile all-the-icons-ivy-rich)
  :config
  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)

  (let* ((col-def '((all-the-icons-ivy-rich-file-icon)
                    (file-name-nondirectory (:width 0.2 :face (:foreground "#61AFEF" :slant 'italic)))
                    ((lambda (str) (string-join (butlast (split-string (counsel-projectile-find-file-transformer str) "/")) "/")) (:width 0.4))
                    ;; (counsel-projectile-find-file-transformer (:width 0.4))
                    (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
                    (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
                    (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
                    (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))))
    (set-face-attribute 'all-the-icons-ivy-rich-doc-face nil :font "ChalkBoard SE" :italic t :height 136)
    (ivy-rich-set-columns 'projectile-find-file col-def)
    (ivy-rich-set-columns 'counsel-projectile-find-file col-def)
    (ivy-rich-set-columns 'projectile--find-file col-def)))

(use-package ivy-posframe
  :after ivy
  :custom-face
  (ivy-posframe-border ((t (:background ,+m-color-main))))
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-parameters '((internal-border-width . 2) (left-fringe . 18) (right-fringe . 18))
        ivy-posframe-height 14
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-display-functions-alist '((t . posframe-poshandler-top-center-with-offset))

        ivy-posframe-font "JetBrainsMonoExtraBold Nerd Font Mono 12")
  ;; ivy-posframe-font "JetBrainsMono Nerd Font 13")
  (defun ivy-posframe-get-size ()
    "Func for detect ivy posframe size after resize dynamically"
    (list
     ;; :height ivy-posframe-height
     ;; :width ivy-posframe-width
     :min-height (or ivy-posframe-min-height
                     (let ((height (+ ivy-height 1)))
                       (min height (or ivy-posframe-height height))
                       ))
     :min-width (or ivy-posframe-min-width
                    (let ((width (round (* (frame-width) 0.9))))
                      (min width (or ivy-posframe-width width))
                      ))))

  (defun posframe-poshandler-frame-top-center-with-offset (info)
    "Posframe's position handler.

Let posframe(0.5, 0) align to frame(0.5, 0).  The structure of
INFO can be found in docstring of `posframe-show'."
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          40))

  (defun ivy-posframe-display-at-frame-top-center-with-offset (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-top-center-with-offset))

  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center-with-offset))))

;;; Formatter
;; Improve counsel search (async)
(use-package format-all
  :defer 1.5
  :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode go-mode) . format-all-mode)
  :config
  (add-to-list '+format-on-save-enabled-modes 'typescript-mode t)
  (add-to-list '+format-on-save-enabled-modes 'ng2-mode t)
  (add-to-list '+format-on-save-enabled-modes 'js2-mode t))

(defun my-ecmascript-formatter ()
  "My custom chaif of formatters for ecmascript and html."
  (interactive)
  (prettier-prettify)
  (+format/buffer))

(defun my-install-formatter ()
  (add-hook 'before-save-hook 'my-ecmascript-formatter))

(use-package prettier
  :defer 1.5
  :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode vue-mode web-mode) . prettier-mode))

;; (use-package prettier-js
;;   :defer 0.3
;;   ;; :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode) . prettier-js-mode)
;;   :config
;;   (setq prettier-js-args '(
;;                            "--trailing-comma" "all"
;;                            "--bracket-spacing" "true")))


;;; Flycheck
;; (use-package flycheck
;;   :defer 0.6
;;   :config)
;;; Smartparens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(use-package autopair
  :defer 5
  :config
  (autopair-global-mode))
;; (use-package smartparens
;;   :defer 5
;;   :config

;;   (defun indent-between-pair (&rest _ignored)
;;     (newline)
;;     (indent-according-to-mode)
;;     (forward-line -1)
;;     (indent-according-to-mode))

;;   (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
;;   (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
;;   (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))




;; (defun my-after-electric-pair-inserted ()
;;   "Call afer electric paid indention inserted for auto align with current mode."
;;   (message "Amm was inserted"))

;; (advice-add 'my-after-electric-pair-inserted :after #'electric-pair-post-self-insert-function)
;; (advice-add 'my-after-electric-pair-inserted :after #'electric-pair-open-newline-between-pairs-psif)
;; (advice-add 'my-after-electric-pair-inserted :after #'electric-pair-will-use-region)


;;; Undo
(use-package undo-tree
  :defer 0.3
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/tmp/undo")))
  ;; (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode))

;;; Spell check
(use-package flyspell
  :defer 7
  :config
  ;; (setq ispell-program-name "aspell")
  ;; You could add extra option "--camel-case" for since Aspell 0.60.8
  ;; @see https://github.com/redguardtoo/emacs.d/issues/796
  ;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
  (setq-default flyspell-prog-text-faces
                '(tree-sitter-hl-face:comment
                  tree-sitter-hl-face:doc
                  tree-sitter-hl-face:string
                  tree-sitter-hl-face:function
                  tree-sitter-hl-face:variable
                  tree-sitter-hl-face:type
                  tree-sitter-hl-face:method
                  tree-sitter-hl-face:function.method
                  tree-sitter-hl-face:function.special
                  tree-sitter-hl-face:attribute
                  font-lock-comment-face
                  font-lock-doc-face
                  font-lock-string-face
                  lsp-face-highlight-textual
                  default))

  (setq spell-fu-directory "~/.doom.d/dictionary") ;; Please create this directory manually.
  (setq ispell-personal-dictionary "~/.doom.d/dictionary/.pws")
  (after! ispell
    (setq ispell-program-name "aspell"
          ;; Notice the lack of "--run-together"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=56"))
    (ispell-kill-ispell t))

  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))

  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))
;; (use-package spell-fu
;;   :defer 0.1
;;   :config
;;   (global-spell-fu-mode))

(add-hook 'text-mode-hook 'flyspell-mode!)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; Folding
;;;; Html
(use-package origami
  :defer 2
  :bind (:map evil-normal-state-map
         ("SPC z a" . origami-toggle-node)
         ("SPC z r" . origami-open-all-nodes)
         ("SPC z m" . origami-close-all-node))
  :hook ((ng2-html-mode html-mode) . origami-mode))
;;;; Outline
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

;;; File managers
;;;; treemacs
(use-package treemacs
  :defer t
  :custom-face
  (font-lock-doc-face ((t (:inherit nil))))
  (doom-themes-treemacs-file-face ((t (:inherit font-lock-doc-face :slant italic))))
  (doom-themes-treemacs-root-face ((t (:inherit nil :slant italic))))
  (treemacs-root-face ((t (:inherit variable-pitch :slant italic))))
  :custom
  (treemacs-width 45)
  :config
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package ranger
  :defer t
  :bind (:map evil-normal-state-map
         ("SPC r r" . ranger))
  :config
  (setq ranger-preview-file t)
  (setq ranger-footer-delay 0.2)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-show-literal t)
  (setq ranger-dont-show-binary t)
  (setq ranger-max-preview-size 10)
  (setq ranger-preview-delay 0.040))

;;;; Dired
(use-package all-the-icons-dired
  :defer 5
  :hook (dired-mode . all-the-icons-dired-mode))

;;; Bookmarks
;;;; quick bm
(use-package bm
  :defer 3
  :custom-face
  (bm-face ((t (:foreground ,+m-color-secondary))))
  :bind (("C-M-n" . bm-next)
         ("C-M-p" . bm-previous)
         ("s-b" . bm-toggle)))

;;;; Doom bm
(use-package bookmark
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file "~/.doom.d/bookmarks"))


;;; Google translate
(use-package google-translate
  :defer 30
  :bind
  (:map google-translate-minibuffer-keymap
   ("C-k" . google-translate-next-translation-direction)
   ("C-l" . google-translate-next-translation-direction))
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-backend-method 'curl)
  (setq google-translate-translation-directions-alist
        '(("en" . "ru") ("ru" . "en") ))
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))


;;; Terminal
(set-face-attribute 'fixed-pitch nil ':font "Fira Code 14")
(add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
            (buffer-face-mode t)))

(use-package vterm-toggle
  :defer 8
  :bind (:map evil-normal-state-map
         ("SPC t ]" . vterm-toggle-forward)
         ("SPC t [" . vterm-toggle-backward)
         ("SPC t n" . (lambda () (interactive)
                        (let ((current-buffer-name (buffer-name)))
                          (vterm-toggle--new)
                          (delete-window)
                          (display-buffer current-buffer-name)
                          (vterm-toggle-forward))))
         ("SPC t x" . (lambda (args) (interactive "P")
                        (when (string-match "vterm" (buffer-name))
                          (let ((kill-buffer-query-functions nil))
                            (kill-this-buffer)
                            (+vterm/toggle args)))))
         ("SPC t h" . vterm-toggle-hide))
  :config
  (setq vterm-toggle-scope 'project))


;;; Colors
(use-package rainbow-mode
  :hook (((css-mode scss-mode org-mode typescript-mode js-mode emacs-list-mode). rainbow-mode))
  :defer 15)

;; TODO
;; (use-package hl-todo
;;
;;   :defer 0.1
;;   :init
;;   (global-hl-todo-mode 1)
;;   :config
;;   (setq hl-todo-keyword-faces
;;         '(("TODO"   . "#E5C07B")
;;           ("FIXME"  . "#E06C75")
;;           ("DEBUG"  . "#C678DD")
;;           ("GOTCHA" . "#FF4500")
;;           ("NOTE"   . "#98C379")
;;           ("STUB"   . "#61AFEF"))))

;;; Themes
;;; Modus
(use-package modus-themes
  :defer t)
;;;; Theme switcher
(use-package heaven-and-hell
  :after doom-themes
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . pinky-winky)
          (dark . pinky-winky-dark)))
  ;; (setq heaven-and-hell-themes
  ;;       '((light . zaiste)
  ;;         (dark . deep-atom)))
  ;; (dark . doom-moonlight)))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("<f5>" . heaven-and-hell-toggle-theme)))

;;;; Modeline
;; The most important package in the world
(use-package nyan-mode
  :after doom-modeline
  :init
  (nyan-mode))

(use-package doom-modeline
  :defer 0.1
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name))

;;; Time track
(use-package wakatime-mode
  :defer 1.3
  :config
  (global-wakatime-mode))

;;; Indent guide
;; NOTE: can i live without it?
;; (use-package indent-guide
;;   :defer 1.2
;;   :hook ((web-mode
;;           html-mode
;;           scss-mode
;;           css-mode
;;           go-mode
;;           typescript-mode
;;           js-mode
;;           ng2-ts-mode
;;           python-mode) . indent-guide-mode)
;;   :custom-face
;;   (indent-guide-face ((t (:foreground ,+m-color-main :font "Fira Code" :height 0.9))))
;;   :config
;;   (add-hook '+doom-dashboard-mode-hook #'(lambda () (setq indent-guide-mode nil)))
;;   (setq indent-guide-char "|")
;;   ;; (setq indent-guide-char ":")
;;   (setq indent-guide-delay 0.2))

;;; Fast commenting
(use-package turbo-log
  :defer 15
  :bind (("C-s-l" . turbo-log-print)
         ("C-s-i" . turbo-log-print-immediately)
         ("C-s-h" . turbo-log-comment-all-logs)
         ("C-s-s" . turbo-log-uncomment-all-logs)
         ("C-s-x" . turbo-log-delete-all-logs))
  :config
  (add-to-list 'turbo-log--modes '(js2-mode . turbo-log--ecmascript-print))

  (setq turbo-log--prefix "🚀")
  (setq turbo-log--ecmascript-loggers '("console.log" "console.debug" "console.warn"))
  (setq turbo-log--python-loggers '("print" "logger"))
  (setq turbo-log--golang-loggers '("fmt.Printf" "log.Info().Msgf")))

;;; Quickly type converting
(use-package quicktype
  :defer 15
  :bind (("C-x j v" . quicktype-json-to-type)
         ("C-x j p" . quicktype-paste-json-as-type)
         ("C-x j q" . quicktype)))

;;; Programming
;; Common configurations for all programming languages
;;;; Flycheck
;; (use-package flycheck
;;   :defer 0.1
;;   :bind (:map evil-normal-state-map
;;          ("SPC f n" . flycheck-next-error)))
;;;; Lsp
;;;;
;; (defun my-setup-flycheck ()
;;   (if (eq major-mode 'go-mode)
;;       ;; (flycheck-add-next-checker 'lsp '('golangci-lint 'go-errcheck 'go-gofmt 'go-goim) 'append)
;;       (flycheck-add-next-checker 'lsp 'golangci-lint 'append)))

(use-package lsp
  :defer 0.1
  ;; TIDE check, less laggi?
  ;; :hook (((go-mode scss-mode css-mode web-mode ng2-html-mode ng2-ts-mode python-mode typescript-tsx-mode) . lsp-deferred))
  :hook (((go-mode
           scss-mode
           css-mode
           js-mode
           typescript-mode
           vue-mode
           web-mode
           ng2-html-mode
           ng2-ts-mode
           python-mode
           typescript-tsx-mode
           clojure-mode) . lsp-deferred))
  :bind (:map evil-normal-state-map
         ("SPC f n" . flycheck-next-error)
         ("g i" . lsp-goto-implementation))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 4000)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :config

  (set-face-attribute 'lsp-face-highlight-read nil :background "#61AFEF")
  ;; Flycheck patch checkers
  (require 'flycheck)
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  ;; Golang
  (defun lsp-go-install-save-hooks ()
    ;; (flycheck-add-next-checker 'lsp '(t . golangci-lint) 'append)

    (flycheck-add-next-checker 'lsp '(warning . go-gofmt) 'append)
    (flycheck-add-next-checker 'lsp '(warning . go-golint))
    (flycheck-add-next-checker 'lsp '(warning . go-errcheck))
    (flycheck-add-next-checker 'lsp '(warning . go-staticcheck))

    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                      (setq company-backends '(company-tabnine company-dabbrev))))


  ;; Override company backends for lsp
  ;; (setq +lsp-company-backends '(company-tabnine :separate company-capf))
  ;; (setq +lsp-company-backends '(company-tabnine :separate company-yasnippet))
  ;; (setq +lsp-company-backends '(company-tabnine :separate company-capf))
  (setq +lsp-company-backends '(company-tabnine :separate))

  (setq lsp-disabled-clients '(html html-ls))
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (setq lsp-eldoc-hook nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-line-length 200
        lsp-ui-sideline-diagnostic-max-lines 5
        lsp-ui-doc-delay 2
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-border +m-color-main))

;;;; Syntax highlight
(use-package tree-sitter-langs
  :defer 6)


(use-package tree-sitter
  :after tree-sitter-langs
  :hook ((go-mode typescript-mode css-mode typescript-tsx-mode html-mode scss-mode ng2-mode js-mode python-mode rust-mode ng2-ts-mode ng2-html-mode) . tree-sitter-hl-mode)
  :config
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (push '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . css) tree-sitter-major-mode-language-alist)
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))


;;;; Company
(defun my-setup-tabnine ()
  (interactive)
  (setq-local company-backends '(company-tabnine :separate company-capf)))

;;;; Only tabnine
(defun my-setup-tabnine-2 ()
  (interactive)
  (setq-local +lsp-company-backends '((company-tabnine company-dabbrev)))
  (setq-local company-backends '(company-tabnine company-dabbrev)))

(defun my-setup-tabnine-3 ()
  (interactive)
  (setq-local +lsp-company-backends '((company-capf)))
  (setq-local company-backends '((company-capf))))

(use-package company
  :defer t
  :bind (:map evil-insert-state-map ("C-'" . company-yasnippet)))

;; (my-setup-tabnine)
;; Autocomplete with AI
(use-package company-tabnine
  :after (company lsp)
  :bind (("C-x C-i" . company-tabnine))
  :when (featurep! :completion company)
  :config
  (setq company-idle-delay 0.1)
  (setq company-show-numbers nil)
  (setq company-quick-access-modifier 'super)
  (setq company-show-quick-access t)
  (setq company-minimum-prefix-length 1)
  (setq company-tabnine-always-trigger nil)
  (setq company-tabnine-show-annotation t)
  ;; (setq company-tabnine-auto-balance nil)
  (setq company-dabbrev-char-regexp "[A-z:-]"))


;;; Languages
;;;; Lisp

(use-package elisp-mode
  :defer t
  :hook ((emacs-lisp-mode . paren-face-mode)
         (emacs-lisp-mode . my-setup-tabnine-2)
         (emacs-lisp-mode . rainbow-delimiters-mode-disable))

  :bind (("C-c o" . outline-cycle)
         ("C-c r" . outline-show-all)
         ("C-c m" . outline-hide-body)
         ("C-c ]" . outline-next-heading)
         ("C-c [" . outline-previous-heading)
         ("C-c c" . counsel-outline)
         ("C-c e" . outline-hide-entry)
         ("C-c t" . outline-toggle-children)
         ("C-c b" . outline-cycle-buffer))
  :config
  (setq rainbow-delimiters-mode -1))

(use-package clojure-mode
  :hook ((clojure-mode . format-all-mode)
         (clojure-mode . paren-face-mode))
  :defer t)

(use-package cider
  :defer t)

(use-package package-build
  :defer 15)

(use-package package-lint
  :defer 15)

;;;; Typescript
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package typescript-mode
  :defer 0.5
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\.ts\'" . typescript-mode)))

(use-package ng2-mode
  :after typescript-mode
  :hook (ng2-html-mode . web-mode)
  :config
  (setq lsp-clients-angular-language-server-command
        '("node"
          "/usr/local/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          "/usr/local/lib/node_modules"
          "--stdio")))

;;;; Javascript
(use-package js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode))

;;;; Golang
(use-package go-playground
  :defer 15)

;; (defun my-go-mode-hook ()
;;   (flycheck-add-next-checker 'lsp 'golangci-lint 'go-errcheck 'go-gofmt 'go-goim 'append))
;; (use-package go-mode
;;   :defer 0.3
;;   :hook (go-mode . my-go-mode-hook))

;;;; Rust
(use-package rustic
  :defer 0.9
  :bind (:map rustic-mode-map
         ("M-j" . lsp-ui-imenu)
         ("M-?" . lsp-find-references)
         ("C-c C-c l" . flycheck-list-errors)
         ("C-c C-c a" . lsp-execute-code-action)
         ("C-c C-c r" . lsp-rename)
         ("C-c C-c q" . lsp-workspace-restart)
         ("C-c C-c Q" . lsp-workspace-shutdown)
         ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t
        rustic-format-display-method 'ignore)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;;;; Python
(use-package pipenv
  :defer 0.5
  :hook (python-mode . pipenv-mode)
  :config
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/.local/share/virtualenvs"))
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

;;;;; Python check depdendencies
(use-package pippel
  :defer t)


(use-package python-mode
  :defer 0.5
  :hook (python-mode . format-all-mode)
  :config
  (setq pytnon-indent-level 4)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4))))

(use-package lsp-pyright
  :defer 0.5
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace"))

;;;; Vue
;; npm install -g @volar/server
(use-package! lsp-volar
  :defer t)

;;;; Web mode
(use-package web-mode
  :defer 0.5
  :mode (("\\.vue\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-comment-formats
        '(("java"       . "/*")
          ("javascript" . "//")
          ("typescript" . "//")
          ("vue"        . "//")
          ("php"        . "/*")
          ("pug"        . "//")
          ("css"        . "/*")))
  ;; Crutch for tsx mode
  ;; (setq font-lock-defaults '('(web-mode-fontify) t))
  ;; (setq tree-sitter-hl-use-font-lock-keywords nil)
  ;; ---------------------------END CRUTCH HERE -------------------------------------
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

;;;; Pug
(use-package pug-mode
  :defer 3)

;;;; Html
(use-package emmet-mode
  :hook ((scss-mode . emmet-mode) (css-mode . emmet-mode) (ng2-html-mode . emmet-mode) (html-mode . emmet-mode))
  :defer 5)

;;;; Scss
(use-package css-mode
  :defer 10
  :hook ((css-mode . my-setup-tabnine) (scss-mode . my-setup-tabnine))
  :config
  (my-setup-tabnine)
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defun run-sass-auto-fix ()
    "Run sass auto fix if cli tool exist"
    (interactive)
    (let ((default-directory (file-name-directory buffer-file-name)))
      (shell-command "sass-lint-auto-fix")
      (revert-buffer-no-confirm)
      (message "SASS FORMATTED")
      ))
  (add-hook 'scss-mode-hook '(lambda () (add-hook 'after-save-hook #'run-sass-auto-fix t t))))

;;;; Json
(use-package json-mode
  :defer 5
  :hook (json-mode . format-all-mode))

;;;; Debug
(use-package dap-mode
  :defer 3
  :bind (:map evil-normal-state-map
         ("SPC d n" . dap-next)
         ("SPC d i" . dap-step-in)
         ("SPC d o" . dap-step-out)
         ("SPC d c" . dap-continue)
         ("SPC d Q" . dap-disconnect)
         ("SPC d d" . dap-debug)
         ("SPC d r" . dap-debug-recent)
         ("SPC d l" . dap-ui-locals)
         ("SPC d b" . dap-ui-breakpoints)
         ("SPC d s" . dap-ui-sessions)
         ("SPC d l" . dap-debug-last)
         ("SPC d p" . dap-breakpoint-toggle)
         ("SPC d e" . dap-debug-edit-template))
  :config
  (dap-mode 1)
  (require 'dap-go))

;;;; Docker compose
(use-package docker-compose-mode
  :defer 6)

;;;; Docker
(use-package dockerfile-mode
  :defer 6)

;;;; Jenkins
(use-package jenkinsfile-mode
  :defer 6
  :config)

;;;; Nginx
(use-package company-nginx
  :after nginx-mode
  :config (add-hook 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))

(use-package nginx-mode
  :defer 10)

;;; Help tools
;;;; PASCAL_CASE -> camelCase -> snake_case
(use-package string-inflection
  :defer 10
  :bind ("C-s-c" . string-inflection-all-cycle))

;;; Git
(use-package magit
  :defer t
  :config
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq))

(use-package gist                       ;
  :defer t
  :bind (:map gist-list-menu-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . gist-fork)
         ("x" . gist-kill-current)
         ("f" . avy-goto-char)
         ("v" . evil-visual-char)
         :map evil-normal-state-map
         ("SPC g l g" . gist-list)))


(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo"))
  (push `(,+m-work-gitlab-url ,(concat +m-work-gitlab-url "/api/v4")
                              "gpalex" forge-gitlab-repository)
        forge-alist))

(use-package git-gutter
  :defer 9
  :init
  (global-git-gutter-mode)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk))

;;; Blamer (own package)
(use-package blamer
  :defer 15
  :custom
  (blamer-idle-time 0.35)
  ;; (blamer-min-offset 50)
  (blamer-max-commit-message-length 65)
  (blamer-commit-formatter "• %s")
  ;; (blamer-entire-formatter "   %s")
  (blamer-entire-formatter "  > %s")
  ;; (blamer-offset-per-symbol 17)
  ;; (blamer-view 'overlay-right)
  (blamer-view 'overlay)
  ;; (blamer-uncommitted-changes-message "(งツ)
  (blamer-uncommitted-changes-message "uncommitted yet")
  :custom-face
  (blamer-face ((t :inherit company-preview
                   :italic t
                   :font "Fira Code 14"
                   :height 0.9
                   :background nil)))
  :config
  (defun blamer-callback-show-commit-diff (commit-info)
    (interactive)
    (message "Blamer my custom callback")
    (message "%s" commit-info)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (magit-show-commit commit-hash))))

  (defun blamer-callback-open-remote (commit-info)
    (interactive)
    (message "Copy authro")
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (message commit-hash)
        (forge-browse-commit commit-hash))))

  (setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))

  (global-blamer-mode 1))

(use-package hydra
  :defer t)

;;; Keybinding
;;;; Ru/en keybinding
(use-package reverse-im
  :defer 5
  :config
  (reverse-im-activate "russian-computer"))
(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

;;;; Register copy past
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2017-01-23"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (line-beginning-position))
             (setq $p2 (l(defun xah-paste-from-register-1 ()
                           "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
                           (interactive)
                           (when (use-region-p)
                             (delete-region (region-beginning) (region-end)))
                           (insert-register ?1 t))ine-end-position))))
    (copy-to-register ?1 $p1 $p2)
    (message "Copied to register 1: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

;; (global-set-key (kbd "s-1") 'xah-copy-to-register-1)
;; (global-set-key (kbd "s-2") 'xah-paste-from-register-1)
;;;; Global keybinding
(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-c l") 'smerge-keep-lower)
(global-set-key (kbd "C-c u") 'smerge-keep-upper)
(global-set-key (kbd "C-c a") 'smerge-keep-all)
(global-set-key (kbd "C-c j") 'smerge-next)
(global-set-key (kbd "C-c k") 'smerge-prev)

(global-set-key (kbd "s-e") 'emmet-expand-line)
(global-set-key (kbd "C-s") 'save-buffer)

(use-package evil-leader
  :after evil
  :bind (:map evil-normal-state-map
         ("f" . avy-goto-char)
         ("SPC n r f" . org-roam-node-find)
         ("SPC t a" . treemacs-add-project-to-workspace)
         ("SPC g t" . git-timemachine)
         ;; Org mode
         ("SPC d t" . org-time-stamp-inactive)
         ("SPC d T" . org-time-stamp)
         ("SPC r p" . +python/open-ipython-repl)
         ("SPC r n" . nodejs-repl)
         ("s-2" . xah-paste-from-register-1)
         :map evil-insert-state-map
         ("s-2" . xah-paste-from-register-1)
         :map evil-visual-state-map
         ("s-1" . xah-copy-to-register-1)
         ("s-2" . xah-paste-from-register-1)
         :map ivy-mode-map
         ("s-1" . xah-copy-to-register-1)
         ("s-2" . xah-paste-from-register-1))
  :init
  (global-evil-leader-mode)
  :config
  (setq-default evil-kill-on-visual-paste nil)
  (evil-leader/set-key
    "f" 'evil-find-char
    "b" 'evilem-motion-previous-line
    "x" 'my-ecmascript-formatter
    "k" 'save-buffer-without-dtw
    "w" 'avy-goto-word-0
    "f" 'avy-goto-word-1
    "]" 'flycheck-next-error
    "[" 'flycheck-previous-error

    "d" 'dap-debug

    "o" 'org-mode
    "q" 'kill-current-buffer
    "v" 'vterm
    "`" 'vterm-toggle-cd
    "i" 'git-messenger:popup-message
    "t" 'google-translate-smooth-translate
    "T" 'google-translate-query-translate

    "a" 'counsel-org-agenda-headlines
    "c" 'dired-create-empty-file
    "p" '+format/buffer
    "s" 'publish-org-blog
    "g" 'dogears-go

    ;; Lsp
    "h" 'lsp-ui-doc-show
    "e" 'lsp-treemacs-errors-list
    "l" 'lsp-execute-code-action

    "r" 'treemacs-select-window
    "1" 'my-setup-tabnine
    "2" 'my-setup-tabnine-2
    "3" 'my-setup-tabnine-3

    "m" 'toggle-maximize-buffer
    "y" 'yas-expand
    ))

;;; Navigation
(use-package evil-matchit
  :defer 15)

(evilmi-load-plugin-rules '(ng2-html-mode) '(html))
(global-evil-matchit-mode 1)

;;; Org mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :defer t
  ;; :demand t
  ;; :bind
  ;; (:map org-mode-map ("C-o f" . format-org-mode-block))
  :config
  (progn
    (define-key org-mode-map "\C-x a f" "\C-x h \C-M-\\ \C-c")
    (custom-set-faces
     '(org-document-title ((t (:inherit outline-1 :height 2.5))))
     '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
     )
    (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))



    (defun publish-org-blog()
      "Publish this note to du-blog!"
      (interactive)
      (require 'ox-gfm)
      (setq org-export-with-sub-superscripts '{})
      (defun org-gfm-format-toc (headline)
        "")
      (org-gfm-export-to-markdown)
      (message (concat
                "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js"
                (buffer-file-name)))
      (shell-command
       (concat
        "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js "
        (buffer-file-name))
       ))

    (setenv "NODE_PATH"
            (concat
             (getenv "HOME") "/org-node/node_modules"  ":"
             (getenv "NODE_PATH")
             )
            )

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((typescript . t)
       (js . t)
       (restclient . t)))

    (defun org-babel-execute:typescript (body params)
      (let ((org-babel-js-cmd "npx ts-node < "))
        (org-babel-execute:js body params)))

    (defvar org-babel-js-function-wrapper
      ""
      "Javascript code to print value of body.")))

;; (defun my-org-mode-hook ()
;;   "Custom `org-mode' behaviours."
;;   (add-hook  #'(lambda ()
;;                  (message "Amma inside org mode")
;;                  (prettier-mode -1))
;;              'org-in-src-block-p
;;              nil :local))

;; (add-hook 'org-mode-hook 'my-org-mode-hook)
;; )

;;;; Org mode todos list
(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"     ; A task that needs doing & is ready to do
           "PROJ(p)"     ; A project, which usually contains other tasks
           "IDEA(i)"     ; Idea
           "STRT(s)"     ; A task that is in progress
           "WAIT(w)"     ; Something external is holding up this task
           "TEST(c)"     ; In TEST statement
           "FEEDBACK(f)" ; Feedback required
           "REVIEW(r)"   ; Somebody reviewed your feature
           "HOLD(h)"     ; This task is paused/on hold because of me
           "|"
           "DONE(d)"     ; Task successfully completed
           "KILL(k)")    ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"      ; A task that needs doing
           "[-](S)"      ; Task is in progress
           "[?](W)"      ; Task is being held up or paused
           "|"
           "[X](D)"))    ; Task was completed
        org-todo-keyword-faces
        '(("[-]"        . +org-todo-active)
          ("STRT"       . +org-todo-active)
          ("DONE"       . org-todo)
          ("IDEA"       . org-todo)
          ("[?]"        . +org-todo-onhold)
          ("WAIT"       . +org-todo-onhold)
          ("TEST"       . +org-todo-active)
          ("FEEDBACK"   . +org-todo-onhold)
          ("REVIEW"     . +org-todo-onhold)
          ("HOLD"       . +org-todo-onhold)
          ("PROJ"       . +org-todo-project)
          ("KILL"       . +org-todo-cancel))))

;;;; Org agenda
(use-package org-caldav
  :defer t
  :config
  (require 'oauth2)
  (setq org-caldav-oauth2-client-secret +m-google-calendar-client-secret)
  (setq org-caldav-oauth2-client-id +m-google-calendar-client-id)
  (setq org-icalendar-timezone "Equrope/Moscow")
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (setq org-caldav-url 'google)
  ;; For nextcloud
  ;; (setq org-caldav-url "https://mark.nl.tab.digital/remote.php/dav/calendars/artawower@mail.ru")
  ;; (setq org-caldav-calendars
  ;;       '((:calendar-id "work" :files ("~/Yandex.Disk.localized/org/almanah.org")
  ;;          :inbox "~/Yandex.Disk.localized/org/fromwork.org"))))

  (setq org-caldav-calendars
        ;; Work
        `((:calendar-id ,+m-work-calendar-id :files ("~/Yandex.Disk.localized/Dropbox/org/calendar/work.org")
           :inbox "~/Yandex.Disk.localized/Dropbox/org/calendar/fromwork.org")
          ;; Live and self education
          (:calendar-id ,+m-live-calendar-id :files ("~/Yandex.Disk.localized/Dropbox/org/calendar/live.org")
           :inbox "~/Yandex.Disk.localized/Dropbox/org/fromlive.org")
          ;; Pet projects
          (:calendar-id ,+m-pet-calendar-id :files ("~/Yandex.Disk.localized/Dropbox/org/calendar/pet.org")
           :inbox "~/Yandex.Disk.localized/Dropbox/org/frompet.org"))))

;;;; Org superstar
(use-package org-superstar
  :defer 5
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-directory "~/Yandex.Disk.localized/Dropbox/org")
  (setq org-agenda-files (append (directory-files-recursively "~/Yandex.Disk.localized/Dropbox/org/" "\\.org$")
                                 (directory-files-recursively "~/projects/pet" "\\.org$"))))

;;;; Roam
(use-package org-roam
  :defer 8
  :init
  (setq org-roam-v2-ack t)
  :config
  (cl-defmethod org-roam-node-compositetitle ((node org-roam-node))
    "Return customized title of roam node"
    (let* ((tags (org-roam-node-tags node))
           (title (org-roam-node-title node)))
      (if (not tags)
          title
        (setq joined-text (string-join tags ", "))
        (concat (propertize (format "(%s) " joined-text) 'face `(:foreground ,+m-color-main :weight bold :slant italic)) title))))

  ;; (message m-color-main)
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-node-display-template "${compositetitle:100}")
  (setq org-roam-directory (file-truename "~/Yandex.Disk.localized/Dropbox/org-roam"))
  (org-roam-db-autosync-mode))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-roam-ui-browser-function #'xwidget-webkit-browse-url))


;;;; Org ligatures
(add-hook 'org-mode-hook (lambda ()
                           "Beautify Org Checkbox Symbol"
                           (push '("[ ]" .  "☐") prettify-symbols-alist)
                           (push '("[X]" . "☑" ) prettify-symbols-alist)
                           (push '("[-]" . "❍" ) prettify-symbols-alist)
                           (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
                           (push '("#+TITLE:" . "") prettify-symbols-alist)
                           (push '("#+DESCRIPTION:" . "") prettify-symbols-alist)
                           (push '("#+ID:" . "") prettify-symbols-alist)
                           (push '("#+FILETAGS:" . "") prettify-symbols-alist)
                           (push '("#+ACTIVE:" . "") prettify-symbols-alist)
                           (push '("#+START_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
                           (push '("#+END_HIDDEN" . "") prettify-symbols-alist)
                           (push '("[#A]" . "⚡") prettify-symbols-alist)
                           (push '("[#B]" . "⬆") prettify-symbols-alist)
                           (push '("[#C]" . "■") prettify-symbols-alist)
                           (push '("[#D]" . "⬇") prettify-symbols-alist)
                           (push '("[#E]" . "❓") prettify-symbols-alist)
                           (prettify-symbols-mode)))

;;;; Org indent
(use-package org-indent
  :defer 8
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

;;;; Org rest client
(use-package ob-async
  :defer 5
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package ob-restclient
  :defer 8)

(defun format-org-mode-block ()
  "Format org mode code block"
  (interactive "p")
  ;; (execute-kbd-macro (kbd "C-c ' C-x h C-M-\\ C-c '"))
  ;; (execute-kbd-macro (read-kbd-macro "C-c ' C-x h C-M-\\ C-c '"))
  (org-edit-special)
  (format-all-ensure-formatter)
  (format-all-buffer)
  (org-edit-src-exit))


;;;; Org inline images from http
(use-package org-yt
  :defer 20
  :config
  (defun org-image-link (protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (cl-assert (string-match "\\`img" protocol) nil
               "Expected protocol type starting with img")
    (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
      (cl-assert buf nil
                 "Download of image \"%s\" failed." link)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (buffer-substring-no-properties (point) (point-max)))))

  (org-link-set-parameters
   "imghttp"
   :image-data-fun #'org-image-link)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-image-link))

;;; Dependencies
(use-package! websocket
  :after org-roam)

(use-package restclient
  :defer 3)


;;; Not programming
(use-package pretty-agenda
  :load-path "~/.doom.d/"
  :defer 15)

(use-package wakatime-ui
  :load-path "~/.doom.d/"
  :custom
  ;; (wakatim-ui-schedule-url "https://wakatime.com/share/@darkawower/bb8cf0d7-3554-4297-ac4d-01f8a155073c.svg")
  (wakatim-ui-schedule-url "https://wakatime.com/share/@darkawower/af1bfb85-2c8b-44e4-9873-c4a91b512e8d.png")
  :config
  (wakatime-ui-mode))

;;; COlloboration
(use-package floobits
  :defer t)
;;; RSS
(use-package elfeed
  :defer 30
  :config
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (setq-default elfeed-search-filter "@12-hours-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100)

  ;; (setq browse-url-generic-program #'xwidget-webkit-browse-url)
  (setq browse-url-browser-function #'browse-url-default-browser)
  ;; For built in browser
  ;; (setq browse-url-browser-function #'xwidget-webkit-browse-url)
  ;; (advice-add 'browse-url :after #'(lambda (a a2) (switch-to-first-matching-buffer "xwidget webkit")))
  ;; (advice-add 'browse-url-generic :after #'(lambda (a a2) (switch-to-first-matching-buffer "xwidget webkit")))
  )

(use-package elfeed-score
  :after elfeed
  :config
  (setq elfeed-score-score-file "~/.doom.d/elfeed.score")
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))
;;;
;;; Temporary section


;; (use-package shikimori
;;   :defer t
;;   :custom
;;   (shikimori-default-browser 'browse-url-default-browser)
;;   :config
;;   (setq shikimori-default-browser #'browse-url-firefox))

;;; Temporary unused

;; (use-package code-review
;;   :defer t)
