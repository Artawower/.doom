;;;; Awesome priority
;; Unused cause can be replaced by ligatures
(use-package org-fancy-priorities
  :defer 3
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "■" "⬇" "❓")))


;;; Ivy
(use-package lsp-ivy
  :after lsp)

(use-package ivy-posframe
  :after ivy
  :disabled t
  :custom-face
  (ivy-posframe-border ((t (:background ,+m-color-main))))
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-parameters '((internal-border-width . 20) (left-fringe . 18) (right-fringe . 18))
        ivy-posframe-height 14
        ;; ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ;; ivy-posframe-display-functions-alist '((t . posframe-poshandler-top-center-with-offset))
                                        ; ivy-posframe-font "JetBrainsMonoExtraBold Nerd Font Mono 13")
        ivy-posframe-font "JetBrainsMono Nerd Font Mono Bold 13")
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



  (defun ivy-posframe-display-at-frame-top-center-with-offset (str)
    (ivy-posframe--display str #'posframe-poshandler-frame-top-center-with-offset))

  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center-with-offset))))

;; Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         :map evil-insert-state-map
         ("C-x C-o" . completion-at-point)
         ("S-SPC" . completion-at-point))

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (company-mode -1)
  (corfu-global-mode)
  :config
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))

;; Optionally use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(use-package poly-vue
  :defer 0.5)

;;; Environment
;; Doom env instead!
;; (use-package exec-path-from-shell
;;   :defer 0.3
;;   :config
;;   (custom-set-variables
;;    '(exec-path-from-shell-arguments (quote ("-l"))))
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-env "SSH_AGENT_PID")
;;   (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
;;; Org
(use-package calfw
  :defer 10)

;;;; Git messanger
(use-package git-messenger
  :defer 25
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  ;; :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string popuped-message
                                :left-fringe 8
                                :right-fringe 8
                                ;; :poshandler #'posframe-poshandler-window-top-right-corner
                                :poshandler #'posframe-poshandler-window-top-right-corner
                                ;; Position broken with xwidgets and emacs 28
                                ;; :position '(-1 . 0)
                                :y-pixel-offset 20
                                :x-pixel-offset -20
                                :internal-border-width 2
                                :lines-truncate t
                                :internal-border-color (face-foreground 'font-lock-comment-face)
                                :accept-focus nil)
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    ;; (advice-add #'git-messenger:popup-close :override #'(setq modal-opened 0))
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))


(use-package multi-vterm
  :after vterm-toggle
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-i")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "SPC t d")  #'multi-vterm-toggle)
  (evil-define-key 'normal vterm-mode-map (kbd "C-i")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "SPC t m")  #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd "SPC ]")    #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd "SPC [")    #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;;; Autopairs
(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(use-package electric
  :defer 3
  :bind (:map evil-insert-state-map
         ("RET" . new-line-dwim))
  :config
  (setq electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs nil
        electric-pair-open-newline-between-pairs nil)

  ;; https://github.com/hlissner/doom-emacs/issues/1739#issuecomment-529858261
  ;; NOTE: fix indent after electric pair appear
  ;; BUG not work properly
  (electric-pair-mode 1))

;;; VUE JS (This solution doesn't work properly)
(use-package polymode
        :ensure t
        :defer t
        :hook (vue-mode . lsp-deferred)
        :mode ("\\.vue\\'" . vue-mode)
        :config


        (define-innermode poly-vue-template-innermode
          :mode 'html-mode
          :head-matcher "<[[:space:]]*template[[:space:]]*[[:space:]]*>"
          :tail-matcher "</[[:space:]]*template[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-script-innermode
          :mode 'js-mode
          :head-matcher "<[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-typescript-innermode
          :mode 'typescript-mode
          :head-matcher "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*ts[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-javascript-innermode
          :mode 'js2-mode
          :head-matcher "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*js[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-auto-innermode poly-vue-template-tag-lang-innermode
          :head-matcher "<[[:space:]]*template[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*template[[:space:]]*[[:space:]]*>"
          :mode-matcher (cons  "<[[:space:]]*template[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
          :head-mode 'host
          :tail-mode 'host)

        (define-auto-innermode poly-vue-script-tag-lang-innermode
          :head-matcher "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :mode-matcher (cons  "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
          :head-mode 'host
          :tail-mode 'host)

        (define-auto-innermode poly-vue-style-tag-lang-innermode
          :head-matcher "<[[:space:]]*style[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*style[[:space:]]*[[:space:]]*>"
          :mode-matcher (cons  "<[[:space:]]*style[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-style-innermode
          :mode 'css-mode
          :head-matcher "<[[:space:]]*style[[:space:]]*[[:space:]]*>"
          :tail-matcher "</[[:space:]]*style[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-polymode vue-mode
          :hostmode 'poly-sgml-hostmode
          :innermodes '(
                        poly-vue-typescript-innermode
                        poly-vue-javascript-innermode
                        poly-vue-template-tag-lang-innermode
                        poly-vue-script-tag-lang-innermode
                        poly-vue-style-tag-lang-innermode
                        poly-vue-template-innermode
                        poly-vue-script-innermode
                        poly-vue-style-innermode
                        )))


;;; Elfeed xwidgets

  (define-minor-mode lordpretzel-elfeed-xwidgets-mode
    "Minor mode for setting up keys when viewing elfeed entry in xwidgets."
    :init-value nil
    :lighter "elfeed-browsing"
    :keymap
    `((,(kbd "q")
       . lordpretzel/elfeed-search-window-only))
    :global nil)

  ;; register minor mode with xwidgets-reuse to turn it on or off
  (xwidgets-reuse-register-minor-mode 'lordpretzel-elfeed-xwidgets-mode)

  (defun lordpretzel/elfeed-search-window-only ()
    "Show only the search window of elfeed."
    (interactive)
    (switch-to-buffer (elfeed-search-buffer))
    (delete-other-windows)
    )

  (defun lordpretzel/elfeed-open-entry-in-xwidgets
      (entry)
    (interactive
     (list
      (elfeed-search-selected :ignore-region)))
    (require 'elfeed-show)
    (when
        (elfeed-entry-p entry)
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (forward-line)
      (let
          ((link
            (elfeed-entry-link entry)))
        (when link
          (let
              ((window
                (selected-window))
               newwindow)
            (delete-other-windows)
            (setq newwindow
                  (split-window-right))
            (select-window newwindow)
            (lordpretzel/elfeed-xwidget-reuse-browse-url link)
            (select-window window))))))

  (define-key elfeed-show-mode-map (kbd "<RET>") lordpretzel/elfeed-open-entry-in-xwidgets)


;;;; Sticky header
(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :defer 8)


(use-package beacon
  :defer 12
  :custom
  (beacon-color "#61AFEF")
  :config
  (beacon-mode 1))


;;; Gist
;; not work
(defun my-setup-gist-bindings (&rest _)
    (interactive)
    (evil-normal-state)
    (define-key gist-list-menu-mode-map "n" 'git-timemachine-show-next-revision)
    (define-key gist-list-menu-mode-map "x" 'gist-kill-current)
    (define-key gist-list-menu-mode-map "p" 'gist-print-current-url)
    (define-key gist-list-menu-mode-map "+" 'gist-add-buffer)
    (define-key gist-list-menu-mode-map "e" 'gist-edit-current-description)
    (define-key gist-list-menu-mode-map "s" 'gist-star)
    (define-key gist-list-menu-mode-map "u" 'gist-unstar)
    (define-key gist-list-menu-mode-map "r" 'gist-reload)
    (define-key gist-list-menu-mode-map "RET" 'gist-fetch-current)
    (define-key gist-list-menu-mode-map "TAB" 'gist-fetch-current-noselect)

    ;; (define-key evil-normal-state-local-map "n" 'git-timemachine-show-next-revision)
    ;; (define-key evil-normal-state-local-map "x" 'gist-kill-current)
    ;; (define-key evil-normal-state-local-map "p" 'gist-print-current-url)
    ;; (define-key evil-normal-state-local-map "+" 'gist-add-buffer)
    ;; (define-key evil-normal-state-local-map "e" 'gist-edit-current-description)
    ;; (define-key evil-normal-state-local-map "s" 'gist-star)
    ;; (define-key evil-normal-state-local-map "u" 'gist-unstar)
    ;; (define-key evil-normal-state-local-map "r" 'gist-reload)
    ;; (define-key evil-normal-state-local-map "RET" 'gist-fetch-current)
    ;; (define-key evil-normal-state-local-map "TAB" 'gist-fetch-current-noselect))

(use-package lsp-grammarly
  :defer 20
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred

;;; Org mode
;; (use-package polymode
;;   :defer t)
;; (use-package poly-org
;;   :hook (org-mode . poly-org-mode)
;;   :defer 15)

;;;; Offline documentation
(use-package counsel-dash
  :defer t
  :bind (:map evil-normal-state-map
         ("SPC i d" . counsel-dash))
  :config
  (setq counsel-dash-docsets-path "~/.doom.d/.docsets")
  (setq counsel-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master")
  (setq counsel-dash-common-docsets '("Javascript" "HTML" "React" "Angular")))

;; (use-package dash-docs
;;   :defer t
;;   :config
;;   (setq dash-docs-docsets-path "~/.doom.d/.docsets"))

;;; Anime
;; (use-package shikimori
;;   :defer t
;;   :custom
;;   (shikimori-default-browser 'browse-url-default-browser)
;;   :config
;;   (setq shikimori-default-browser #'browse-url-firefox))

;;; Python
;;;;; Python check depdendencies
(use-package pippel
  :defer t)

(use-package live-py-mode
  :defer t)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

;;; Debug

;; (use-package benchmark-init
;;   :config

;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
;; )


;;; (use-package smartparens
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

;;; COmpany
(add-hook! (go-mode scss-mode css-mode js-mode typescript-mode vue-mode web-mode ng2-html-mode ng2-ts-mode emacs-lisp-mode)
           #'reset-lsp-backends)

(defun reset-lsp-backends-straightaway ()
  (my-setup-tabnine))

(defun reset-lsp-backends (&optional arg)
  (run-at-time "2 sec" nil #'reset-lsp-backends-straightaway))

(advice-add 'lsp :after #'reset-lsp-backends)

;;; Indention
(use-package aggressive-indent
  :defer t
  :hook ((emacs-lisp-mode css-mode) . aggressive-indent-mode))

;;; Files
(use-package dirvish
  :defer t
  :config
  (setq dired-kill-when-opening-new-dired-buffer t) ;; added in emacs 28
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq large-file-warning-threshold 50000000)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-AGhlv --group-directories-first --time-style=long-iso"))

;;; Navigation
(use-package dumb-jump
  :defer t)

;;; Themes
;; Not for me

;;;; Change template for all componnets :)
(use-package ewal
  :defer t
  :config
  (setq ewal-use-built-in-always-p nil
        ewal-use-built-in-on-failure-p t
        ewal-built-in-palette "sexy-material"))

(use-package ewal-doom-themes
  :defer t)
;;; Files
(use-package dirvish
  :defer t
  :bind (:map dirvish-mode-map
         ("q" . dirvish-quit))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t) ;; added in emacs 28
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dirvish-show-icons nil)
  (setq large-file-warning-threshold 50000000)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-AGhlv --group-directories-first --time-style=long-iso"))


;; Documentation
;; TODO: check usage
(use-package zeal-at-point
  :defer t
  :bind (:map evil-normal-state-map ("SPC d z" . zeal-at-point)))

;;; Indent guide
;; NOTE: can i live without it?
(use-package indent-guide               ;
  :defer 1.2
  :hook ((web-mode
          html-mode
          scss-mode
          css-mode
          go-mode
          typescript-mode
          js-mode
          ng2-ts-mode
          python-mode) . indent-guide-mode)
  :custom-face
  (indent-guide-face ((t (:foreground ,+m-color-main :font "Fira Code" :height 0.9))))
  :config
  (add-hook '+doom-dashboard-mode-hook #'(lambda () (setq indent-guide-mode nil)))
  (setq indent-guide-char "|")
  ;; (setq indent-guide-char ":")
  (setq indent-guide-delay 0.2))

;;; Spell check via wukuo
(use-package wucuo
  :after ispell
  :init
  (add-hook 'prog-mode-hook #'wucuo-start)
  (add-hook 'text-mode-hook #'wucuo-start)
  :config
  (setq my-force-to-use-hunspell t)
  (setq ispell-program-name "hunspell")
  (ispell-check-version)
  (setenv
   "DICPATH"
   "/Users/darkawower/Library/Spelling")
  ;; reset the hunspell so it STOPS querying locale!
  (setq ispell-local-dictionary "LocalDictionary") ; "myhunspell" is key to lookup in `ispell-local-dictionary-alist`
  ;; two dictionaries "en_US" and "zh_CN" are used. Feel free to remove "zh_CN"
  ;; Привет как дла
  (setq ispell-personal-dictionary "~/.doom.d/dictionary/.pws")

  (setq ispell-local-dictionary-alist   ;
        '(("LocalDictionary" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US" "Russian-English") nil utf-8)))
  (setq wucuo-font-faces-to-check '(tree-sitter-hl-face:comment
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
  (setq ispell-hunspell-dict-paths-alist '(("Russian-English" "/Users/darkawower/Library/Spelling/Russian-English.aff")
                                           ("ru_RU" "/Users/darkawower/Library/Spelling/ru_RU.aff")
                                           ("en_US" "/Users/darkawower/Library/Spelling/en_US.aff")))
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))
  (advice-add 'ispell-pdict-save :after (lambda (_arg) (wucuo-spell-check-visible-region))))

;;; Spell check
;; (use-package flyspell
;;   :defer 7
;;   :config
;;   ;; (setq ispell-program-name "aspell")
;;   ;; You could add extra option "--camel-case" for since Aspell 0.60.8
;;   ;; @see https://github.com/redguardtoo/emacs.d/issues/796
;;   ;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
;;   (setq-default flyspell-prog-text-faces
;;                 '(tree-sitter-hl-face:comment
;;                   tree-sitter-hl-face:doc
;;                   tree-sitter-hl-face:string
;;                   tree-sitter-hl-face:function
;;                   tree-sitter-hl-face:variable
;;                   tree-sitter-hl-face:type
;;                   tree-sitter-hl-face:method
;;                   tree-sitter-hl-face:function.method
;;                   tree-sitter-hl-face:function.special
;;                   tree-sitter-hl-face:attribute
;;                   font-lock-comment-face
;;                   font-lock-doc-face
;;                   font-lock-string-face
;;                   lsp-face-highlight-textual
;;                   default))

;;   (setq spell-fu-directory "~/.doom.d/dictionary") ;; Please create this directory manually.
;;   (setq ispell-personal-dictionary "~/.doom.d/dictionary/.pws")
;;   (after! ispell
;;     (setq ispell-program-name "aspell"
;;           ;; Notice the lack of "--run-together"
;;           ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=56"))
;;     (ispell-kill-ispell t))

;;   (defun flyspell-buffer-after-pdict-save (&rest _)
;;     (flyspell-buffer))

;;   (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))
;; (use-package spell-fu
;;   :defer 0.1
;;   :config
;;   (global-spell-fu-mode))

;; (add-hook 'text-mode-hook 'flyspell-mode!)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Autocomplete with AI
(use-package company-tabnine
  :after (company lsp)
  :bind (("C-x C-i" . company-tabnine))
  :when (featurep! :completion company)
  :config
  (setq company-tabnine-always-trigger nil)
  ;; (setq company-tabnine-auto-balance nil)
  (setq company-tabnine-show-annotation t))
;;; Tabnine compay
(defun my-setup-tabnine ()
  (interactive)
  ;; (setq-local +lsp-company-backends '((company-tabnine :separate company-capf)))
  ;; (setq-local company-backends '((company-tabnine :separate company-capf))))
  (setq-local company-backends '((company-tabnine company-capf))))

;;;; Only tabnine
(defun my-setup-tabnine-2 ()
  (interactive)
  (setq-local +lsp-company-backends '((company-tabnine company-dabbrev)))
  (setq-local company-backends '(company-tabnine company-dabbrev)))

(defun my-setup-tabnine-3 ()
  (interactive)
  (setq-local +lsp-company-backends '((company-capf company-dabbrev)))
  (setq-local company-backends '((company-capf company-dabbrev))))


(use-package company
  :defer t
  :bind (:map evil-insert-state-map ("C-'" . company-yasnippet)
         :map company-active-map
         ("<escape>" . (lambda () (interactive)
                         (company-cancel)
                         (evil-normal-state))))
;;; Org mode
(use-package org-modern
  :after org
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  ;; Enable org-modern-mode
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;;; Undo
;; Some problems with preserved data
(use-package undo-tree
  :defer 2
  :custom
  ;; (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist '(("." . "~/tmp/undo")))
  :config
  (global-undo-tree-mode 1))


;;; File manager
;; (use-package ranger
;;   :defer t
;;   :bind (:map evil-normal-state-map
;;          ("SPC r r" . ranger))
;;   :config
;;   (setq ranger-preview-file t)
;;   (setq ranger-footer-delay 0.2)
;;   (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
;;   (setq ranger-show-literal t)
;;   (setq ranger-dont-show-binary t)
;;   (setq ranger-max-preview-size 10)
;;   (setq ranger-preview-delay 0.040))



;; Don't work, and sometime call the big delay
(use-package lsp-sonarlint
  :config
  (require 'lsp-sonarlint-typescript)
  (require 'lsp-sonarlint-python)
  (setq lsp-sonarlint-typescript-enabled t)
  (setq lsp-sonarlint-python-enabled t))

;;; Centaurs tabs
(defun centaur-tabs-hide-tab (x)
  (not (string-match "vterm" (format "%s" (format "%s" x)))))

(use-package centaur-tabs
  :defer 5
  :bind (:map evil-normal-state-map
         ("M-]" . centaur-tabs-forward)
         ("M-[" . centaur-tabs-backward))
  :hook ((vterm-mode vterm-toggle--mode) . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 46
        centaur-tabs-style "bar"
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)

  ;; (defun centaur-tabs-buffer-groups ()
  ;;   (list
  ;;    (cond
  ;;     ((string-match "vterm" (format "%s" (buffer-name))) "Emacs")
  ;;     (t (centaur-tabs-get-group-name (current-buffer))))))

  (defun my-show-only-vterm ()
    (when (bound-and-true-p centaur-tabs-mode)
          (if (string-match "vterm" (format "%s" (buffer-name)))
              (unless centaur-tabs-local-mode
                (centaur-tabs-local-mode))
            (centaur-tabs-local-mode nil))))

  ;; (add-hook! 'window-configuration-change-hook 'my-show-only-vterm)
  (add-hook! 'buffer-list-update-hook 'my-show-only-vterm))


;;;; Nginx company
(use-package company-nginx
  :after nginx-mode
  :config (add-hook 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))
