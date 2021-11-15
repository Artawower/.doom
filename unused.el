;;;; Awesome priority
;; Unused cause can be replaced by ligatures
(use-package org-fancy-priorities
  :defer 3
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "■" "⬇" "❓")))


(use-package lsp-ivy
  :after lsp)

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
