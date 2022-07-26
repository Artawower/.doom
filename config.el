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
;;; Regexp for compilation and qucik error finding
;; (add-to-list 'compilation-error-regexp-alist '("^Error: \\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
(setq compilation-error-regexp-alist '(("^Error: \\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))) ;
;;;; Additional colors
(setq +m-color-main "#61AFEF"
      +m-color-secondary "red")

(setq global-vi-tilde-fringe-mode nil)
;;;;; Browser configs
;; Mac os only
(when (eq system-type 'darwin)
  (setq browse-url-firefox-program "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")
  (setq browse-url-generic-program "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
        browse-url-browser-function 'browse-url-generic))

;;; My custom functions
;;;; Toggle transparency
(defun my-toggle-transparency ()
  "Toggle transparency"
  (interactive)
  (let* ((not-transparent-p (and (boundp 'my-transparency-disabled-p) my-transparency-disabled-p))
         (alpha (if not-transparent-p 100 95)))
    (setq my-transparency-disabled-p (not not-transparent-p))
    (message "%s" alpha)
    (progn
      (set-frame-parameter (selected-frame) 'alpha `(,alpha . ,alpha))
      (add-to-list 'default-frame-alist `(alpha . (,alpha . ,alpha))))))

;;;; Insert TODO for current git branch
(defun my-insert-todo-by-current-git-branch ()
  "Insert todo for current git branch."
  (interactive)
  (let* ((branch-name (magit-get-current-branch))
         (vw (string-match "\\(?1:[A-Za-z0-9]+\/\\)\\(?2:VW-[0-9]+\\)" branch-name))
         (task-number (match-string 2 branch-name))
         (todo-msg (or task-number branch-name)))
    (insert (format "TODO: %s " todo-msg))
    (comment-line 1)
    (previous-line)
    (end-of-line)
    (evil-insert 1)))

;;;; Add additional space before org-insert link function
(defun my-add-additional-space-when-not-exist (_)
  "Add additional sapce if previous char is not space!"
  (unless (eq (char-before) ? )
    (insert " ")))

(advice-add 'org-insert-link :before 'my-add-additional-space-when-not-exist)

(defun my-open-kitty-right-here ()
  "Open or switch kitty to root directory of current project."
  (interactive)
  (let* ((cmd (concat "open -a kitty.app --args \"cd\" " default-directory)))
    (shell-command cmd)))

;;;; Switch default browser
(defun my-switch-to-xwidget-buffer (&optional a b)
  "Switch to xwidget buffer."
  (interactive)
  (switch-to-first-matching-buffer "xwidget webkit"))

(defun my-toggle-default-browser ()
  "Toggle default browser for preview"
  (interactive)
  (if (eq browse-url-browser-function #'browse-url-default-browser)
      (progn (setq browse-url-browser-function #'xwidget-webkit-browse-url)
             (advice-add 'browse-url :after #'my-switch-to-xwidget-buffer))
    (progn
      (setq browse-url-browser-function #'browse-url-default-browser)
      (advice-remove 'browse-url #'my-switch-to-xwidget-buffer))))

;;;; Switch to first finded buffer
(defun switch-to-first-matching-buffer (regex)
  (switch-to-buffer (car (remove-if-not (apply-partially #'string-match-p regex)
                                        (mapcar #'buffer-name (buffer-list))))));;;
(defun my-remove-cr (&optional begin end)
  "Remove line prefixes ending with carriage-return.

BEGIN END specifies region, otherwise works on entire buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (while (re-search-forward "^.*\033\\[2K\033\\[1G" end t)
      (replace-match ""))))

(defun my-copy-pwd ()
  "Copy PWD command to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (replace-regexp-in-string " " "\\\\\  " (file-name-directory (buffer-file-name))))))

(defun my-copy-full-path ()
  "Copy full path till file to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (replace-regexp-in-string " " "\\\\\  " (buffer-file-name)))))


;;;; Maximize current buffer
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

;;;; Change vterm directory to current buffer pwd
(defun my-vterm-change-current-directory-to-active-buffer-pwd ()
  "Just exec CD to pwd of active buffer."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (file-dir (file-name-directory file-name))
              (file-dir (replace-regexp-in-string " " "\\\\\  " file-dir)))
    (message "FILE: %s" file-dir)
    (save-window-excursion
      (switch-to-first-matching-buffer "vterm")
      (vterm-send-C-c)
      (vterm-send-string (concat "cd " file-dir))
      (vterm-send-return)
      )
    (evil-window-down 1)))

;;;; Browse current git file at remote machine
(defun my-forge-browse-buffer-file ()
  (interactive)
  (browse-url
   (let
       ((rev (magit-rev-abbrev "HEAD"))
        (repo (forge-get-repository 'stub))
        (file (magit-file-relative-name buffer-file-name))
        (highlight
         (if
             (use-region-p)
             (let ((l1 (line-number-at-pos (region-beginning)))
                   (l2 (line-number-at-pos (- (region-end) 1))))
               (format "#L%d-L%d" l1 l2))
           ""
           )))
     (forge--format repo "https://%h/%o/%n/blob/master/%f%L"
                    `((?r . ,rev) (?f . ,file) (?L . ,highlight))))))
;;;; My sass format
(defun my-run-sass-auto-fix ()
  "Run sass auto fix if cli tool exist"
  (interactive)
  (save-window-excursion
    (let ((default-directory (file-name-directory buffer-file-name)))
      (async-shell-command "sass-lint-auto-fix")
      ;; (revert-buffer-no-confirm)
      (message "SASS FORMATTED"))))
;;; Transparent bg
(progn
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90))))

;;; Fonts

(set-frame-font "JetBrainsMono Nerd Font 17" nil t)
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

(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)



;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;;;; Benchmark
(use-package explain-pause-mode
  :defer t)

;;; Centaur tabs
;; (defun centaur-tabs-hide-tab (x)
;;   (not (string-match "vterm" (format "%s" (format "%s" x)))))

;; (use-package centaur-tabs
;;   :defer 5
;;   :bind (:map evil-normal-state-map
;;          ("M-]" . centaur-tabs-forward)
;;          ("M-[" . centaur-tabs-backward))
;;   :hook ((vterm-mode vterm-toggle--mode) . centaur-tabs-local-mode)
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-height 46
;;         centaur-tabs-style "bar"
;;         centaur-tabs-set-icons t
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-show-navigation-buttons t
;;         centaur-tabs-set-bar 'under
;;         x-underline-at-descent-line t)

;;   ;; (defun centaur-tabs-buffer-groups ()
;;   ;;   (list
;;   ;;    (cond
;;   ;;     ((string-match "vterm" (format "%s" (buffer-name))) "Emacs")
;;   ;;     (t (centaur-tabs-get-group-name (current-buffer))))))

;;   (defun my-show-only-vterm ()
;;     (when (bound-and-true-p centaur-tabs-mode)
;;           (if (string-match "vterm" (format "%s" (buffer-name)))
;;               (unless centaur-tabs-local-mode
;;                 (centaur-tabs-local-mode))
;;             (centaur-tabs-local-mode nil))))

;;   ;; (add-hook! 'window-configuration-change-hook 'my-show-only-vterm)
;;   (add-hook! 'buffer-list-update-hook 'my-show-only-vterm))



;;; Completion
(use-package ivy
  :defer t
  :bind (:map ivy-mode-map
         ("C-<return>" . ivy-immediate-done)
         ("C-d" . (lambda ()
                    (interactive)
                    (ivy-kill-whole-line)
                    (ivy--cd "~/")))
         ("C-a" . (lambda ()
                    (interactive)
                    (ivy-kill-whole-line)
                    (ivy--cd "~/org/")))
         ("C-r" . (lambda ()
                    (interactive)
                    (ivy-kill-whole-line)
                    (ivy--cd "/")))))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :after ivy
  :config)

(use-package counsel-projectile
  :defer 0.5
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; (use-package all-the-icons-ivy-rich
;;   :defer 0.5)
(defun @ivy-rich-prettify-file-search ()
  (let* ((col-def '((all-the-icons-ivy-rich-file-icon)
                    (file-name-nondirectory (:width 0.2 :face (:foreground "#61AFEF" :slant 'italic)))
                    ((lambda (str) (string-join (butlast (split-string (counsel-projectile-find-file-transformer str) "/")) "/")) (:width 0.4))
                    ;; (counsel-projectile-find-file-transformer (:width 0.4))
                    (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
                    (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
                    (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
                    (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))))
    (set-face-attribute 'all-the-icons-ivy-rich-doc-face nil :font "ChalkBoard SE 13" :italic t :height 136)
    (ivy-rich-set-columns 'projectile-find-file col-def)
    (ivy-rich-set-columns 'counsel-projectile-find-file col-def)
    (ivy-rich-set-columns 'projectile--find-file col-def)))

(use-package all-the-icons-ivy-rich
  :after (all-the-icons ivy-rich counsel-projectile)
  :config
  (run-at-time "3 sec" nil #'@ivy-rich-prettify-file-search)
  (all-the-icons-ivy-rich-mode 1))


(defun posframe-poshandler-frame-top-center-with-offset (info)
  "Posframe position at top + 40px offset."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        40))

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

;;; Formatter
;; Improve counsel search (async)
(use-package format-all
  :defer 1.5
  ;; :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode go-mode) . format-all-mode)
  :hook ((json-mode go-mode) . format-all-mode)
  :config
  (add-to-list '+format-on-save-enabled-modes 'typescript-mode t)
  (add-to-list '+format-on-save-enabled-modes 'ng2-mode t)
  (add-to-list '+format-on-save-enabled-modes 'js2-mode t))

;; (defun my-ecmascript-formatter ()
;;   "My custom chaif of formatters for ecmascript and html."
;;   (interactive)
;;   (prettier-prettify)
;;   (+format/buffer))

;; (defun my-install-formatter ()
;;   (add-hook 'before-save-hook 'my-ecmascript-formatter))

;; TODO: check problem with file corruption
(use-package prettier
  :defer 1.5
  :hook ((js2-mode typescript-mode ng2-html-mode vue-mode web-mode) . prettier-mode))

;; (use-package prettier-js
;;   :defer 0.3)
;; :hook ((js2-mode typescript-mode ng2-html-mode vue-mode web-mode) . prettier-js-mode))
;; :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode) . prettier-js-mode)



;;; UI

;;; Flycheck
(use-package flycheck
  :defer 2
  :bind (:map evil-normal-state-map
         ("SPC f ]" . flycheck-next-error)
         ("SPC f [" . flycheck-previous-error)))
;;; Smartparens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;; NOTE: this package is used instead of electric pair mode
;; cause its simple, and it works in all cases.
(use-package autopair
  :defer 5
  :config
  (autopair-global-mode))


;;; Undo

(use-package vundo
  :defer 1
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Better contrasting highlight.
  (custom-set-faces
    '(vundo-node ((t (:foreground "#808080"))))
    '(vundo-stem ((t (:foreground "#808080"))))
    '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

(use-package undo-fu-session
  :defer 1
  :config
  (global-undo-fu-session-mode))

(with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "C-M-u") 'vundo))

;;; Spellcheck via spell-fu
;;;; Enable spell fu for tree sitter face
(defun my-set-spellfu-faces ()
  "Set faces for correct spell-fu working"
  (interactive)
  (setq spell-fu-faces-include '(tree-sitter-hl-face:comment
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
  (setq spell-fu-faces-exclude (append spell-fu-faces-exclude
                                       '(diredfl-file-name))))
(use-package spell-fu
  :bind (:map evil-normal-state-map
         ("z g" . spell-fu-word-add))
  :defer 1
  :config
  (setq ispell-program-name "aspell")
  (setq spell-fu-directory "~/.doom.d/dictionary")
  (setq ispell-program-name "aspell"
        ;;           ;; Notice the lack of "--run-together"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=56"))
  (setq spell-fu-ignore-modes '(org-mode dired-mode vterm-mode elfeed-search-mode))

  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "ru"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "en-personal" "/Users/darkawower/.doom.d/dictionary/.pws"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "ru-personal" "/Users/darkawower/.doom.d/dictionary/ru.pws"))))

  ;; Camel case support
  (setq-default spell-fu-word-regexp
                (rx
                 (or
                  ;; lowercase
                  (seq
                   (one-or-more lower)
                   (opt
                    (any "'’")
                    (one-or-more lower)
                    word-end))

                  ;; capitalized
                  (seq
                   upper
                   (zero-or-more lower)
                   (opt
                    (any "'’")
                    (one-or-more lower)
                    word-end))

                  ;; uppercase
                  (seq
                   (one-or-more upper)
                   (opt
                    (any "'’")
                    (one-or-more upper)
                    word-end)))))

  (defun cs/spell-fu-check-range (pos-beg pos-end)
    (let (case-fold-search)
      (spell-fu-check-range-default pos-beg pos-end)))

  (setq-default spell-fu-check-range #'cs/spell-fu-check-range)
  (global-spell-fu-mode)
  (my-set-spellfu-faces))


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
  :defer 10
  :bind (:map treemacs-mode-map
         ("@" . evil-execute-macro))
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

;;; Files
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;; Maybe the icons are too big to your eyes
  (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  (dirvish-peek-mode)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dirvish-reuse-session t)
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-default-layout '(1 0.3 0.7))
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  ;; Make sure to use the long name of flags when exists
  ;; eg. use "--almost-all" instead of "-A"
  ;; Otherwise some commands won't work properly
  ;; (dirvish-define-preview exa (file)
  ;;   "Use `exa' to generate directory preview."
  ;;   :require ("exa") ; tell Dirvish to check if we have the executable
  ;;   (when (file-directory-p file) ; we only interest in directories here
  ;;     `(shell . ("exa" "--color=always" "-al" ,file))))
                                        ; use the command output as preview

  ;; (add-to-list 'dirvish-preview-dispatchers 'exa)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (setq dirvish-attributes '(vc-state subtree-state collapse git-msg file-size))
  (advice-add #'+dired/quit-all :after (lambda () (interactive) (dirvish-kill (dirvish-prop :dv))))
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dired-mode-map ; Dirvish respects all the keybindings in this map
   ("h" . dired-up-directory)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("l" . dired-find-file)
   ("i" . wdired-change-to-wdired-mode)
   ("." . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
   ("?"   . dirvish-dispatch)  ; remapped `dired-summary'
   ("TAB" . dirvish-subtree-toggle)
   ("SPC" . dirvish-history-jump)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; (setq dired-kill-when-opening-new-dired-buffer t) ;; added in emacs 28
;; (setq dired-clean-confirm-killing-deleted-buffers nil)
;; (setq large-file-warning-threshold 50000000)
;; (setq dired-recursive-copies 'always)
;; (setq dired-recursive-deletes 'always)
;; (setq delete-by-moving-to-trash t)
;; (setq dired-dwim-target t)
;; (setq dired-listing-switches "-AGhlv --group-directories-first --time-style=long-iso"))

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


;;; Bookmarks
;;;; quick bm
(use-package bm
  :defer t
  :custom-face
  (bm-face ((t (:foreground ,+m-color-secondary))))
  :bind (("C-M-n" . bm-next)
         ("C-M-p" . bm-previous)
         ("s-b" . bm-toggle)))

;;;; Doom bm
(use-package bookmark
  :defer t
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file "~/.doom.d/bookmarks"))


;;; Terminal
;; (set-face-attribute 'fixed-pitch nil ':font "Fira Code 14")
;; (add-hook 'vterm-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
;;             (buffer-face-mode t)))

;; https://github.com/jixiuf/vterm-toggle/issues/30
(use-package vterm-toggle
  :defer 10
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
         ("SPC o h" . (lambda () (interactive)
                        (+vterm/toggle t)))
         ("SPC t h" . vterm-toggle-hide)
         ("SPC t k" . my-open-kitty-right-here))
  :config
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-toggle-scope 'project))


;;; Colors
(use-package rainbow-mode
  :hook (((css-mode scss-mode org-mode typescript-mode js-mode emacs-lisp-mode). rainbow-mode))
  :defer 5)

(use-package hl-todo
  :defer 2
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#E5C07B")
          ("FIXME"  . "#E06C75")
          ("DEBUG"  . "#C678DD")
          ("REFACTOR"  . "#C678DD")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#98C379")
          ("QUESTION"   . "#98C379")
          ("STUB"   . "#61AFEF")))
  (global-hl-todo-mode 1))

;;; Themes
;;; Modus
(use-package modus-themes
  :defer t)

(use-package auto-dark
  :config
  (setq auto-dark--dark-theme 'doom-moonlight)
  (setq auto-dark--light-theme 'pinky-winky))


;;;; Theme switcher
(use-package heaven-and-hell
  :after doom-themes
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . pinky-winky)
          (dark . tokio-night)))
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
  :defer 2
  :config
  (global-wakatime-mode))


;;; Fast commenting
(use-package turbo-log
  :defer 15
  :bind (("C-s-l" . turbo-log-print)
         ("C-s-i" . turbo-log-print-immediately)
         ("C-s-h" . turbo-log-comment-all-logs)
         ("C-s-s" . turbo-log-uncomment-all-logs)
         ("C-s-x" . turbo-log-delete-all-logs)
         ("C-s-[" . turbo-log-paste-as-logger )
         ("C-s-]" . turbo-log-paste-as-logger-immediately))
  :custom
  (turbo-log-allow-insert-without-tree-sitter-p t)
  ;; (turbo-log-payload-format-template "")
  (turbo-log-payload-format-template "\x1b[35m%s: ")
  :config
  (turbo-log-configure
   :modes (typescript-mode js2-mode web-mode ng2-ts-mode js-mode)
   :strategy merge
   :post-insert-hooks (prettier-prettify lsp)
   :msg-format-template "'🦄: %s'"))

;;; Quickly type converting
(use-package quicktype
  :defer 15
  :bind (("C-x j v" . quicktype-json-to-type)
         ("C-x j p" . quicktype-paste-json-as-type)
         ("C-x j q" . quicktype)))

;;; Programming
;; Common configurations for all programming languages
;;;; Lsp


;; Don't work, and sometime call the big delay
;; (use-package lsp-sonarlint
;;   :config
;;   (require 'lsp-sonarlint-typescript)
;;   (require 'lsp-sonarlint-python)
;;   (setq lsp-sonarlint-typescript-enabled t)
;;   (setq lsp-sonarlint-python-enabled t))

(use-package lsp
  :hook ((clojure-mode
          scss-mode
          go-mode
          css-mode
          js-mode
          typescript-mode
          vue-mode
          web-mode
          ng2-html-mode
          ng2-ts-mode
          python-mode
          typescript-tsx-mode) . lsp-deferred)
  :bind (:map evil-normal-state-map
         ("SPC f n" . flycheck-next-error)
         ("g i" . lsp-goto-implementation)
         ("SPC l a" . lsp-execute-code-action))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 4000)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  (lsp-yaml-schemas '((kubernetes . ["/auth-reader.yaml", "/deployment.yaml"])))
  ;; (setq lsp-enable-file-watchers t)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 5000)
  ;; (lsp-yaml-schemas '(:kubernetes "/.yaml" :kubernetes "/*.yml"))
  :config
  (setq lsp-javascript-display-return-type-hints t)
  (setq lsp-json-schemas
        `[
          (:fileMatch ["ng-openapi-gen.json"] :url "https://raw.githubusercontent.com/cyclosproject/ng-openapi-gen/master/ng-openapi-gen-schema.json")
          (:fileMatch ["package.json"] :url "http://json-schema.org/draft-07/schema")
          ])
  (set-face-attribute 'lsp-face-highlight-read nil :background "#61AFEF")
  ;; Flycheck patch checkers
  (require 'flycheck)
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  ;; Golang
  (defun lsp-go-install-save-hooks ()
    (flycheck-add-next-checker 'lsp '(warning . go-gofmt) 'append)
    (flycheck-add-next-checker 'lsp '(warning . go-golint))
    (flycheck-add-next-checker 'lsp '(warning . go-errcheck))
    (flycheck-add-next-checker 'lsp '(warning . go-staticcheck))

    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)

  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  ;; (add-hook 'emacs-lisp-mode-hook #'(lambda ()
  ;;                                     (setq company-backends '(company-tabnine company-dabbrev))))


  ;; (setq +lsp-company-backends '(company-tabnine company-capf))
  ;; (setq company-backends '((company-tabnine :separate company-capf)))
  ;;
  ;; (setq +lsp-company-backends '(company-dabbrev company-capf))
  ;; (setq company-backends '((company-dabbrev company-capf)))

  (setq lsp-disabled-clients '(html html-ls))
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\pyenv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (setq lsp-eldoc-hook nil))


(use-package lsp-yaml
  :defer t
  :hook (yaml-mode . lsp-mode))
;; :custom
;; (lsp-yaml-schemas '(:kubernetes "https://raw.githubusercontent.com/instrumenta/kubernetes-json-schema/master/v1.18.0-standalone-strict/all.json")))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map evil-normal-state-map
         ("SPC l r" . lsp-find-references))
  :config
  (setq lsp-ui-sideline-diagnostic-max-line-length 100
        lsp-ui-sideline-diagnostic-max-lines 5
        lsp-ui-doc-delay 2
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-border +m-color-main))

;;;; Syntax highlight
(use-package tree-sitter-langs
  :defer 3)

(use-package tree-sitter
  :after (tree-sitter-langs spell-fu)
  :hook ((go-mode typescript-mode css-mode typescript-tsx-mode html-mode scss-mode ng2-mode js-mode python-mode rust-mode ng2-ts-mode ng2-html-mode) . tree-sitter-hl-mode)
  :config
  (setq tsc-dyn-get-from '(:compilation))
  (advice-add 'tree-sitter-hl-mode :before 'my-set-spellfu-faces)
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (push '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . css) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . typescript) tree-sitter-major-mode-language-alist)
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package tree-edit
  :defer t)

(use-package evil-tree-edit
  :after tree-edit)


;;;; Company
(use-package company
  :defer t
  :bind (:map evil-insert-state-map ("C-'" . company-yasnippet)
         :map company-active-map
         ("<escape>" . (lambda () (interactive)
                         (company-cancel)
                         (evil-normal-state))))
  :config
  (setq company-idle-delay 0.2)
  (setq company-show-numbers nil)
  (setq company-quick-access-modifier 'super)
  (setq company-show-quick-access t)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-char-regexp "[A-z:-]"))


;;;; Copilot
(use-package copilot
  :defer 1
  :bind
  ("s-]" . copilot-next-completion)
  ("s-[" . copilot-previous-completion)
  ("s-l" . copilot-accept-completion)
  ("s-j" . copilot-complete)
  ("s-;" . copilot-accept-completion-by-word)
  :custom
  (copilot-idle-delay 0.05)
  :config
  (setq copilot--previous-point nil)
  (setq copilot--previous-window-width nil)
  (copilot-diagnose)

  (defun copilot--preserve-positions ()
    (setq copilot--previous-point (point))
    (setq copilot--previous-window-width (blamer--real-window-width)))

  (defun copilot--positions-changed-p ()
    (or (not (equal (point)  copilot--previous-point))
        (not (equal (window-width) copilot--previous-window-width))))


  (defun copilot--rerender ()
    (when-let ((copilot--changed (copilot--positions-changed-p)))
      (copilot-clear-overlay)
      (copilot--preserve-positions)
      (blamer--clear-overlay)
      (when (evil-insert-state-p) (copilot-complete))))

  (add-hook 'post-command-hook #'copilot--rerender)
  ;; (add-hook 'evil-insert-state-exit-hook 'copilot-clear-overlay)
  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p t)
                                            (blamer--clear-overlay)))
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (message "Okay, now blamer should works correctly!")
                                            (setq blamer--block-render-p nil)
                                            (copilot-clear-overlay)))
  ;; (copilot-clear-overlay)) nil t)
  )



;;; Languages
;;;; Lisp

(use-package elisp-mode
  :defer t
  :hook ((emacs-lisp-mode . paren-face-mode)
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
  :defer t)

(use-package package-lint
  :defer t)

;;;; Typescript
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package typescript-mode
  :defer 10
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\.ts\'" . typescript-mode))
  (setq compilation-error-regexp-alist '(("^Error: \\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))

;;;;; Angular
(use-package ng2-mode
  :after typescript-mode
  :hook (ng2-html-mode . web-mode)
  :init
  (setq lsp-clients-angular-language-server-command
        '("/opt/homebrew/opt/node@14/bin/node"
          "/opt/homebrew/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/opt/homebrew/lib/node_modules"
          ;; "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          ;; "/usr/local/lib/node_modules"
          "/opt/homebrew/lib/node_modules"
          "--stdio"))
  :config
  ;; (add-to-list 'lsp-disabled-clients 'deno-ls)
  (setq lsp-clients-angular-language-server-command
        '("/opt/homebrew/opt/node@14/bin/node"
          "/opt/homebrew/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/opt/homebrew/lib/node_modules"
          ;; "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          ;; "/usr/local/lib/node_modules"
          "/opt/homebrew/lib/node_modules"
          "--stdio"))
  (setq compilation-error-regexp-alist '(("^Error: \\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))


;;;; Javascript
(use-package js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode))

(use-package npm
  :defer t)

;;;; Golang
(use-package go-playground
  :defer t)

;;;; Rust
(use-package rustic
  :defer t
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
  :defer t
  :hook (python-mode . pipenv-mode)
  :config
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/.local/share/virtualenvs"))
  (add-hook 'pyvenv-post-activate-hooks #'lsp-restart-workspace)
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))



(use-package python-mode
  :defer t
  :hook (python-mode . format-all-mode)
  :config
  (setq pytnon-indent-level 4)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4))))


(setq lsp-pyright-multi-root nil)
(use-package lsp-pyright
  :defer t
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-venv-directory "/Users/darkawower/.local/share/virtualenvs/spice-farm-YhO8T07I")
  (setq lsp-pyright-diagnostic-mode "workspace"))


;;;; Vue
;; npm install -g @volar/server
;; Check it
(use-package! lsp-volar
  :after lsp-mode)

;;;; Web mode
(use-package web-mode
  :defer t
  :mode (("\\.vue\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil)
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
  :defer t)

;;;; Html
(use-package emmet-mode
  :hook ((scss-mode . emmet-mode) (css-mode . emmet-mode) (ng2-html-mode . emmet-mode) (html-mode . emmet-mode))
  :defer 5)

;;;; Scss
(use-package css-mode
  :defer 10
  :config
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defun run-sass-auto-fix ()
    "Run sass auto fix if cli tool exist"
    (interactive)
    (save-window-excursion
      (let ((default-directory (file-name-directory buffer-file-name)))
        (async-shell-command "sass-lint-auto-fix")
        ;; (revert-buffer-no-confirm)
        (message "SASS FORMATTED")
        )))
  ;; (add-hook 'scss-mode-hook '(lambda () (add-hook 'after-save-hook #'run-sass-auto-fix t t)))
  (add-hook 'scss-mode-hook '(lambda () (add-hook 'before-save-hook #'format-all-buffer nil t))))

;;;; Json
(use-package json-mode
  :defer 5
  :hook (json-mode . format-all-mode))

;;;; Lua
(use-package lua-mode
  :defer t)

;;;; Debug
(use-package dap-mode
  :defer 3
  :bind (:map evil-normal-state-map
         ("SPC d n" . dap-next)
         ("SPC d i" . dap-step-in)
         ("SPC d o" . dap-step-out)
         ("SPC d c" . dap-continue)
         ("SPC d Q" . dap-disconnect)
         ("SPC d q" . dap-disconnect)
         ("SPC d d" . (lambda () (interactive)
                        (call-interactively #'dap-debug)
                        (set-window-buffer nil (current-buffer))))
         ("SPC d r" . dap-debug-recent)
         ("SPC d l" . dap-ui-locals)
         ("SPC d b" . dap-ui-breakpoints)
         ("SPC d s" . dap-ui-sessions)
         ("SPC d e" . dap-debug-last)
         ("SPC d p" . (lambda () (interactive)
                        (set-window-buffer nil (current-buffer))
                        (dap-breakpoint-toggle)))
         ("SPC d e" . dap-debug-edit-template))
  :config
  (dap-mode 1)
  (setq dap-auto-configure-features '(sessions locals))
  (require 'dap-go))

;;; Infrastructure
;;;; Docker compose
(use-package docker-compose-mode
  :defer t)

;;;; Docker
(use-package dockerfile-mode
  :defer t
  :config
  (add-hook 'compilation-filter-hook #'my-remove-cr -90))

;;;; Jenkins
(use-package jenkinsfile-mode
  :defer t
  :config)

;;;; Nginx
;; (use-package company-nginx
;;   :after nginx-mode
;;   :config (add-hook 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))

;;;; Kuber
(use-package kubernetes
  :defer 6
  :commands (kubernetes-overview)
  :bind (:map evil-normal-state-map
         ("SPC o K" . kubernetes-overview))
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :after kubernetes)

(use-package k8s-mode
  :defer t)

(use-package nginx-mode
  :defer 10)

;;; Markup
;;;; Jinja
(use-package jinja2-mode
  :defer t)
;;;; Markdown
(use-package grip-mode
  :after markdown-mode
  ;; :hook (markdown-mode . grip-mode)
  :custom
  (browse-url-browser-function 'browse-url-generic)
  ;; (grip-url-browser #'browse-url-firefox-program)
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

(use-package auto-rename-tag
  :defer t
  :hook ((html-mode ng2-html-mode-hook vue-mode web-mode) . auto-rename-tag-mode)
  :config
  (auto-rename-tag-mode 1))

;;; Help tools
;;;; PASCAL_CASE -> camelCase -> snake_case
(use-package string-inflection
  :defer t
  :bind ("C-s-c" . string-inflection-all-cycle))

;;;; Indention

;;; Google translate
(use-package google-translate
  :defer 30
  :bind
  (:map google-translate-minibuffer-keymap
   ("C-k" . google-translate-next-translation-direction)
   ("C-n" . google-translate-next-translation-direction)
   ("C-l" . google-translate-next-translation-direction))
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-backend-method 'curl)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-translation-directions-alist
        '(("en" . "ru") ("ru" . "en") ))
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

;;; Git
(use-package magit
  :defer t
  :bind (:map magit-mode-map
         ("s-<return>" . magit-diff-visit-file-worktree)
         :map evil-normal-state-map
         ("SPC g i" . (lambda () (interactive) (wakatime-ui--clear-modeline) (magit-status))))
  :config
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq)
  (advice-add
   'ansi-color-apply-on-region
   :before
   #'my-remove-cr)
  (setq magit-process-finish-apply-ansi-colors t))


(use-package gist                       ;
  :defer t
  :bind (:map gist-list-menu-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . gist-fork)
         ("x" . gist-kill-current)
         ("f" . avy-goto-word-1)
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

(use-package code-review
  :defer t
  :config
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer))

(use-package git-gutter
  :defer 10
  :init
  (global-git-gutter-mode)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk))

;;; Blamer
(use-package blamer
  :defer 5
  :bind (
         ("C-c i" . blamer-show-commit-info)
         ("C-c h" . (lambda () (interactive) (blamer-show-commit-info 'visual)))
         ("s-i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.8)
  (blamer-min-offset 20)
  (blamer-max-commit-message-length 65)
  ;; (blamer-commit-formatter "• %s")
  ;; (blamer-commit-formatter nil)
  ;; (blamer-entire-formatter "   %s")
  ;; (blamer-entire-formatter "  • %s")
  ;; (blamer-offset-per-symbol 17)
  ;; (blamer-view 'overlay-right)
  (blamer-commit-formatter "◉ %s")
  (blamer-view 'overlay)
  ;; (blamer-uncommitted-changes-message "(งツ)
  (blamer-uncommitted-changes-message "uncommitted yet")
  ;; (blamer-min-offset 10)
  :custom-face
  (blamer-face ((t :inherit company-preview
                   :italic t
                   :font "Fira Code 14"
                   :height 0.9
                   :background nil)))
  :config
  (tooltip-mode)
  (setq blamer-tooltip-function 'blamer-tooltip-commit-message)


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

  ;; (advice-add 'blamer--clear-overlay :before 'copilot-complete)
  (global-blamer-mode 1))

(use-package sideline
  :hook (lsp-mode-hook . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-lsp))
  :defer 2)

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

;;;; Global keybinding
(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "s-y") 'yas-expand)
(global-set-key (kbd "<C-S-up>") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'enlarge-window)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-c l") 'smerge-keep-lower)
(global-set-key (kbd "C-c u") 'smerge-keep-upper)
(global-set-key (kbd "C-c a") 'smerge-keep-all)
(global-set-key (kbd "C-c j") 'smerge-next)
(global-set-key (kbd "C-c k") 'smerge-prev)

(global-set-key (kbd "s-e") 'emmet-expand-line)
(global-set-key (kbd "C-s") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC w w") 'ace-window)

;;; Navigation
;;;; Evil
(use-package avy
  :defer t
  :bind (:map evil-normal-state-map
         ("SPC k l" . avy-kill-whole-line)
         ("SPC k r" . avy-kill-region))
  :custom
  (avy-single-candidate-jump t)
  (avy-keys '(?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)))

(use-package evil-leader
  :after evil
  :bind (:map evil-normal-state-map
         ("f" . avy-goto-word-1)
         ("SPC n r f" . org-roam-node-find)
         ("SPC t a" . treemacs-add-project-to-workspace)
         ("SPC g t" . git-timemachine)
         ;; Compilation
         ("SPC c v" . compilation-display-error)
         ;; CUSTOM
         ("SPC t i" . my-insert-todo-by-current-git-branch)
         ;; Org mode
         ("SPC d t" . org-time-stamp-inactive)
         ("SPC d T" . org-time-stamp)
         ("SPC r p" . +python/open-ipython-repl)
         ("SPC r n" . nodejs-repl)
         ("SPC t t" . ivy-magit-todos)
         ;; TODO: add check might be roam buffer already opened?
         ("SPC r u" . (lambda () (interactive)
                        (org-roam-ui-open)
                        (run-at-time "0.3 sec" nil (lambda () (org-roam-ui-sync-theme) (my-switch-to-xwidget-buffer)))))
         ("SPC j" . ace-window)
         ("SPC w f" . ace-window)
         ("s-Y" . xah-paste-from-register-1)
         ("s-p" . yank-from-kill-ring)
         ("s-r" . (lambda () (interactive) (set-mark-command nil) (evil-avy-goto-char)))
         ("SPC y k" . yank-from-kill-ring)
         ("s-." . ace-window)
         ;; Git
         ("SPC g o f" . my-forge-browse-buffer-file)
         :map evil-insert-state-map
         ("s-Y" . xah-copy-to-register-1)
         ;; ("s-2" . xah-paste-from-register-1)
         ("s-p" . yank-from-kill-ring)
         ("s-." . ace-window)
         :map evil-visual-state-map
         ("s-Y" . xah-copy-to-register-1)
         ("s-P" . xah-paste-from-register-1)
         ("s-." . ace-window)
         :map ivy-mode-map
         ("s-Y" . xah-copy-to-register-1)
         ("s-P" . xah-paste-from-register-1))
  :init
  (global-evil-leader-mode)
  :config
  (setq-default evil-kill-on-visual-paste nil)
  (evil-leader/set-key
    ;; "f" 'evil-find-char
    "f" 'avy-goto-char
    "b" 'my-switch-to-xwidget-buffer
    "x" 'my-ecmascript-formatter
    "k" 'save-buffer-without-dtw
    "w" 'avy-goto-word-0
    "]" 'flycheck-next-error
    "[" 'flycheck-previous-error

    "d" 'dap-debug
    "\\" 'ace-window

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
    "g" 'ace-window
    ;; Evil
    "=" 'evil-record-macro
    "-" 'evil-execute-macro
    "0" 'my-toggle-default-browser
    ;; "=" 'kmacro-start-macro-or-insert-counter
    ;; Lsp
    "h" 'lsp-ui-doc-show
    "e" 'lsp-treemacs-errors-list
    "l" 'lsp-execute-code-action

    "r" 'treemacs-select-window

    "m" 'toggle-maximize-buffer
    "y" 'yas-expand))


(use-package evil-matchit
  :defer t)

(evilmi-load-plugin-rules '(ng2-html-mode) '(html))
(global-evil-matchit-mode 1)


;;; Org mode
(use-package prg-crypt
  :defer t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :defer t
  ;; :demand t
  :bind (:map evil-normal-state-map
         ("SPC h ]" . org-next-visible-heading)
         ("SPC h [" . org-previous-visible-heading))
  :config
  (progn
    (define-key org-mode-map "\C-x a f" "\C-x h \C-M-\\ \C-c")
    (custom-set-faces
     '(org-document-title ((t (:inherit outline-1 :height 2.5))))
     '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

    (setq org-hide-emphasis-markers t)

    (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))

    ;; Increase priorities count
    (setq org-highest-priority ?A
          org-default-priority ?C
          org-lowest-priority ?E)


    (defun publish-org-blog()
      "Publish this note to du-blog!"
      (interactive)
      (require 'ox-gfm)
      (setq org-export-with-sub-superscripts '{})
      (defun org-gfm-format-toc (headline) "")
      (org-gfm-export-to-markdown)
      (let ((file-path (replace-regexp-in-string " " "\\\\\  " (buffer-file-name))))

        (message (concat
                  "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js"
                  file-path))
        (shell-command
         (concat
          "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js "
          file-path))))

    (setenv "NODE_PATH"
            (concat
             (getenv "HOME") "/org-node/node_modules"  ":"
             (getenv "NODE_PATH")))

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



(use-package svg-tag-mode
  :defer 7
  :hook (org-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :beg 1 :end -1)))))))


;;;; Org mode todos list
(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"     ; A task that needs doing & is ready to do
           "PROJ(p)"     ; A project, which usually contains other tasks
           "IDEA(i)"     ; Idea
           "PROGRESS(s)" ; A task that is in progress
           "WAIT(w)"     ; Something external is holding up this task
           "TEST(c)"     ; In TEST statement
           "BLOCK(b)"    ; task blocked
           "REJECTED(x)" ; somebody rejected idea :(
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
          ("PROGRESS"   . org-todo)
          ("DONE"       . org-todo)
          ("IDEA"       . org-todo)
          ("[?]"        . +org-todo-onhold)
          ("WAIT"       . +org-todo-onhold)
          ("TEST"       . +org-todo-active)
          ("FEEDBACK"   . +org-todo-onhold)
          ("REVIEW"     . +org-todo-onhold)
          ("HOLD"       . +org-todo-onhold)
          ("PROJ"       . +org-todo-project)
          ("BLOCK"       . +org-todo-cancel)
          ("REJECTED"       . +org-todo-cancel)
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
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  (cl-defmethod org-roam-node-mtitle ((node org-roam-node))
    "Return customized title of roam node"
    (let* ((tags (org-roam-node-tags node))
           (title (org-roam-node-title node)))
      (if (not tags)
          title
        (setq joined-text (string-join tags ", "))
        (concat (propertize (format "(%s) " joined-text) 'face `(:foreground ,+m-color-main :weight bold :slant italic)) title))))
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-node-display-template "${mtitle:100}")
  (setq org-roam-directory (file-truename "~/org-roam"))
  (org-roam-db-autosync-mode))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-roam-ui-browser-function #'xwidget-webkit-browse-url))

(use-package! web-roam
  :defer t
  :bind (:map evil-normal-state-map
         ("SPC n p" . web-roam-publish-file))
  :hook (org-mode . web-roam-sync-mode))

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
                           (push '("#+STARTUP:" . "") prettify-symbols-alist)
                           (push '("#+ACTIVE:" . "") prettify-symbols-alist)
                           (push '("#+START_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
                           (push '("#+END_HIDDEN" . "") prettify-symbols-alist)
                           (prettify-symbols-mode)))

(use-package org-fancy-priorities
  :after org
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "🔥")
                                    (?B . "⬆")
                                    (?C . "❗")
                                    (?D . "⬇")
                                    (?E . "❓")
                                    (?1 . "🔥")
                                    (?2 . "⚡")
                                    (?3 . "⮮")
                                    (?4 . "☕")
                                    (?I . "Important"))))

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
  :defer 4
  :custom
  ;; (wakatim-ui-schedule-url "https://wakatime.com/share/@darkawower/bb8cf0d7-3554-4297-ac4d-01f8a155073c.svg")
  (wakatim-ui-schedule-url "https://wakatime.com/share/@darkawower/af1bfb85-2c8b-44e4-9873-c4a91b512e8d.png")
  :config
  (wakatime-ui-mode))

;;;; Messanging
(use-package telega
  :defer t
  :bind (:map evil-normal-state-map
         ("SPC t v" . telega)
         :map telega-prefix-map
         ("g" . telega-filter-by-folder))
  :config
  (require 'telega-alert)
  (setq telega-server-libs-prefix "/opt/homebrew")
  (setq telega-chat-fill-column 190)
  (setq telega-use-docker t)
  (setq telega-accounts (list
                         (list "demonnsd" 'telega-database-dir telega-database-dir)
                         (list "artawower" 'telega-database-dir (expand-file-name "artawower" telega-database-dir))
                         (list "ksenofobious" 'telega-database-dir (expand-file-name "ksenofobious" telega-database-dir))))
  (telega-alert-mode 1))

;;; COlloboration
(use-package floobits
  :defer t)
;;; Reading
;;;; RSS
(use-package elfeed
  :defer 30
  :config
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (setq-default elfeed-search-filter "@12-hours-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100)
  (setq browse-url-browser-function #'browse-url-default-browser))

(add-hook 'before-save-hook (lambda (set-buffer-file-coding-system 'utf-8)))
(set-buffer-file-coding-system 'utf-8)

(use-package elfeed-score
  :after elfeed
  :config
  (setq elfeed-score-score-file "~/.doom.d/elfeed.score")
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;;;; Pocket
(use-package pocket-reader
  :defer t)
;;; Temporary section


(use-package secret-mode
  :defer t)
;;; Temporary unused

;; (use-package code-review
;;   :defer t)
;;; Crutches
(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; Movement
        (define-key map [remap backward-sentence] 'python-nav-backward-block)
        (define-key map [remap forward-sentence] 'python-nav-forward-block)
        (define-key map [remap backward-up-list] 'python-nav-backward-up-list)
        (define-key map [remap mark-defun] 'python-mark-defun)
        (define-key map "\C-c\C-j" 'imenu)
        ;; Indent specific
        (define-key map "\177" 'python-indent-dedent-line-backspace)
        (define-key map (kbd "<backtab>") 'python-indent-dedent-line)
        (define-key map "\C-c<" 'python-indent-shift-left)
        (define-key map "\C-c>" 'python-indent-shift-right)
        ;; Skeletons
        (define-key map "\C-c\C-tc" 'python-skeleton-class)
        (define-key map "\C-c\C-td" 'python-skeleton-def)
        (define-key map "\C-c\C-tf" 'python-skeleton-for)
        (define-key map "\C-c\C-ti" 'python-skeleton-if)
        (define-key map "\C-c\C-tm" 'python-skeleton-import)
        (define-key map "\C-c\C-tt" 'python-skeleton-try)
        (define-key map "\C-c\C-tw" 'python-skeleton-while)
        ;; Shell interaction
        (define-key map "\C-c\C-p" 'run-python)
        (define-key map "\C-c\C-s" 'python-shell-send-string)
        (define-key map "\C-c\C-e" 'python-shell-send-statement)
        (define-key map "\C-c\C-r" 'python-shell-send-region)
        (define-key map "\C-\M-x" 'python-shell-send-defun)
        (define-key map "\C-c\C-c" 'python-shell-send-buffer)
        (define-key map "\C-c\C-l" 'python-shell-send-file)
        (define-key map "\C-c\C-z" 'python-shell-switch-to-shell)
        ;; Some util commands
        (define-key map "\C-c\C-v" 'python-check)
        (define-key map "\C-c\C-f" 'python-eldoc-at-point)
        (define-key map "\C-c\C-d" 'python-describe-at-point)
        ;; Utilities
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (easy-menu-define python-menu map "Python Mode menu"
          '("Python"
            :help "Python-specific Features"
            ["Shift region left" python-indent-shift-left :active mark-active
             :help "Shift region left by a single indentation step"]
            ["Shift region right" python-indent-shift-right :active mark-active
             :help "Shift region right by a single indentation step"]
            "-"
            ["Start of def/class" beginning-of-defun
             :help "Go to start of outermost definition around point"]
            ["End of def/class" end-of-defun
             :help "Go to end of definition around point"]
            ["Mark def/class" mark-defun
             :help "Mark outermost definition around point"]
            ["Jump to def/class" imenu
             :help "Jump to a class or function definition"]
            "--"
            ("Skeletons")
            "---"
            ["Start interpreter" run-python
             :help "Run inferior Python process in a separate buffer"]
            ["Switch to shell" python-shell-switch-to-shell
             :help "Switch to running inferior Python process"]
            ["Eval string" python-shell-send-string
             :help "Eval string in inferior Python session"]
            ["Eval buffer" python-shell-send-buffer
             :help "Eval buffer in inferior Python session"]
            ["Eval statement" python-shell-send-statement
             :help "Eval statement in inferior Python session"]
            ["Eval region" python-shell-send-region
             :help "Eval region in inferior Python session"]
            ["Eval defun" python-shell-send-defun
             :help "Eval defun in inferior Python session"]
            ["Eval file" python-shell-send-file
             :help "Eval file in inferior Python session"]
            ["Debugger" pdb :help "Run pdb under GUD"]
            "----"
            ["Check file" python-check
             :help "Check file for errors"]
            ["Help on symbol" python-eldoc-at-point
             :help "Get help on symbol at point"]
            ["Complete symbol" completion-at-point
             :help "Complete symbol before point"]))
        map))
