;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
;; (set-window-margins (selected-window) 4 1)
(setq-default left-margin-width 4 right-margin-width 4)

;; (setq doom-font (font-spec :family "Ligamonacop Nerd Font" :size 14))
(plist-put +ligatures-extra-symbols :name "⁍")
(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "===" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "==" "++" "--" "=>" "|>" "<|" "||>" "<||"
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
;; (defconst fira-code-mode--ligatures
;;   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
;;     "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
;;     "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
;;     "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
;;     ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
;;     "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
;;     "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
;;     "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
;;     ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
;;     "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
;;     "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
;;     "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
;;     ;;"x"
;;     ":" "+" "+" "*"))
(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
                        (aref pat 0)
                        (nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
                                             0
                                             'compose-gstring-for-graphic)))))

(add-hook 'prog-mode-hook #'wucuo-start)
(add-hook 'text-mode-hook #'wucuo-start)
(add-hook 'go-mode #'wucuo-start)
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for since Aspell 0.60.8
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
(setq wucuo-spell-check-buffer-predicate
      (lambda ()
        (not (memq major-mode
                   '(dired-mode
                     log-edit-mode
                     compilation-mode
                     help-mode
                     profiler-report-mode
                     speedbar-mode
                     gud-mode
                     calc-mode
                     Info-mode)))))
(setq wucuo-personal-font-faces-to-check '(font-lock-comment-face))
(defun my-checker (word)
  "If WORD is typo, return t."
  t)
(setq wucuo-extra-predicate #'my-checker)
;; (use-package guess-language
;;   :after wucuo
;;   :config
;;   (setq guess-language-languages '(en ru))
;;   )

                                        ; ====== 31j
;; (use-package wucuo
;;   :config
;;   (wucuo-start)
;;   ;; :hook ((text-mode prog-mode) . wucuo-start)
;;   :init
;;   ;; (setenv "DICTIONARY" "en_US")
;;   ;; (setenv "LANG" "en_US.UTF-8")
;;   ;; (setq ispell-program-name "/usr/local/bin/hunspell"
;;   ;;       ispell-dictionary "en_US")
;;   (setq ispell-program-name "/usr/local/bin/aspell"
;;         ispell-dictionary "en_GB")
;;   (setq ispell-local-dictionary-alist
;;         ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
;;         ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
;;         '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))
;;   (setq-default ispell-extra-args '("--sug-mode=ultra"
;;                                     "--camel-case"
;;                                     "--lang=en_GB"))
;;   (ispell-set-spellchecker-params)
;;   (setq wucuo-flyspell-start-mode "lite")
;;   ;; (ispell-hunspell-add-multi-dic "en_US,ru_RU")

;;   ;; Make sure new aspell is installed
;;   ;; (when (string-match-p "--camel-case"
;;   ;;                       (shell-command-to-string (concat ispell-program-name " --help")))
;;   ;; (push "--camel-case" ispell-extra-args))
;;   ;; (setq ispell-really-aspell nil
;;   ;;       ispell-really-hunspell t)
;;   ;; (ispell-set-spellchecker-params)
;;   ;; (ispell-hunspell-add-multi-dic "en_US,ru_RU")
;;   )
;;   =======
;; Spell checker
;; (setq ispell-program-name "aspell"
;;       ispell-dictionary "ru")
;; (add-hook 'prog-mode-hook #'wucuo-start)
;; (add-hook 'text-mode-hook #'wucuo-start)
;; (setq wucuo-spell-check-buffer-predicate
;;       (lambda ()
;;         (not (memq major-mode
;;                    '(dired-mode
;;                      log-edit-mode
;;                      compilation-mode
;;                      help-mode
;;                      profiler-report-mode
;;                      speedbar-mode
;;                      gud-mode
;;                      calc-mode
;;                      Info-mode
;;                      go-mode)))))
;; (with-eval-after-load "ispell"
;;   (setenv "DICTIONARY" "en_US,ru_RU")
;;   (setenv "LANG" "en_US.UTF-8")
;; ;; two dictionaries "en_US" and "zh_CN" are used. Feel free to remove "zh_CN"
;; (setq ispell-local-dictionary-alist
;;       '(("myhunspell" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US" "zh_CN") nil utf-8)))
;;   (setq ispell-program-name "/usr/local/bin/hunspell"
;;         ispell-dictionary "en_US,ru_RU")
;; (setq ispell-local-dictionary-alist
;;     '(("russian"
;;        "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
;;        "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
;;        "[-]"  nil ("-d" "ru_RU") nil utf-8)
;;       ("english"
;;        "[A-Za-z]" "[^A-Za-z]"
;;        "[']"  nil ("-d" "en_US") nil iso-8859-1)))

;; вместо aspell использовать hunspell
;; (setq ispell-really-aspell nil
;;       ispell-really-hunspell t)
;; (ispell-set-spellchecker-params)
;; (ispell-hunspell-add-multi-dic "en_US,ru_RU")
;; (ispell-hunspell-add-multi-dic "ru_RU,en_US")
;; (setq ispell-program-name "/usr/local/bin/aspell")

;; (defun ispell-get-word (following)
;;   (when following
;;     (camelCase-forward-word 1))
;;   (let* ((start (progn (camelCase-backward-word 1)
;;                        (point)))
;;          (end (progn (camelCase-forward-word 1)
;;                      (point))))
;;     (list (buffer-substring-no-properties start end)
;;           start end)))
;; )

;; Opacity
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(use-package tree-sitter
  :defer 5
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (ng2-mode . tree-sitter-hl-mode))
  :config
  (require 'tree-sitter-langs)
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (global-tree-sitter-mode)
  ;; (add-hook 'lsp-mode-hook 'tree-sitter-hl-mode)
  )


;; theme
(use-package heaven-and-hell
  :defer t
  :config
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . doom-nord-light)
          (dark . doom-moonlight))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))


;; Time track
(use-package wakatime-mode
  :config
  (global-wakatime-mode)
  )

(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 45)
  )

;; Another file manager
;; (use-package ranger)
;; terminal
(use-package vterm-toggle
  :defer t
  :after vterm
  :config
  (setq vterm-toggle-scope 'project)

  )

(use-package ivy-posframe
  :defer t
  :after ivy
  :diminish
  :custom-face
  ;; (ivy-posframe-border ((t (:background "#c95562"))))
  (ivy-posframe-border ((t (:background "#4FAAEA"))))
  :init
  (ivy-posframe-mode +1)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))
        ivy-posframe-height-alist '((t . 20))
        ;; ivy-posframe-min-width 110
        ;; ivy-posframe-width 110
        ivy-posframe-height 20
        ;; ivy-posframe-min-height 60
        ivy-posframe-parameters '((internal-border-width . 2) (left-fringe . 18) (right-fringe . 18) )
        )
  (defun ivy-posframe-get-size ()
    "The default functon used by `ivy-posframe-size-function'."
    (list
     :height ivy-posframe-height
     :width ivy-posframe-width
     :min-height (or ivy-posframe-min-height
                     (let ((height (+ ivy-height 1)))
                       (min height (or ivy-posframe-height height))))
     :min-width (or ivy-posframe-min-width
                    (let ((width (round (* (frame-width) 0.85))))
                      (min width (or ivy-posframe-width width))))))
  )

;; Google translate, why not?
(use-package google-translate
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

;; bookmarks
(use-package bm
  :defer t
  :init
  (global-set-key (kbd "s-b") 'bm-toggle)
  (global-set-key (kbd "C-M-n") 'bm-next)
  (global-set-key (kbd "C-M-p") 'bm-previous)
  (define-key global-map [f8] 'bookmark-jump)
  (define-key global-map [f9] 'bookmark-set)


  (setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
  (setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry
  )

;; auto bookmarks
(use-package dogears
  :defer t
  :config
  (dogears-mode 1)
  )

;; Better vim like mathit navifation
(use-package evil-matchit
  :after evil-mode
  :init
  ;; :config
  ;; (evilmi-load-plugin-rules '(mhtml-mode) '(ng2-html-mode ng2-html))
  ;; (evilmi-load-plugin-rules '(html-mode) '(ng2-html-mode ng2-html))
  )
(evilmi-load-plugin-rules '(ng2-html-mode) '(html))
(global-evil-matchit-mode 1)


;; indent
(use-package indent-guide
  :defer
  :init
  (indent-guide-global-mode)
  :config
  (setq indent-guide-threshold 0)
  (setq indent-guide-char ".")

  (add-hook 'ng2-html-mode 'indent-guide-mode)
  (add-hook 'ng2-ts-mode 'indent-guide-mode)
  (add-hook 'yaml-mode 'indent-guide-mode)
  (add-hook 'html-mode 'indent-guide-mode)
  (add-hook 'python-mode 'indent-guide-mode)
  (add-hook 'web-mode 'indent-guide-mode)
  (add-hook 'scss-mode 'indent-guide-mode)
  (add-hook 'css-mode 'indent-guide-mode)
  (add-hook 'go-mode 'indent-guide-mode)
  )

;; ;; modeline
(use-package doom-modeline
  :defer t
  :config
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  ;; (setq doom-modeline-vcs-max-length 18)
  ;; (setq doom-modeline-modal-icon t)
  ;; (setq doom-modeline-icon (display-graphic-p))
  ;; (setq doom-modeline-major-mode-icon t)
  ;; (setq doom-modeline-major-mode-color-icon t)
  ;; (setq doom-modeline-buffer-modification-icon t)
  ;; (setq doom-modeline-persp-icon t)
  )
(use-package nyan-mode
  :init
  (nyan-mode))

;; Yasnippet
(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1)
  :config

  (setq yas-snippet-dirs
        '("~/doom.d/snippets"                 ;; personal snippets
          ))
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


  (global-set-key (kbd "C-c C-s") 'yas-new-snippet)
  )

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))


(use-package floobits
  :defer 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  TMP!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
