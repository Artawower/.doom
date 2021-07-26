;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
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


;; Opacity
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))


;; theme
(use-package heaven-and-hell
  :defer t
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . doom-nord-light)
          (dark . doom-one))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
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
(use-package ranger)
;; terminal
(use-package vterm-toggle
  :defer t
  :after vterm
  :init
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
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-min-width 110
        ivy-posframe-width 110
        ivy-posframe-height 20
        ivy-posframe-min-height 20
        ivy-posframe-parameters '((internal-border-width . 2) (left-fringe . 18) (right-fringe . 18) )
        )
  (ivy-posframe-mode +1)
  )

;; Google translate, why not?
(use-package google-translate
  :ensure t
  :custom
  (google-translate-backend-method 'curl)
  :config
   (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

;; (use-package all-the-icons-ivy-rich
;;   :after ivy
;;   :init
;;   (all-the-icons-ivy-rich-mode 1)
;;   (ivy-rich-mode 1)
;;   )

;; bookmarks
(use-package bm
  :defer t
  :init
  (global-set-key (kbd "C-M-b") 'bm-toggle)
  (global-set-key (kbd "s-b") 'bm-toggle)
  (global-set-key (kbd "C-M-n") 'bm-next)
  (global-set-key (kbd "C-M-p") 'bm-previous)
  (define-key global-map [f8] 'bookmark-jump)
  (define-key global-map [f9] 'bookmark-set)


  (setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
  (setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry
  )

;; indent
(use-package indent-guide
  :defer
  :init
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
  (indent-guide-global-mode)
  )

;; ;; modeline
(use-package doom-modeline
  :defer t
  :init
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

  (setq yas-snippet-dirs
        '("~/doom.d/snippets"                 ;; personal snippets
          ))
  (yas-global-mode 1)
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
