(use-package evil-leader
  :defer t
  :config
  (evil-leader/set-key
    "f" 'avy-goto-char
    "b" 'evilem-motion-previous-line
    "p" 'prettier-prettify
    "k" 'save-buffer-without-dtw

    "d" 'dup-debug

    "o" 'org-mode
    "q" 'kill-current-buffer
    "v" 'vterm
    "`" 'vterm-toggle-cd
    "i" 'git-messenger:popup-message
    "t" 'google-translate-at-point
    "T" 'google-translate-query-translate

    "a" 'counsel-org-agenda-headlines
    "c" 'dired-create-empty-file
    "p" '+format/buffer
    "s" 'publish-org-blog
    "g" 'dogears-go

    "h" 'lsp-ui-doc-show
    "e" 'lsp-treemacs-errors-list
    "r" 'treemacs-select-window
    )
  :init
  (global-evil-leader-mode)
  (evilem-default-keybindings "SPC")

  ;; some bullshit for butiful rechange..
  ;; (add-hook 'evil-insert-state-entry-hook (lambda ()
  ;;                                           (prettify-symbols-mode -1)
  ;;                                           (org-superstar-mode -1)
  ;;                                           ))
  ;; (add-hook 'evil-normal-state-entry-hook (lambda () (prettify-symbols-mode 1) (when (derived-mode-p 'org-mode) (org-superstar-mode 1))))
  ;; (add-hook 'evil-insert-state-entry-hook (lambda ()
  ;;                                           (prettify-symbols-mode -1)))
  ;; (add-hook 'evil-normal-state-entry-hook (lambda ()
  ;;                                           (prettify-symbols-mode 1)))

  )
(use-package reverse-im
  :ensure t
  :defer 0.1
  :config
  (reverse-im-activate "russian-computer"))
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
(global-set-key (kbd "C-x C-/") 'company-tide)
(global-set-key (kbd "C-x C-i") 'company-tabnine)

(provide 'keybindings)
