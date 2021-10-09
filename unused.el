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
         ("C-x c" . completion-at-point))

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

Optionally use the `orderless' completion style.
Enable `partial-completion' for files to allow path expansion.
You may prefer to use `initials' instead of `partial-completion'.
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
