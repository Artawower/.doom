;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  common
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'javascript-eslint 'typescript-tslint))

(use-package lsp-mode
  :config
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (setenv "PATH" (concat (getenv "HOME") "/go/bin"))
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook '(lambda () (setq lsp-diagnostic-package :none)))
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-file-watch-threshold 4000)
  (setq lsp-ui-sideline-show-code-actions nil)
  ;; (setq lsp-print-performance t)
  (setq lsp-idle-delay 0.500)
  (setq lsp-enable-file-watchers nil) ;; boost performance ?

  )

(use-package format-all
  :init
  (add-hook 'before-save-hook #'format-all-buffer)
  )



(use-package lsp-ui
  :after lsp
  :config
  (setq lsp-ui-sideline-show-code-actions nil))

;; AI completion
(defun add-company-tabnine ()
  (add-to-list (make-local-variable 'company-backends) 'company-tabnine))

(use-package! company-tabnine
  :config
  (setq +lsp-company-backends '(company-tabnine company-capf))
  (setq company-backends '(company-tabnine))
  (add-company-tabnine))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  backend
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package dap-go
  :after go-mode
  :init
  (require 'dap-ui)
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (set-fringe-style (quote (14 . 10))) ;; Left breakpoint sqr size ;)
  )

(with-eval-after-load 'css-mode
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
  (add-hook 'scss-mode-hook '(lambda () (add-hook 'after-save-hook #'run-sass-auto-fix t t)))
  )

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/.local/share/virtualenvs"))
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "social-network-promotion-qKnIBgNK")  ; Default venv
  (pyvenv-tracking-mode 1))

(use-package python-mode
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq python-indent-offset 4)
              (setq global-flycheck-mode 1)
              )
            )
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (add-to-list (make-local-variable 'company-backends) 'company-tabnine)
  )


;; (use-package lsp-pyright
;;   :ensure t
;;   :config
;;   (setq lsp-pyright-auto-import-completions t)
;;   (setq lsp-pyright-auto-search-paths t)
;;   (setq lsp-pyright-log-level "trace")
;;   ;; (setq lsp-pyright-venv-path "/Users/arturarosenko/projects/atom-security/atom-security-back/venv")
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (require 'pipenv)
;;                          (pipenv-activate)
;;                          (lsp))))  ; or lsp-deferred

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  frontend
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js
  :config
  (setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")

  )
(use-package ng2-mode
  :after lsp
  :init
  (add-to-list 'auto-mode-alist '("\.ts\'" . typescript-mode))

  (setq read-process-output-max (* 1024 1024))

  (setq lsp-clients-angular-language-server-command
        '("node"
          "/usr/local/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          "/usr/local/lib/node_modules"
          "--stdio"))

  )

(defun init-angular-env ()
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'ng2-html-mode-hook #'lsp)
  ;; (add-hook 'ng2-mode #'lsp)
  ;; (add-hook 'typescript-mode-hook #'my-flycheck-setup)

  ;; (add-hook 'ng2-mode-hook #'lsp-deferred)
  )
(with-eval-after-load 'typescript-mode (init-angular-env))
(with-eval-after-load 'ng2-html (init-angular-env))
;; (with-eval-after-load 'ng2-mode (init-angular-env))
;; (with-eval-after-load 'ng2-ts-mode (init-angular-env))

;; Vue js

(use-package web-mode
  :defer t
  :init
  (require 'flycheck)
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui)
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-hook 'web-mode-hook '(lambda () (setq lsp-diagnostic-package :none)))
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'flycheck-mode)
  (add-hook 'web-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; (flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint) 'append)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; (flycheck-add-next-checker 'typescript-tide)
  ;; (flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint) 'append)
  ;; (flycheck-add-mode 'lsp-ui 'web-mode)


  ;; (add-hook 'web-mode-hook 'my-flycheck-setup)

  (setq-default indent-tabs-mode nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq mmm-vue-html-mode-exit-hook (lambda ()
                                      (message "Run when leaving vue-html mode")
                                      (emmet-mode -1)))
  (setq mmm-vue-html-mode-enter-hook (lambda ()
                                       (message "Run when entering vue-html mode")
                                       (emmet-mode 1)))
  )

(use-package lua-mode
  :defer t)
