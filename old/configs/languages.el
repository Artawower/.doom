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
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :hook ((js-mode . lsp)
         (go-mode . lsp)
         (javascript-mode . lsp)
         (web-mode . lsp)
         (vue-mode . lsp))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.3)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
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

;; (with-eval-after-load 'lsp-mode (lambda ()
;;                                   (add-hook 'before-save-hook #'+format/buffer nil t)))
;; (use-package format-all
;;   :after lsp
;;   :init
;;   (add-hook 'before-save-hook #'format-all-buffer nil t)
;;   )
;; (add-hook 'before-save-hook #'+format/buffer nil t)


(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; (setq lsp-ui-doc-position 'top)
  ;; (setq lsp-ui-doc-max-width 180)
  ;; (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-diagnostic-max-line-length 200)
  (setq lsp-ui-sideline-diagnostic-max-lines 5)
  ;; (setq lsp-ui-sideline-show-symbol t)
  ;; (setq lsp-ui-doc-alignment 'window)
  (setq lsp-diagnostic-clean-after-change t)
  ;; (setq lsp-ui-doc-delay 0.8)
  ;; (setq lsp-ui-doc-use-webkit t)
  ;; (setq lsp-ui-doc-use-childframe t)
  ;; (setq lsp-ui-sideline-show-code-actions nil)
  (add-hook 'before-save-hook #'+format/buffer nil t)
  :init

  (setq lsp-ui-sideline-diagnostic-max-lines 5)
  )

(use-package prettier-js
  :defer t
  :hook ((ng2-html-mode . prettier-js-mode)
         (ng2-ts-mode . prettier-js-mode)
         (js-mode . prettier-js-mode))
  )
;; AI completion
(defun add-company-tabnine ()
  (add-to-list (make-local-variable 'company-backends) 'company-tabnine))

(use-package company
  :config
  (setq company-idle-delay 500
        company-minimum-prefix-length 1
        )
  )

(use-package! company-tabnine
  :after company-mode
  :init
  (setq +lsp-company-backends '(company-tabnine company-capf))
  (setq company-backends '(company-tabnine company-capf))
  :config
  (add-company-tabnine))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  backend
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ; (use-package dap-mode
                                        ;   :ensure
                                        ;   :config
                                        ;   (dap-ui-mode)
                                        ;   (dap-ui-controls-mode 1)

                                        ;   (require 'dap-lldb)
                                        ;   (require 'dap-gdb-lldb)
                                        ;   ;; installs .extension/vscode
                                        ;   (dap-gdb-lldb-setup)
                                        ;   (dap-register-debug-template
                                        ;    "Rust::LLDB Run Configuration"
                                        ;    (list :type "lldb"
                                        ;          :request "launch"
                                        ;          :name "LLDB::Run"
                                        ; 	 :gdbpath "rust-lldb"
                                        ;          :target nil
                                        ;          :cwd nil)))

(use-package dap-go
  :after go-mode
  :config
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
  :config
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
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq python-indent-offset 4)
              (setq global-flycheck-mode 1)
              )
            )
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
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

;; Rust
(setq lsp-ui-sideline-diagnostic-max-lines 4)
(use-package rustic
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  frontend
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js
  :after lsp
  :hook (js-mode . lsp-mode)
  :init
  (setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
  )
(use-package ng2-mode
  :after lsp
  :config
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

;; (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;; (setq-hook! 'ng2-mode-hook +format-with-lsp nil)
;; (setq-hook! 'typescript-mode-hook +format-with-lsp nil)

(defun init-angular-env ()
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  ;; (add-hook 'typescript-mode-hook #'before-save-mode)
  (add-hook 'ng2-html-mode-hook #'lsp)
  ;; (add-hook 'ng2-html-mode-hook #'before-save-mode)
  (add-hook 'ng2-mode #'lsp)

  ;; (add-hook 'before-save-hook #'+format/buffer nil t)
  ;; (add-hook 'typescript-mode-hook #'my-flycheck-setup)

  )


;; (add-hook 'before-save-hook #'+format/buffer nil t)

(with-eval-after-load 'typescript-mode (init-angular-env))
(with-eval-after-load 'ng2-html (init-angular-env))
;; (with-eval-after-load 'ng2-mode (init-angular-env))
;; (with-eval-after-load 'ng2-ts-mode (init-angular-env))

;; Vue js
(add-hook 'before-save-hook #'+format/buffer nil t)
(use-package web-mode
  :defer t
  :config
  (require 'flycheck)
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui)
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-hook 'web-mode-hook '(lambda () (setq lsp-diagnostic-package :none)))
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'flycheck-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode)
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
  (setq typescript-indent-level 2)

  (setq mmm-vue-html-mode-exit-hook (lambda ()
                                      (message "Run when leaving vue-html mode")
                                      (emmet-mode -1)))
  (setq mmm-vue-html-mode-enter-hook (lambda ()
                                       (message "Run when entering vue-html mode")
                                       (emmet-mode 1)))
  )
(use-package vue-mode
  :mode ("\\.vue\\'")
  :init
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 1)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'flycheck-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode)
  (add-hook 'web-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )
;; (add-hook 'vue-mode-hook (flycheck-select-checker 'javascript-eslint))
(setq mmm-vue-html-mode-exit-hook (lambda ()
                                    (message "Run when leaving vue-html mode")
                                    (emmet-mode -1)))
(setq mmm-vue-html-mode-enter-hook (lambda ()
                                     (message "Run when entering vue-html mode")
                                     (emmet-mode 1)))
;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(custom-set-faces '(mmm-default-submode-face ((t (:background nil)))))
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

;; (setq 'web-mode-javascript-indentation 2)
(setq-default indent-tabs-mode nil)
(use-package lua-mode
  :defer t)
