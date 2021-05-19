
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; performance
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(menu-bar-mode t)
(setq user-full-name "Artur Yaroshenko"
      user-mail-address "artawower@33gmail.com")

;; (load-theme 'atom-one-dark t)
(setq doom-theme 'doom-material)

(setq display-line-numbers-type 'relative)


;; Fix mac os error
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))



(add-to-list 'load-path "~/.doom.d/configs/")


(load! "./configs/editor.el")
(load! "./configs/org-conf.el")
(load! "./configs/keybindings.el")
(load! "./configs/tools.el")
(load! "./configs/languages.el")
(load! "./configs/git.el")

;; Specific frameworks
;; (load! "./configs/angular2.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  Common configs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPORARY SECTION
(use-package ranger)
(setq-default evil-kill-on-visual-paste nil)

(defun my-exec-path-from-shell-initialize ()

  (setenv "PERL5LIB" (concat "~/perl5/lib/perl5" ":"
                             (getenv "PERL5LIB")))

  (setenv "LC_ALL" "en_US.UTF-8")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package exec-path-from-shell
  :init
  (add-hook 'after-init-hook 'my-exec-path-from-shell-initialize))
