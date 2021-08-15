(use-package org-superstar
  :after org
  :config
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (when (derived-mode-p 'org-mode) (org-superstar-mode 1))                             ))
  (setq org-directory "~/Yandex.Disk.localized/org")
  (setq org-agenda-files '("~/Yandex.Disk.localized/org/articles"))
  (setq org-agenda-files '("~/Yandex.Disk.localized/org/strudy"))
  (setq org-agenda-files (directory-files-recursively "~/Yandex.Disk.localized/org/" "\\.org$"))

  )


(use-package org
  :defer t
  :mode (("\\.org$" . org-mode))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t)))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.75))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   )
  (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      "☸"
              :options       "⌥"
              :latex_class   "🄲"
              :latex_header  "⇥"
              :beamer_header "↠"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :begin_quote   "❮"
              :end_quote     "❯"
              :caption       "☰"
              :header        "›"
              :results       "🠶"
              :begin_export  "⯮"
              :end_export    "⯬"
              :properties    "⚙"
              :end           "∎"
              :priority_a   ,(propertize "⚑")
              :priority_b   ,(propertize "⬆")
              :priority_c   ,(propertize "■")
              :priority_d   ,(propertize "⬇")
              :priority_e   ,(propertize "❓")))

              ;;   :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              ;; :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              ;; :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              ;; :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              ;; :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))

  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_latex:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"
    )
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t)))


  (defun publish-org-blog()
    "Publish this note to du-blog!"
    (interactive)

    (message (concat
              "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js"
              (buffer-file-name)))
    (shell-command
     (concat
      "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js "
      (buffer-file-name))
     ))

  (setenv "NODE_PATH"
          (concat
           (getenv "HOME") "/org-node/node_modules"  ":"
           (getenv "NODE_PATH")
           )
          )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)))

  (defun org-babel-execute:typescript (body params)
    (let ((org-babel-js-cmd "npx ts-node < "))
      (org-babel-execute:js body params)))

  (defvar org-babel-js-function-wrapper
    ""
    "Javascript code to print value of body.")
  )

;; (provide 'org-conf)

;; (use-package org-roam
;;       :ensure t
;;       :custom
;;       (org-roam-directory (file-truename "~/Yandex.Disk.localized/org-roam"))
;;       :bind (("C-c n l" . org-roam-buffer-toggle)
;;              ("C-c n f" . org-roam-node-find)
;;              ("C-c n g" . org-roam-graph)
;;              ("C-c n i" . org-roam-node-insert)
;;              ("C-c n c" . org-roam-capture)
;;              ;; Dailies
;;              ("C-c n j" . org-roam-dailies-capture-today))
;;       :config
;;       (setq org-roam-v2-ack t)
;;       (org-roam-setup)
;;       ;; If using org-roam-protocol
;;       (require 'org-roam-protocol))

(use-package org-roam
  :defer 10
  :config

  (setq org-roam-directory "~/Yandex.Disk.localized/org-roam")
  (require 'ox-json)
  (require 'org-roam-protocol)
  )

(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(use-package org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (smartparens-global-mode -1)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
    (smartparens-global-mode 1))
  )


;; automatically enable server-mode
(after! org-roam
  (smartparens-global-mode -1)
  (org-roam-server-mode)
  (smartparens-global-mode 1))
