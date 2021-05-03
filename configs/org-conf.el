(use-package org-superstar
  :after org
  :init
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
  :init
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

  :config
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
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
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
  )

(require 'ox-json)
;; (provide 'org-conf)
