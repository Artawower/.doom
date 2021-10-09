;; This solution doesn't work properly
(use-package polymode
        :ensure t
        :defer t
        :hook (vue-mode . lsp-deferred)
        :mode ("\\.vue\\'" . vue-mode)
        :config


        (define-innermode poly-vue-template-innermode
          :mode 'html-mode
          :head-matcher "<[[:space:]]*template[[:space:]]*[[:space:]]*>"
          :tail-matcher "</[[:space:]]*template[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-script-innermode
          :mode 'js-mode
          :head-matcher "<[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-typescript-innermode
          :mode 'typescript-mode
          :head-matcher "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*ts[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-javascript-innermode
          :mode 'js2-mode
          :head-matcher "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*js[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-auto-innermode poly-vue-template-tag-lang-innermode
          :head-matcher "<[[:space:]]*template[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*template[[:space:]]*[[:space:]]*>"
          :mode-matcher (cons  "<[[:space:]]*template[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
          :head-mode 'host
          :tail-mode 'host)

        (define-auto-innermode poly-vue-script-tag-lang-innermode
          :head-matcher "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*script[[:space:]]*[[:space:]]*>"
          :mode-matcher (cons  "<[[:space:]]*script[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
          :head-mode 'host
          :tail-mode 'host)

        (define-auto-innermode poly-vue-style-tag-lang-innermode
          :head-matcher "<[[:space:]]*style[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
          :tail-matcher "</[[:space:]]*style[[:space:]]*[[:space:]]*>"
          :mode-matcher (cons  "<[[:space:]]*style[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
          :head-mode 'host
          :tail-mode 'host)

        (define-innermode poly-vue-style-innermode
          :mode 'css-mode
          :head-matcher "<[[:space:]]*style[[:space:]]*[[:space:]]*>"
          :tail-matcher "</[[:space:]]*style[[:space:]]*[[:space:]]*>"
          :head-mode 'host
          :tail-mode 'host)

        (define-polymode vue-mode
          :hostmode 'poly-sgml-hostmode
          :innermodes '(
                        poly-vue-typescript-innermode
                        poly-vue-javascript-innermode
                        poly-vue-template-tag-lang-innermode
                        poly-vue-script-tag-lang-innermode
                        poly-vue-style-tag-lang-innermode
                        poly-vue-template-innermode
                        poly-vue-script-innermode
                        poly-vue-style-innermode
                        )))
