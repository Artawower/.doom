;;; pinky-winky-dark-theme.el --- Horizon theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2021 Artur Yaroshenko
;;
;; Keywords: custom themes, faces
;; Homepage: https://github.com/Artawower/.doom
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'doom-themes)


;;; Variables

(defgroup pinky-winky-dark-theme nil
  "Options for the `pinky-winky-dark' theme."
  :group 'doom-themes)

(defcustom pinky-winky-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'pinky-winky-dark-theme
  :type 'boolean)

(defcustom pinky-winky-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'pinky-winky-dark-theme
  :type 'boolean)

(defcustom pinky-winky-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'pinky-winky-dark-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme pinky-winky-dark
  "A dark theme inspired by ."

  ;; name        default   256           16
  ((bg         '("#24283b" "#24283b"      "black"  ))
   (fg         '("#c0caf5" "#c0caf5"     "brightwhite"  ))

   (bg-alt     '("#1f2335" "#1f2335"       "black"        ))
   (fg-alt     '("#a9b1d6" "#a9b1d6"     "white"        ))
   (base0      '("#1B2229" "black"       "black"        ))
   (base1      '("#1c1f24" "#1e1e1e"     "brightblack"  ))
   (base2      '("#202328" "#2e2e2e"     "brightblack"  ))
   (base3      '("#23272e" "#262626"     "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f"     "brightblack"  ))
   (base5      '("#5B6268" "#525252"     "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b"     "brightblack"  ))
   (base7      '("#9ca0a4" "#979797"     "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf"     "white"        ))

   (grey       base4)
   (red        '("#FF3399" "#FF3399" "red"          ))
   (light-red  '("#FB5FC0" "#FB5FC0" "red"          ))
   (orange     '("#dd9046" "#dd9046" "brightred"    ))
   (green      '("#8bcd5b" "#8bcd5b" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (blue       '("#4FC3F7" "#4FC3F7" "brightblue"   ))
   (dark-blue  '("#00AEE8" "#00AEE8" "blue"         ))
   (magenta    '("#CC66FF" "#CC66FF" "brightmagenta"))
   (violet     '("#E4CCFF" "#E4CCFF" "magenta"      ))
   (cyan       '("#00CED1" "#00CED1" "brightcyan"   ))
   (dark-cyan       '("#00CED1" "#00CED1" "brightcyan"   ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      light-red)
   (vertical-bar   base1)
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if pinky-winky-dark-brighter-comments base1 base5))
   (doc-comments   (doom-lighten (if pinky-winky-dark-brighter-comments base1 base5) 0.25))
   (constants      (doom-lighten magenta 0.4))
   (functions      (doom-lighten light-red 0.25))
   (keywords       (doom-lighten magenta 0.2))
   (methods        (doom-lighten light-red 0.3))
   (operators      blue)
   (type           (doom-lighten blue 0.65))
   (strings        violet)
   (variables      (doom-lighten light-red 0.5))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base6)
   (modeline-bg              (if pinky-winky-dark-brighter-modeline
                                 (doom-darken bg-alt 0.45)
                               (doom-darken (car bg-alt) 0.35)))
   (modeline-bg-alt          (if pinky-winky-dark-brighter-modeline
                                 (doom-darken bg-alt 0.475)
                               `(,(doom-darken (car bg-alt) 0.35) ,@(cdr bg))))

   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (pinky-winky-dark-modeline-pad
    (when pinky-winky-dark-padded-modeline
      (if (integerp pinky-winky-dark-padded-modeline) pinky-winky-dark-bright-padded-modeline 4))))


;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if pinky-winky-dark-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if pinky-winky-dark-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if pinky-winky-dark-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if pinky-winky-dark-brighter-modeline base8 highlight))

;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
;;;; doom-modeline
   (doom-modeline-bar :background (if pinky-winky-dark-brighter-modeline modeline-bg highlight) :foreground bg)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold :foreground (doom-lighten violet 0.35))
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
;;;; ivy
  (ivy-current-match :background (doom-lighten red 0.35) :distant-foreground base0 :weight 'normal :foreground bg)
  (ivy-minibuffer-match-face-2 :background red :foreground blue)
;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
;;;; Pair parent face
   (sp-show-pair-match-face :foreground (doom-darken light-red 0.1) :background nil)
   (show-paren-match :foreground (doom-darken light-red 0.1) :background nil)
   (show-paren-match-expression :foreground (doom-darken light-red 0.1) :background nil)
;;; Doom
;;;; Workspace
   (+workspace-tab-selected-face :foreground (doom-lighten grey 0.8) :background blue :weight 'bold)
   (doom-dashboard-menu-title :foreground (doom-lighten light-red 0.2))
;;; Lsp
   (lsp-face-highlight-read :foreground red :weight 'bold)
   (lsp-face-highlight-textual :foreground red :weight 'bold)
;;; Magit
(magit-diff-hunk-heading-highlight :background (doom-lighten red 0.35))
;;; Treemacs
(font-lock-doc-face :foreground (doom-lighten light-red 0.3))
;;; Org mode
(org-level-1 :foreground (doom-lighten light-red 0.3))
(org-level-2 :foreground blue)
(org-level-3 :foreground (doom-lighten magenta 0.4))
(org-document-title :foreground (doom-lighten light-red 0.3))
;;;; hl-line mod e
   ;; (hl-line :background (doom-lighten violet 0.85))
;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if pinky-winky-dark-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if pinky-winky-dark-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

;;;; Base theme variable overrides-
  ())

;;; pinky-winky-dark-theme.el ends here
