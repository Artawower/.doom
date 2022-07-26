;;; pinky-winky-theme.el --- Horizon theme -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defgroup pinky-winky-theme nil
  "Options for the `pinky-winky' theme."
  :group 'doom-themes)

(defcustom pinky-winky-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'pinky-winky-theme
  :type 'boolean)

(defcustom pinky-winky-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'pinky-winky-theme
  :type 'boolean)

(defcustom pinky-winky-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'pinky-winky-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme pinky-winky
  "A dark theme inspired by ."

  ;; name        default   256           16
  ((bg         '("#f8f8ff" "#f8f8ff"      "white"  ))
   (fg         '("#44425e" "#44425e"     "brightwhite"  ))

   (bg-alt     '("#fafafa" "#fafafa"       "white"        ))
   (fg-alt     '("#A09BB3" "#A09BB3"     "white"        ))
   (base0      '("#8c8c84" "#efefef" "white"        ))
   (base1      '("#00AEE8" "#00AEE8"     "brightcyan"  ))
   (base2      '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3      '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4      '("#8c8c84" "#9ca0a4" "brightblack"  ))
   (base5      '("#484a42" "#424242" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b"     "brightblack"  ))
   (base7      '("#9ca0a4" "#979797"     "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf"     "white"        ))

   (grey       base4)
   (red        '("#FF3399" "#FF3399" "red"          ))
   (dark-red   '("#f65866" "#f65866" "red"          ))
   (orange     '("#dd9046" "#dd9046" "brightred"    ))
   (green      '("#00D364" "#00D364" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#FFAA00" "#FFAA00" "yellow"       ))
   (blue       '("#00AEE8" "#00AEE8" "brightblue"   ))
   (dark-blue  '("#00AEE8" "#00AEE8" "blue"         ))
   (magenta    '("#CC66FF" "#CC66FF" "brightmagenta"))
   (violet     '("#c75ae8" "#c75ae8" "magenta"      ))
   (cyan       '("#00CED1" "#00CED1" "brightcyan"   ))
   (dark-cyan       '("#00CED1" "#00CED1" "brightcyan"   ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   base1)
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if pinky-winky-brighter-comments base1 base5))
   (doc-comments   (doom-lighten (if pinky-winky-brighter-comments base1 base5) 0.25))
   (constants      red)
   (functions      blue)
   (keywords       violet)
   (methods        red)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten red 0.2))
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
   (modeline-bg              (if pinky-winky-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-lighten (car violet) 0.85)))
   (modeline-bg-alt          (if pinky-winky-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-lighten (car violet) 0.85) ,@(cdr bg))))

   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (pinky-winky-modeline-pad
    (when pinky-winky-padded-modeline
      (if (integerp pinky-winky-padded-modeline) pinky-winky-bright-padded-modeline 4))))


;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if pinky-winky-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if pinky-winky-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if pinky-winky-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if pinky-winky-brighter-modeline base8 highlight))

;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
;;;; doom-modeline
   (doom-modeline-bar :background (if pinky-winky-brighter-modeline modeline-bg highlight) :foreground bg)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold :foreground (doom-lighten violet 0.35))
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
;;;; ivy
   (ivy-current-match :background (doom-lighten red 0.35) :distant-foreground base0 :weight 'normal :foreground bg)
   (ivy-minibuffer-match-face-2 :background violet :foreground cyan)
   (ivy-posframe :background bg)
;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
;;;; Outlines
   (outline-minor-1 :background (doom-lighten red 0.55) :foreground bg :extend t :weight 'bold)
   (outline-minor-2 :background (doom-lighten red 0.66) :foreground bg :extend t :weight 'bold)
   (outline-minor-3 :background (doom-lighten red 0.77) :foreground bg :extend t :weight 'bold)
   (outline-minor-4 :background (doom-lighten red 0.78) :foreground bg :extend t :weight 'bold)
   (outline-minor-5 :background (doom-lighten red 0.79) :foreground bg :extend t :weight 'bold)
   (outline-minor-6 :background (doom-lighten red 0.80) :foreground bg :extend t :weight 'bold)
   (outline-minor-7 :background (doom-lighten red 0.81) :foreground bg :extend t :weight 'bold)
   (outline-minor-8 :background (doom-lighten red 0.82) :foreground bg :extend t :weight 'bold)
;;;; Pair parent face
   (sp-show-pair-match-face :foreground (doom-darken dark-red 0.1) :background nil)
   (show-paren-match :foreground (doom-darken dark-red 0.1) :background nil)
   (show-paren-match-expression :foreground (doom-darken dark-red 0.1) :background nil)
;;;; Region
   (region :background (doom-lighten violet 0.85))
;;;; Cursor
(cursor :background blue)
;;; Doom
;;;; Workspace
   (+workspace-tab-selected-face :foreground (doom-lighten grey 0.8) :background blue :weight 'bold)
;;; Lsp
   (lsp-face-highlight-read :foreground violet :weight 'bold)
   (lsp-face-highlight-textual :foreground violet :weight 'bold)
;;; Avy
   (avy-background-face :foreground (doom-lighten fg 0.8))
;;; Org mode
(org-block :background (doom-lighten dark-red 0.98))
;;; Treemacs
(font-lock-doc-face :foreground (doom-lighten red 0.3))
;;; Magit
(magit-diff-hunk-heading-highlight :background (doom-lighten violet 0.35))
;;; Smerge marjers
(smerge-markers :background base2 :foreground yellow)
(smerge-upper :background (doom-lighten red 0.8))
(parenthesis :foreground base2)
;;;; Evil
(evil-ex-lazy-highlight :foreground bg :background (doom-lighten green 0.3))
;;;; hl-line mod e
(hl-line :background (doom-lighten violet 0.85))
;;;; markdown mode
;; (markdown-markup-face :background (doom-lighten dark-red 0.98))
;; (markdown-pre-face :background (doom-lighten dark-red 0.98))
(markdown-code-face :distant-background (doom-lighten dark-red 0.98))

;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if pinky-winky-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if pinky-winky-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))
;;;; Base theme variable overrides-
  ())

;;; pinky-winky-theme.el ends here
