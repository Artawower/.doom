;;; aurora-theme.el --- Aurora theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2021 Artur Yaroshenko
;;
;; Keywords: custom themes, faces
;; Homepage: https://github.com/Artawower/.doom
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Aurora theme. From vscode.
;;; Code:

(require 'doom-themes)


;;; Variables

(defgroup aurora-theme nil
  "Options for the `aurora' theme."
  :group 'doom-themes)

(defcustom aurora-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'aurora-theme
  :type 'boolean)

(defcustom aurora-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'aurora-theme
  :type 'boolean)

(defcustom aurora-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'aurora-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme aurora
  "A dark theme inspired by ."

  ;; name        default   256           16
  ((bg         '("#07090F" "#07090F"      "black"  ))
   (fg         '("#C7D5FF" "#C7D5FF"     "brightwhite"  ))

   (bg-alt     '("#07090F" "#07090F"       "black"        ))
   (fg-alt     '("#576daf" "#576daf"     "white"        ))

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
   (red        '("#f07178" "#f07178" "red"          ))
   (orange     '("#dd9046" "#dd9046" "brightred"    ))
   (green      '("#63eb90" "#63eb90" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#FFCB6B" "#FFCB6B" "yellow"       ))
   (blue       '("#82AAFF" "#82AAFF" "brightblue"   ))
   (dark-blue  '("#A8BEFF" "#A8BEFF" "blue"         ))
   (magenta    '("#C792EA" "#C792EA" "brightmagenta"))
   (violet     '("#c778db" "#c778db" "magenta"      ))
   (cyan       '("#89DDFF" "#89DDFF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        cyan)
   (comments       (if aurora-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if aurora-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      blue)
   (keywords       violet)
   (methods        red)
   (operators      cyan)
   (type           (doom-lighten yellow 0.2))
   (strings        green)
   (variables      red)
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
   (modeline-fg-alt          base5)
   (modeline-bg              (if aurora-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken "#86A5FF" 0.75)))
   (modeline-bg-alt          (if aurora-brighter-modeline
                                 (doom-darken blue 0.475)
                               (doom-darken "#86A5FF" 0.75)))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (aurora-modeline-pad
    (when aurora-padded-modeline
      (if (integerp aurora-padded-modeline) doom-deep-one-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if aurora-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if aurora-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if aurora-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if aurora-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if aurora-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if aurora-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if aurora-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; aurora-theme.el ends here
