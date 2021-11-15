;;; tokio-night-theme.el --- Tokion night theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2021 Artur Yaroshenko
;;
;; Keywords: custom themes, faces
;; Homepage: https://github.com/Artawower/.doom
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Stolen from codeart
;;; Code:

(require 'doom-themes)


;;; Variables

(defgroup tokio-night-theme nil
  "Options for the `tokio-night' theme."
  :group 'doom-themes)

(defcustom tokio-night-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'tokio-night-theme
  :type 'boolean)

(defcustom tokio-night-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'tokio-night-theme
  :type 'boolean)

(defcustom tokio-night-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'tokio-night-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme tokio-night
  "Tokio night doom theme"
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
   (red        '("#f7768e" "#f7768e" "red"          ))
   (orange     '("#ff9e64" "#ff9e64" "brightred"    ))
   (green      '("#9ece6a" "#9ece6a" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#e0af68" "#e0af68" "yellow"       ))
   (blue       '("#7aa2f7" "#7aa2f7" "brightblue"   ))
   (dark-blue  '("#1c4a6e" "#1c4a6e" "blue"         ))
   (magenta    '("#bb9af7" "#bb9af7" "brightmagenta"))
   (violet     '("#9d7cd8" "#9d7cd8" "magenta"      ))
   (cyan       '("#7dcfff" "#7dcfff" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if tokio-night-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if tokio-night-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      blue)
   (keywords       (doom-lighten violet 0.2))
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
   (modeline-fg-alt          base5)
   (modeline-bg              (if tokio-night-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if tokio-night-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (tokio-night-modeline-pad
    (when tokio-night-padded-modeline
      (if (integerp tokio-night-padded-modeline) tokio-night-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if tokio-night-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if tokio-night-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if tokio-night-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if tokio-night-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if tokio-night-brighter-modeline modeline-bg highlight))
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
    :box (if tokio-night-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if tokio-night-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; tokio-night-theme.el ends here
