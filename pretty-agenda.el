;;; pretty-agenda.el --- just a template for agenda                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/pretty-agenda
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This simple package provide beautiful agenda template.
;; It being used with Fira Code font. It can be more ugly with other fonts.
;; I dont't recommend to use this package, cause i totally don't understand real implementation of
;; org-agenda-get-restriction-and-command in many cases. I'll try to refactor and split original code
;; in the future and optimize render process.

(customize-set-value
 'org-agenda-category-icon-alist
 `(
   ("work" "~/.doom.d/icons/community.svg" nil nil :ascent center :mask heuristic)
   ("finance" "~/.doom.d/icons/briefcase.svg" nil nil :ascent center :mask heuristic)
   ("bonds" "~/.doom.d/icons/briefcase.svg" nil nil :ascent center :mask heuristic)
   ("health" "~/.doom.d/icons/home.svg" nil nil :ascent center :mask heuristic)
   ("english" "~/.doom.d/icons/class.svg" nil nil :ascent center :mask heuristic)
   ("self-education" "~/.doom.d/icons/book.svg" nil nil :ascent center :mask heuristic)
   ("freelance" "~/.doom.d/icons/money-bag.svg" nil nil :ascent center :mask heuristic)))

(setq org-agenda-hidden-separator "‌‌ ")

(defun agenda-color-char ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "⚡" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:height 220 :foreground "gold2" :bold t)))))

(setq org-agenda-block-separator (string-to-char " "))

(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)

(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date 1 nil))
         (day (cadr date))
         (persian (substring (calendar-persian-date-string date) 0 -6))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month 1))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
    (format " %-2s. %2d %s, %s"
            dayname day monthname persian)))

(setq org-agenda-block-separator nil)

(setq org-agenda-custom-commands
      '(("a" "My Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero nil)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 2)
                      (org-agenda-overriding-header "⚡ Calendar")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")
                      ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                      ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline: " "Deadline: "))
                      (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

          (todo "TODO" (
                        (org-agenda-overriding-header "\n⚡ To Do")
                        (org-agenda-sorting-strategy '(priority-down))
                        (org-agenda-remove-tags t)
                        ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-prefix-format "   %-2i %?b")
                        (org-agenda-todo-keyword-format "")))

          ;; (todo "NEXT" (
          ;;              (org-agenda-todo-ignore-scheduled 'all)
          ;;              (org-agenda-overriding-header "⚡ THIS WEEK")
          ;;              (org-agenda-remove-tags t)
          ;;              (org-agenda-prefix-format "   %-2i %?b")
          ;;              (org-agenda-todo-keyword-format "")))

          (tags "+project" (
                            (org-agenda-overriding-header "\n⚡ Projects")
                            (org-agenda-remove-tags t)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-show-inherited-tags nil)
                            (org-agenda-prefix-format "   %-2i %?b")
                            (org-agenda-todo-keyword-format "")))

          ;; (org-ql-block '(and
          ;;                 (tags "project")
          ;;                 )
          ;;               (
          ;;                (org-ql-block-header "⚡ Projects and Areas")
          ;;                ))
          ))
        ("po" "Personal Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero nil)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 3)
                      (org-agenda-overriding-header "⚡ Calendar")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")
                      ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                      ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline: " "Deadline: "))
                      (org-agenda-time-grid nil)))

          (todo "TODO" (
                        (org-agenda-overriding-header "\n⚡ To Do")
                        (org-agenda-sorting-strategy '(priority-down))
                        (org-agenda-remove-tags t)
                        ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-prefix-format "   %-2i %?b")
                        (org-agenda-todo-keyword-format "")))

          ))))



;;; Agenda looks
(use-package org-agenda)
;; (defcustom pretty-agenda-template--max-str-length 36)

(defcustom pretty-agenda--border-color "#61AFEF"
  "Color of agenda border")
(defcustom pretty-agenda--icon-height 108
  "Icon height")

(defun pretty-agenda-template--format-first-column (text &optional col-length)
  "Format TEXT inside first column by COL-LENGTH."
  (let* ((col-len (or col-length 36))
         (text-length (length text))
         (text (if (= text-length 0) "   " text))
         (need-spaces (- col-len text-length)))
    (format "│  %s%s                                              │"
            text
            (make-string need-spaces ? ))))

(setq pretty-agenda-symbol-icon-map
  '(("a" "📅" "white")
    ("t" "📋" "white")
    ("m" "📝" "white")
    ("s" "🔎" "white")
    ("/" "📁" "white")
    ("?" "📒" "white")
    ("*" "✅" "white")
    ("#" "👾" "white")
    ("e" "💾" "white")
    (">" "👉" "white")
    ("<" "👈" "white")
    ("T" "📝" "white")
    ("M" "📝" "white")
    ("S" "📝" "white")
    ("C" "⚙" "white")
    ("o" "🌏" "white")
    ("p" "⤵" "white"))
)

(defcustom pretty-agenda-bottom-border "└───────────────────────────────────────────────────────────────────────────────────────┘"
  "Bottom line.")
(defcustom pretty-agenda-header-detail-line "┌─ My Agenda detail ────────────────────────────────────────────────────────────────────┐"
  "Top line for detail view.")

(defcustom pretty-agenda-raw-table
  "
┌─ Press key for an agenda command ───────┬─────────────────────────────────────────────┐
│                                         │                                             │
│  a   Agenda for current week or day  │  <   Buffer, subtree/region restriction  │
│  t   List of all TODO entries        │  >   Remove restriction                  │
│  m   Match a TAGS/PROP/TODO query    │  e   Export agenda views                 │
│  s   Search for keywords             │  T   Entries with special TODO kwd       │
│  /   Multi-occur                     │  M   Like m, but only TODO entries       │
│  ?   Find :FLAGGED: entries          │  S   Like s, but only TODO entries       │
│  *   Toggle sticky agenda views      │  C   Configure custom agenda commands    │
│                                         │  #   List stuck projects (!=configure)   │
│                                         │                                             │"
  "Awful template of table >.<")

(defun pretty-agenda--colorize-view ()
  "Beautify current buffer."
  (remove-text-properties (point-min) (point-max) '(face nil))
  (let* ((buffer-text (buffer-substring (point-min) (point-max))))
    (erase-buffer)
    (insert (propertize buffer-text 'face `(:foreground ,pretty-agenda--border-color)))
    (goto-char 0)
    ;; TODO: make with one iteration
    (while (search-forward-regexp "[\-a-zA-Z\/\!=\(\),\:]+" nil 'noerror)
      (replace-match (propertize (match-string 0) 'face 'font-lock-comment-face)))
    (goto-char 0)
    (while (search-forward-regexp "  \\(?1:[A-Z><#/?*]\\)  " nil 'noerror)
      (let* ((agenda-cmd (match-string 1))
             (pretty-symbol-icon (assoc agenda-cmd pretty-agenda-symbol-icon-map))
             (icon-color (nth 2 pretty-symbol-icon))
             (icon (nth 1 pretty-symbol-icon))
             (small-stub (propertize "   " 'face '(:height 15)))
             (icon (if pretty-symbol-icon
                       (propertize (nth 1 pretty-symbol-icon) 'face `(:foreground ,icon-color :height ,pretty-agenda--icon-height)) " ")))

        (if (string-match icon (thing-at-point 'line))
            (ignore)
          (replace-match (format "  %s%s  %s " icon small-stub (propertize agenda-cmd 'face 'font-lock-string-face))))))
    (goto-char 0)
    (while (search-forward-regexp "─ \\(?1:[A-Za-z ]+[a-z]\\) ─" nil 'noerror)
      (replace-match (propertize (match-string 1) 'face 'font-lock-constant-face) nil nil nil 1)
      (defface tmp-buffer-local-face
        '((t :family "Fira Code" :size 16))
        "Temporary buffer-local face")
      (buffer-face-set 'tmp-buffer-local-face)
      )))

(defun org-agenda-get-restriction-and-command (prefix-descriptions)
  "The user interface for selecting an agenda command."
  (message "its arg %s" prefix-descriptions)
  (catch 'exit
    (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	   (restrict-ok (and bfn (derived-mode-p 'org-mode)))
	   (region-p (org-region-active-p))
	   (custom org-agenda-custom-commands)
	   (selstring "")
           (prettified nil)
	   restriction second-time
	   c entry key type match prefixes rmheader header-end custom1 desc
	   line lines left right n n1 is-detail-view)
      (save-window-excursion
	(delete-other-windows)
	(org-switch-to-buffer-other-window " *Agenda Commands*")
	(erase-buffer)
	(insert (eval-when-compile
		  (let ((header (copy-sequence pretty-agenda-raw-table))
			(start 0))
		    header)))
	(setq header-end (point-marker))
        (setq lines nil)

	(while t
          (setq is-detail-view (and selstring (not (eq selstring ""))))


          (if is-detail-view
              (progn
                (setq lines nil)
                (erase-buffer)))

	  (setq custom1 custom)
          (org-goto-line 1)
	  (goto-char header-end)

	  ;; Produce all the lines that describe custom commands and prefixes
	  (while (setq entry (pop custom1))
	    (setq key (car entry) desc (nth 1 entry)
		  type (nth 2 entry)
		  match (nth 3 entry))
	    (if (> (length key) 1)
		(cl-pushnew (string-to-char key) prefixes :test #'equal)
	      (setq line
		    (format
		     "%-4s%-14s"
		     (org-add-props (copy-sequence key)
			 '(face bold))
		     (cond
		      ((string-match "\\S-" desc) desc)
		      ((eq type 'agenda) "Agenda for current week or day")
		      ((eq type 'agenda*) "Appointments for current week or day")
		      ((eq type 'alltodo) "List of all TODO entries")
		      ((eq type 'search) "Word search")
		      ((eq type 'stuck) "List of stuck projects")
		      ((eq type 'todo) "TODO keyword")
		      ((eq type 'tags) "Tags query")
		      ((eq type 'tags-todo) "Tags (TODO)")
		      ((eq type 'tags-tree) "Tags tree")
		      ((eq type 'todo-tree) "TODO kwd tree")
		      ((eq type 'occur-tree) "Occur tree")
		      ((functionp type) (if (symbolp type)
					    (symbol-name type)
					  "Lambda expression"))
		      (t "???"))))
	      (cond
	       ((not (org-string-nw-p match)) nil)
	       (org-agenda-menu-show-matcher
		(setq line
		      (concat line ": "
			      (cond
			       ((stringp match)
				(propertize match 'face 'org-warning))
			       ((listp type)
				(format "set of %d commands" (length type)))))))
	       (t
		(org-add-props line nil 'help-echo (concat "Matcher: " match))))
              (push (pretty-agenda-template--format-first-column line) lines)))

          (if (not is-detail-view)
              (push (pretty-agenda-template--format-first-column "")  lines))


          (push pretty-agenda-bottom-border lines)
	  (setq lines (nreverse lines))
	  (when prefixes
	    (mapc (lambda (x)
		    (push
		     (pretty-agenda-template--format-first-column (format "%s   %s"
			                                                  (org-add-props (char-to-string x)
				                                              nil 'face 'bold)
			                                                  (or (cdr (assoc (concat selstring
						                                                  (char-to-string x))
					                                                  prefix-descriptions))
				                                              "Prefix key")))
		     lines)
                    (push (pretty-agenda-template--format-first-column "")  lines)
                    (if (not is-detail-view)
                        (push "├─ Custom ────────────────────────────────┴─────────────────────────────────────────────┤" lines)))
		  prefixes))

          (if is-detail-view
              (add-to-list 'lines pretty-agenda-header-detail-line))

	  ;; Check if we should display in two columns
	  (if org-agenda-menu-two-columns
	      (progn
		(setq n (length lines)
		      n1 (+ (/ n 2) (mod n 2))
		      right (nthcdr n1 lines)
		      left (copy-sequence lines))
		(setcdr (nthcdr (1- n1) left) nil))
	    (setq left lines right nil))
	  (while left
	    (insert "\n" (pop left))
	    (when right
	      (if (< (current-column) 40)
		  (move-to-column 40 t)
		(insert "   "))
	      (insert (pop right))))

	  ;; Make the window the right size
	  (goto-char (point-min))
	  (if second-time
	      (when (not (pos-visible-in-window-p (point-max)))
		(org-fit-window-to-buffer))
	    (setq second-time t)
	    (org-fit-window-to-buffer))
          (pretty-agenda--colorize-view)
	  ;; Hint to navigation if window too small for all information
	  (setq header-line-format
		(when (not (pos-visible-in-window-p (point-max)))
		  "Use C-v, M-v, C-n or C-p to navigate."))

	  ;; Ask for selection
	  (cl-loop
	   do (progn
		(message "Press key for agenda command%s:"
			 (if (or restrict-ok org-agenda-overriding-restriction)
			     (if org-agenda-overriding-restriction
				 " (restriction lock active)"
			       (if restriction
				   (format " (restricted to %s)" restriction)
				 " (unrestricted)"))
			   ""))
		(setq c (read-char-exclusive)))
	   until (not (memq c '(14 16 22 134217846)))
	   do (org-scroll c))

	  (cond
	   ((assoc (char-to-string c) custom)

	    (setq selstring (concat selstring (char-to-string c)))
	    (throw 'exit (cons selstring restriction)))
	   ((memq c prefixes)

	    (setq selstring (concat selstring (char-to-string c))
		  prefixes nil
		  rmheader (or rmheader t)
		  custom (delq nil (mapcar
				    (lambda (x)
				      (if (or (= (length (car x)) 1)
					      (/= (string-to-char (car x)) c))
					  nil
					(cons (substring (car x) 1) (cdr x))))
				    custom))))
	   ((eq c ?*)
	    (call-interactively 'org-toggle-sticky-agenda)
	    (sit-for 2))
	   ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
	    (ding) (sit-for 1))
	   ((eq c ?1)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction 'buffer))
	   ((eq c ?0)

	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction (if region-p 'region 'subtree)))
	   ((eq c ?<)
	    (org-agenda-remove-restriction-lock 'noupdate)

	    (setq restriction
		  (cond
		   ((eq restriction 'buffer)
		    (if region-p 'region 'subtree))
		   ((memq restriction '(subtree region))
		    nil)
		   (t 'buffer))))
	   ((eq c ?>)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction nil))
	   ((and (equal selstring "") (memq c '(?s ?S ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/ ??)))
	    (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
           ((and (> (length selstring) 0) (eq c ?\d))
            (delete-window)
            (org-agenda-get-restriction-and-command prefix-descriptions))

	   ((equal c ?q) (user-error "Abort"))
	   (t (user-error "Invalid key %c" c))))))))


(provide 'pretty-agenda)

;;; my-agenda.el ends here
