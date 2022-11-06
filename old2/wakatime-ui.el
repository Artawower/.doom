;;; .el --- wakatime-ui                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL:  https://github.com/artawower/wakatime-ui.el
;; Package-Requires: ((emacs "24.4") (wakatime-mode "1.0.2"))
;; Version: 0.1

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
;; Package for provide ui functionality for popular time tracker - wakatime.

;;; Code:
(require 'wakatime-mode)
(require 'url)
(require 'posframe)

(defcustom wakatime-ui--update-timeout 120
  "Check timeout in seconds."
  :group 'wakatime-ui
  :type 'int)

(defcustom wakatim-ui-schedule-url nil
  "Url of chart")

(defface wakatime-ui--modeline-face
  '((t :foreground "#f65866"
       :background nil
       :bold t))
  "Face for wakatime ui modeline info."
  :group 'wakatime-ui)

(defvar wakatime-ui--check-timer nil
  "Current timer process for checking wakatime information")

(defvar wakatime-ui--buffer-name "*WakatimeUI*"
  "Buffer name for process output.")

(defvar wakatime-ui--binary-name "wakatime-cli"
  "Name of binary for wakatime api.")

(defvar wakatime-ui--command-args '(:today-time "--today")
  "Plist of available arguments")

(defvar wakatime-ui--busy nil
  "Is there wakatime ui busy right now?")

(defun wakatime-ui--format-time ())

(defun wakatime-ui--update-time (text)
  "Update modeline information by TEXT."
  ;; TODO: add face for modeline info, check correct modeline mode
  ;; (setq-default mode-line-misc-info (propertize text 'face '(:foreground "red" :bold t))))
  ;; (setq global-mode-string (propertize text 'face '(:foreground "#f65866"))))
  ;; (add-to-list 'mode-line-misc-info (propertize (concat text " ") 'face '(:foreground "#f65866"))))
  ;; (setq-default mode-line-misc-info (propertize (concat text " ") 'face '(:foreground "#f65866"))))
  ;; (setq-default mode-line-misc-info (concat text " ")))
  (setq-default mode-line-misc-info (propertize text 'face '(:foreground "#f65866"))))

(defun wakatime-ui--clear-modeline (&optional directory cache)
  "Clear modeline information."
  (interactive)
  ;; (message "mee")
  (setq-default mode-line-misc-info nil))

(defun wakatime-ui--handle-process-output (process signal buffer-name)
  "Handle background PROCESS SIGNAL and BUFFER-NAME."
  (when (memq (process-status process) '(exit signal))
    ;; TODO: check (process-exist-status process) 0!
    (save-window-excursion
      (switch-to-buffer-other-window buffer-name)
      (let* ((output (buffer-substring (point-min) (point-max))))
        (kill-matching-buffers buffer-name nil t)
        (wakatime-ui--update-time (replace-regexp-in-string "\n\\'" "" output))))
    (shell-command-sentinel process signal)
      (setq wakatime-ui--busy nil)))

(defun wakatime-ui--get-changes ()
  "Get changes of current spent time."
  (message "Get changes from wakatime!")
  (unless wakatime-ui--busy
    (let* ((binary (wakatime-find-binary wakatime-ui--binary-name))
           (process (start-process
                     "WakatimeUI"
                     wakatime-ui--buffer-name
                     binary
                     (plist-get wakatime-ui--command-args :today-time))))
      (setq wakatime-ui--busy t)
      (when (process-live-p process)
        (set-process-sentinel process
                              #'(lambda (proc signal)
                                  (wakatime-ui--handle-process-output
                                   proc
                                   signal
                                   wakatime-ui--buffer-name)))))))

(defun wakatime-ui--start-watch-time ()
  (unless wakatime-ui--check-timer
    (message "Wakatime ui check time")
    (setq wakatime-ui--check-timer
          (run-with-timer 20 wakatime-ui--update-timeout 'wakatime-ui--get-changes))))

(defun wakatime-ui--watch-time ()
  "Subscribe to time activity.
Could be stopped by `wakatime-ui--stop-watch-time'"
  (add-hook 'doom-first-file-hook 'wakatime-ui--start-watch-time))

(defun wakatime-ui--stop-watch-time ()
  "Stop to subscribe time activity."
  (remove-hook 'doom-first-file-hook 'wakatime-ui--start-watch-time)
  (when wakatime-ui--check-timer
    (cancel-timer wakatime-ui--check-timer)
    (setq wakatime-ui--check-timer nil)))

;;;###autoload
(define-minor-mode wakatime-ui-mode
  "Wakatime ui mode. Add time track to doom modeline.
TODO:
Add support for other modeline in future."
  :init-value nil
  :global t
  :lighter nil
  :group 'wakatime-ui
  (if wakatime-ui-mode
      (progn
        (wakatime-ui--watch-time))
    (wakatime-ui--stop-watch-time)))

;;;###autoload
(define-globalized-minor-mode
  global-wakatime-ui-mode
  wakatime-ui-mode
  (lambda ()
    (unless wakatime-ui-mode
      (wakatime-ui-mode)))
  :group 'wakatime-ui)

(defun wakatime-ui--insert-image-from-url (&optional url)
  (interactive)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert-image (create-image data nil t :scale 0.8))
          (insert "\nwakatime!"))
      (kill-buffer buffer))))

(defvar wakatime-ui--preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'posframe-hide-all)
    (define-key map "C-q" 'posframe-hide-all)
    map))

(define-minor-mode wakatime-ui--preview-mode
  "Wakatime ui preview"
  :init-value nil
  :global nil
  :lighter nil
  :group 'wakatime-ui
  :keymap wakatime-ui--preview-mode-map)


;;;###autoload
(defun wakatime-ui-show-dashboard ()
  "Show dashboard with information about current widget"
  (interactive)
  (let ((my-posframe-buffer "*wakatime-ui*"))

    (with-current-buffer (get-buffer-create my-posframe-buffer)
      (erase-buffer)

      (wakatime-ui--insert-image-from-url wakatim-ui-schedule-url))

    (when (posframe-workable-p)
      (message "posframe show")
      (run-at-time 1 0
                   #'(lambda ()
                       (posframe-show my-posframe-buffer
                                      :poshandler 'posframe-poshandler-frame-top-center
                                      :border-width 2
                                      :border-color "blue"
                                      :accept-focus t)
                       ;; (wakatime-ui--preview-mode)
                       )))))

(provide 'wakatime-ui)
;;; wakatime-ui.el ends here
