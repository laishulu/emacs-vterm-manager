;;; sis.el --- Manages vterm buffers with configuration files -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-vterm-manager
;; Created: August 23th, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package manages vterm buffers with configuration files.
;; For more information see the README in the GitHub repo.

;;; Code:
(require 'subr-x)

(defvar vtm-prefix-string "VTM"
  "Prefix string of the vterm buffer name.")

(declare-function vterm "ext:vterm.el" (&optional buffer-name) t)
(declare-function vterm-send-string "ext:vterm.el" (string &optional paste-p) t)
(declare-function vterm-send-return "ext:vterm.el" () t)

;;;###autoload
(define-minor-mode vtm-edit-mode
  "Enable editing the vtm file."
  :global t
  :init-value nil)

;; load feature `toml'
(unless (featurep 'toml)
  (load (expand-file-name "toml" (file-name-directory load-file-name))))

(defun vtm--populate-buffer ()
  "Populate a buffer with scaffold."
  (goto-char (point-min))
  (insert "# TOML format, uncomment line to enable.\n")
  (insert "# (optional) verbose name, default to filename.\n")
  (insert "# vebose = \"\"\n")
  (insert "# (optional) command to run, default to nil.\n")
  (insert "# command = \"COMMAND\"\n")
  (insert "# (optional) sleep before command, default to 0.\n")
  (insert "# sleep = 0\n")
  (let ((case-fold-search nil))
    (search-backward "COMMAND"))
  (delete-char (length "COMMAND")))

(defun vtm--open-vterm ()
  "Open vterm with config from the current vtm buffer."
  (let* ((vtm-buffer (current-buffer))
         (conf (toml:read-from-string (buffer-string)))
         (name (or(assoc-default "verbose" conf)
                  (file-name-base (buffer-name))))
         (vterm-name (format "*VTM:%s*" name))
         (sleep (assoc-default "sleep" conf nil))
         (command (assoc-default "command" conf))
         (vterm-buffer (get-buffer vterm-name)))
    (if vterm-buffer
        (switch-to-buffer vterm-buffer)
      (vterm vterm-name)
      (when sleep
        (sleep-for sleep))
      (when command
        (vterm-send-string command)
        (vterm-send-return)))
    (kill-buffer vtm-buffer)))

;;;###autoload
(define-derived-mode vtm-mode conf-toml-mode "TOML[vtm]"
  "TOML Mode starter for vtm files."
  (if (= 0 (length (string-trim (buffer-string))))
      (vtm--populate-buffer)
    (unless vtm-edit-mode
      (unless (featurep 'vterm)
        (require 'vterm nil t))
      (if (featurep 'vterm)
          (vtm--open-vterm)
        (message "The vterm feature is unavailable.")))))

(setq auto-mode-alist
      (append
       '(;; File name ends in ‘.vtm’.
         ("\\.vtm\\'" . vtm-mode))
       auto-mode-alist))

(provide 'vtm)
;;; vtm.el ends here
