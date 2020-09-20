;;; vtm.el --- Manages vterm buffers with configuration files -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-vterm-manager
;; Created: August 23th, 2020
;; Keywords: convenience
;; Package-Requires: ()
;; Version: 2.0

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
(require 'toml)

(defvar vtm-prefix-string "*VTM:"
  "Prefix string of the vterm buffer name.")
(defvar vtm-postfix-string "*"
  "Postfix string of the vterm buffer name.")

(declare-function vterm "ext:vterm.el" (&optional buffer-name) t)
(declare-function vterm-send-string "ext:vterm.el" (string &optional paste-p) t)
(declare-function vterm-send-return "ext:vterm.el" () t)

;;;###autoload
(define-minor-mode vtm-edit-mode
  "Enable editing the vtm file."
  :global t
  :init-value nil)

(defun vtm--populate-buffer ()
  "Populate a buffer with scaffold."
  (goto-char (point-min))
  (insert ";; ;; Uncomment lines to enable.\n")
  (insert "(\n")
  (insert " ;; ;; [Optional] name, default to filename.\n")
  (insert " ;; :name \"\"\n")
  (insert "\n")
  (insert " ;; ;; [Optional] commands to run, default to nil.\n")
  (insert " :commands\n")
  (insert "  (\n")
  (insert "\n")
  (insert "   ;; ;; First command.\n")
  (insert "   ;; (\n")
  (insert "    ;; ;; [Optional] sleep before command, default to 0.\n")
  (insert "    ;; :sleep 0\n")
  (insert "    ;; ;; [Optional] string to send, default to nil.\n")
  (insert "    ;; :string \"\"\n")
  (insert "    ;; ;; [Optional] additional control character to send.\n")
  (insert "    ;; ;; Check commands of /vterm-send-*/, default to return.\n")
  (insert "    ;; :control \"return\"\n")
  (insert "    ;; )\n")
  (insert "\n")
  (insert "   ;; ;; Second command.\n")
  (insert "   ;; (:sleep 0 :string \"\" :control \"return\")\n")
  (insert "\n")
  (insert "   ;; ;; Third command.\n")
  (insert "   ;; (:string \"\")\n")
  (insert "\n")
  (insert "  ))\n")
  (let ((case-fold-search nil))
    (search-backward "COMMAND"))
  (delete-char (length "COMMAND")))

(defvar vtm--conf nil)

(defun vtm--open-vterm ()
  "Open vterm with config from the current vtm buffer."
  (let* ((vtm-buffer (current-buffer))
         (conf (read vtm-buffer))
         (name (or (plist-get conf :name)
                   (file-name-base (buffer-name))))
         (vterm-name
          (format "%s%s%s" vtm-prefix-string name vtm-postfix-string))
         (vterm-buffer (get-buffer vterm-name))
         (commands (plist-get conf :commands)))
    (setq vtm--conf conf)
    (if vterm-buffer
        (switch-to-buffer vterm-buffer)
      (vterm vterm-name)

      (dolist (command commands)
        (let ((sleep (plist-get command :sleep))
              (str (plist-get command :string))
              (control (plist-get command :control)))
          (when sleep
            (sleep-for sleep))
          (when str
            (vterm-send-string str))
          (when control
            (funcall (intern (format "vterm-send-%s" control)))))))

    (kill-buffer vtm-buffer)))

;;;###autoload
(define-derived-mode vtm-mode emacs-lisp-mode "Elisp[vtm]"
  "Elisp Mode starter for vtm files."
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
