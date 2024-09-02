;;; emacs-email-templates-html-mode.el --- Major mode for HTML email templates -*- lexical-binding: t; -*-

;;; Commentary:
;; A major mode for writing HTML emails with validation and conversion
;; capabilities for creating email-compatible HTML content.

;;; Code:

(require 'emacs-email-templates-html-validate)
(require 'emacs-email-templates-html-convert)

(defvar emacs-email-html-mode-hook nil
  "Hook for `emacs-email-html-mode'.")

(defvar emacs-email-html-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") 'emacs-email-validate-html-buffer)
    (define-key map (kbd "C-c C-c") 'emacs-email-convert-html-to-template)
    map)
  "Keymap for `emacs-email-html-mode'.")

(define-derived-mode emacs-email-html-mode fundamental-mode "HTML-Email"
  "Major mode for editing HTML email templates."
  (setq font-lock-defaults '(nil))
  (setq mode-name "HTML-Email"))

(defun emacs-email-validate-html-buffer ()
  "Validate the current buffer's HTML content."
  (interactive)
  (emacs-email-validate-html (buffer-string)))

(defun emacs-email-convert-html-to-template ()
  "Convert the current buffer's HTML content to an email template."
  (interactive)
  (let ((html-content (buffer-string)))
    (if (emacs-email-validate-html html-content)
        (emacs-email-convert-to-template html-content)
      (message "HTML validation failed. Please fix the errors and try again."))))

(add-to-list 'auto-mode-alist '("\\.email\\.html\\'" . emacs-email-html-mode))

(provide 'emacs-email-templates-html-mode)
;;; emacs-email-templates-html-mode.el ends here
