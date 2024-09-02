;;; emacs-email-templates-html-convert.el --- Convert HTML to email template -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to convert validated HTML content into Emacs Lisp email templates.

;;; Code:

(defun emacs-email-convert-to-template (html)
  "Convert validated HTML to an Emacs Lisp email template."
  (let ((template-name (read-string "Template Name: "))
        (subject (read-string "Subject: "))
        (validated-html (emacs-email-validate-html html)))
    (with-temp-buffer
      (insert (format ";;; %s.el --- Email template -*- lexical-binding: t; -*-\n\n" template-name))
      (insert (format "(setq %s-template\n" template-name))
      (insert (format "  '(:subject \"%s\"\n    :body \"%s\"))\n\n" subject validated-html))
      (insert (format "(provide '%s-template)\n" template-name))
      (write-file (concat "~/.emacs.d/emacs-email-templates/" template-name ".el")))
    (message "Template '%s' created successfully!" template-name)))

(provide 'emacs-email-templates-html-convert)
;;; emacs-email-templates-html-convert.el ends here
