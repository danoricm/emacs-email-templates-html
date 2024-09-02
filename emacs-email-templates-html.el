;;; emacs-email-templates-html.el --- HTML email template support for Emacs -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: mail, templates, html, email
;; Homepage: https://github.com/danoricm/emacs-email-templates-html
;; License: GPL-3.0-or-later

;;; Commentary:

;; This package provides tools to create, validate, and convert HTML emails
;; to a format suitable for email clients. It includes a major mode for
;; editing HTML emails, functions for validating and converting HTML and CSS,
;; and integration with the emacs-email-templates package.

;;; Code:

(require 'emacs-email-templates-html-mode)
(require 'emacs-email-templates-html-validate)
(require 'emacs-email-templates-html-convert)

(provide 'emacs-email-templates-html)
;;; emacs-email-templates-html.el ends here
