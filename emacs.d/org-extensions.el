;; Supports export to Markdown
(add-to-list 'org-export-backends 'md)

(defun today-string ()
  (format-time-string "%Y-%m-%d" (current-time)))

(defun mark-today ()
  "Add today's date to this header's properties drawer"
  (interactive)
  (save-excursion
    (outline-previous-visible-heading 1)
    (message "%s" (org-today))
    (org-entry-add-to-multivalued-property (point-at-eol) "date" (today-string))))

(global-set-key (kbd "C-c C-x t") 'mark-today)

(defun replace-stars (i)
  (interactive "n")
  (query-replace (make-string i ?*) (concat ".h" (number-to-string i))))

(defun export-to-dp (i)
  "Export an org-mode file to dp, this is a work in progress"
  (interactive "n")
  (mark-whole-buffer)
  (if (= i 0)
      nil
    (replace-stars i)
    (export-to-dp (- i 1))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))

;; org-beautify-theme is installed in /elpa, not /themes
;; We have to specifically add it to the custom-theme-load-path
(let ((org-beautify-el (car (file-expand-wildcards "~/.emacs.d/elpa/org-beautify*"))))
  (when
      (and (boundp 'custom-theme-load-path) org-beautify-el)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory org-beautify-el))))

(setq org-startup-indented t)

(require 'org-mime)

;; TODO: Load everything in the patches directory from user.el
(load "~/.emacs.d/patches/ob-core-patch.el")

