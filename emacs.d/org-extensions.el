;(load "org.el")

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
