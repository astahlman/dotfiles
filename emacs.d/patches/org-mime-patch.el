(defun org-mime-htmlize (&optional arg)
  "Export to HTML an email body composed using `mml-mode'.
If called with an active region only export that region,
otherwise export the entire body."
  (interactive "P")
  (require 'ox-org)
  (require 'ox-html)
  (let* ((region-p (org-region-active-p))
         (html-start (or (and region-p (region-beginning))
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward mail-header-separator)
                           (+ (point) 1))))
         (html-end (or (and region-p (region-end))
                       ;; TODO: should catch signature...
                       (point-max)))
         (raw-body (concat org-mime-default-header
                           (buffer-substring html-start html-end)))
         (tmp-file (make-temp-name (expand-file-name
                                    "mail" temporary-file-directory)))
         (body (org-export-string-as raw-body 'org t))
         ;; because we probably don't want to export a huge style file
         (org-export-htmlize-output-type 'inline-css)
         ;; makes the replies with ">"s look nicer
         (org-export-preserve-breaks org-mime-preserve-breaks)
         ;; dvipng for inline latex because MathJax doesn't work in mail
         (org-html-with-latex 'dvipng)
         ;; to hold attachments for inline html images
         (html-and-images
          (org-mime-replace-images
           (org-export-string-as raw-body 'html t) tmp-file))
         (html-images (unless arg (cdr html-and-images)))
         (html (org-mime-apply-html-hook
                (if arg
                    (format org-mime-fixedwith-wrap body)
                  (car html-and-images)))))
    (delete-region html-start html-end)
    (message "HIIII")
    (save-excursion
      (goto-char html-start)
      (insert (org-mime-multipart
               body html (mapconcat 'identity html-images "\n"))))))
