
(require 'muse-mode)
(require 'muse-html)
(require 'muse-wiki)
(require 'muse-project)

;; (setq muse-project-alist
;;       '(("my-wiki" ("~/Documents/Wiki" :default "home")
;;          (:base "wiki" :path "~/Documents/Wiki/html"))))

(setq muse-project-alist
      `(("my-wiki" (,@(muse-project-alist-dirs "~/Documents/Wiki")
                    :default "home")
         ,@(muse-project-alist-styles "~/Documents/Wiki"
                                      "~/Documents/Wiki/html"
                                      "html"))))

(setq debug-on-error t)
(setq exclude-dirs (list "." ".." "html"))

(defun project-root (proj)
  (expand-file-name (caadr (muse-project proj))))

(defun -ls (dir)
  (remove-if (lambda (x) (member x exclude-dirs)) (directory-files dir)))

(defun to-path (s-path)
  (remove-if-not (lambda (x) (> (length x) 0))
                 (split-string (expand-file-name s-path) "/")))

(defun cd-up (path)
  "Accepts a list of path elements or a list with two lists of path elements.
   In the first case, return a list with two nested lists. The first list contains
   everything up until the terminal element. The second list contains the terminal
   element. In the second case, push the last element in the first list to the front
   of the second list."
  (if (not (listp (car path)))
      (cons (butlast path) (list (last path)))
    (let ((before (car path))
          (end (car (cdr path))))
      (list (butlast before) (cons (car (last before)) end)))))

(defun extract-path (path proj)
  (let* ((split (cd-up path))
         (base (car split))
         (proj-base (to-path (project-root proj)))
         (p (car (cdr split))))
    (if (equal base proj-base)
        p
      (extract-path split proj))))

(setq my-file "~/Documents/Wiki/Tools/Firefox.muse")
;(extract-path (to-path my-file) "my-wiki")

(defun string-join (s sep)
  "Join string 's' using separator 'sep'"
  (mapconcat 'identity s sep))

(defun write-header ()
  (let* ((proj "my-wiki")
         (path-els (extract-path (to-path muse-publishing-current-file) proj))
         (el-to-str (lambda (x) (concat "[" x "]"))))
    (concat (string-join (map 'list el-to-str path-els) "//") "\n\n")))

(defun insert-breadcrumbs ()
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (insert (write-header))
  (exchange-point-and-mark))

(defun insert-breadcrumbs-proj (proj)
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (insert (write-header))
  (exchange-point-and-mark))

; TODO: Fix this
;(add-hook 'muse-before-publish-hook 'insert-breadcrumbs)

(defun link-to-node (n)
  (if (file-directory-p n)
    (concat (file-name-as-directory n) "index.html")
    n))

(defun display-name-for-node (n)
  (cond ((string-match "\\..$" n) "..")
        (t (file-name-base n))))

(file-name-base "~/Documents/Wiki/..")

(defun listing (dir)
  (let ((paths (directory-files dir t)))
    (mapcar (lambda (p) (concat "[[" (link-to-node p) "][" (display-name-for-node p) "]]")) paths)))

(listing "~/Documents/Wiki/Notes")
(defun add-listing (dir)
  (with-temp-file
      (concat (file-name-as-directory dir) "index.muse")
    (insert (string-join (listing dir) "\n"))))

(setq muse-project-ignore-regexp (concat muse-project-ignore-regexp "\\|.+/Wiki/html.?"))

(defun add-listings-hook (project)
  (mapcar 'add-listing
          (muse-project-alist-dirs (caadr project))))

(add-hook 'muse-after-project-publish-hook 'add-listings-hook)
