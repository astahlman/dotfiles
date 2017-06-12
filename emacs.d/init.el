(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(let ((default-directory
        (car (delq nil (mapcar (lambda (d) (and (file-exists-p d) d))
                               '("/Applications/Emacs.app/Contents/Resources/"
                                 "/usr/local/share/emacs/site-lisp"))))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq exec-path (append exec-path '("~/.emacs.d/elpa")))
(load "~/.emacs.d/user.el")
