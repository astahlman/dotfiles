(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(let ((default-directory "/Applications/Emacs.app/Contents/Resources"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq exec-path (append exec-path '("~/.emacs.d/elpa")))
(load "~/.emacs.d/user.el")
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-eclipse-dirs (quote ("/Applications/Eclipse.app/Contents/Eclipse")))
 '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim")
 '(package-selected-packages
   (quote
    (jedi format-sql pyvenv virtualenvwrapper yaml-mode w3m sr-speedbar solarized-theme smex smart-mode-line rainbow-delimiters projectile paredit ox-mediawiki org-mime org-gcal org-bullets org-beautify-theme nyan-mode muse markdown-mode latex-preview-pane langtool key-chord json-mode inf-ruby ido-ubiquitous idle-highlight-mode highlight-indentation helm-ls-git helm-gtags helm-ag google-translate gnuplot-mode gnuplot ggtags function-args flymd flymake-json flycheck find-file-in-project exec-path-from-shell evil-leader ess erc-terminal-notifier ensime elisp-slime-nav clojure-test-mode cljdoc cider buffer-move blockdiag-mode auctex ac-math))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
