;; TODO: Bootstrap this...
(require 'epl)
(require 'cl)

;; This is where your customizations should live
(setq my-packages
      '(ac-math
        annotate
        async
        auctex
        auto-complete
        blockdiag-mode
        buffer-move
        cider
        cl-lib
        cljdoc
        clj-refactor
        clojure-mode
        clojure-test-mode
        company
        dash
        elisp-slime-nav
        elpy
        ensime
        epl
        erc-terminal-notifier
        ess
        evil
        evil-leader
        exec-path-from-shell
        feature-mode
        find-file-in-project
        flycheck
        flycheck-package
        flymake-json
        flymd
        format-sql
        function-args  ; for C/C++ function argument completion
        ggtags
        git-link
        gnuplot
        gnuplot-mode
        google-translate
        helm
        helm-ag
        helm-company
        helm-git-grep
        helm-gtags  ; helm integration with GNU global
        helm-ls-git
        highlight-indentation
        idle-highlight-mode
        ido-ubiquitous
        inf-ruby
        jedi
        json-mode
        key-chord
        langtool
        latex-preview-pane
        lsp-mode
        lsp-python
        ;;magit
        magit-gh-pulls
        markdown-mode
        math-symbol-lists
        muse
        nyan-mode
        ob-async
        org-plus-contrib
        org-beautify-theme
        org-bullets
        org-gcal
        org-mime
        org-random-todo
        ox-mediawiki
        package-lint
        paredit
        pkg-info
        popup
        projectile
        pyvenv
        queue
        rainbow-delimiters
        s
        sbt-mode
        smart-mode-line
        smex
        solarized-theme
        sr-speedbar
        with-editor
        writegood-mode
        w3m
        yaml-mode
        yasnippet))

;; This looks weird, but it fixes a weird race condition where
;; 'epl-package-outdated-p throws an error because package--builtins
;; doesn't get populated until it 'package-built-in-p gets called with
;; a built-in package
(package-built-in-p 'org)

;; Activate all the packages
(package-initialize)

;; TODO: I don't think the auto-updating via package-install works...
; Install anything that's missing
; Take the latest version of built-in packages, e.g. org
(dolist (package my-packages)
  (unless (and (package-installed-p package)
               (not (epl-package-outdated-p package)))
    (message "Installing %s..." package)
    (package-install package)
    (message "Done.")))

;; https://github.com/purcell/exec-path-from-shell
(require 'exec-path-from-shell)

;; If we are on OSX, just use exec-from-shell
;; Else, get the PATH from our .zshrc
(if (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

;; env PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it

(setq initial-frame-alist '((top . 10) (left . 10) (width . 150) (height . 45)))


;; Place downloaded elisp files in this directory. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)
(load-theme 'tomorrow-night-bright t)

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

(defun smart-open-line-above ()
  "Mimic vim's 'O'"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-S-O") 'smart-open-line-above)

(defun smart-open-line-below ()
  "Mimic vim's 'o'"
  (interactive)
    (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "C-S-j") 'smart-open-line-below)

(defun help-at-point ()
  (interactive)
  (describe-function (symbol-at-point)))

(global-set-key (kbd "C-h SPC") 'help-at-point)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)

; my muse customisations
; TODO: Fix this path on work laptop...
;(load "~/.emacs.d/my-muse.el")

; eshell autocompletion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

; Multilingual Text Input mode
;(set-input-method 'latin-1-prefix)

;; Google Translate plugin
(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)

(setq google-translate-translation-directions-alist
      '(("es" . "en") ("en" . "es")))

; Thanks, Steve: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

; AUCTeX display of LaTeX compilation errors
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

(defun start-jrepl ()
  "Start a Java REPL"
  (interactive)
  (let ((jar "~/Documents/Tools/java-repl/build/artifacts/javarepl-dev.build.jar")
                                        ;(jar "/Users/astahlman/Documents/Programming/Tools/java-repl/build/artifacts/javarepl-dev.build.jar") ;; FIXME
        (buff (buffer-name)))          ; TODO: why is this nil?
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name "*jrepl*")))
    (shell (current-buffer))
    (process-send-string nil (concat "java -jar " jar "\n"))
    (goto-char (point-max))
    (insert (concat ":load " buff))))

(defun compile-java ()
  "Compile the current Java file"
  (interactive)
  (save-buffer)
  (with-output-to-temp-buffer "*javac*"
    (shell-command (concat "javac " (buffer-file-name))
                   "*javac*"
                   "*Messages*")
    (if (= 0 (buffer-size (get-buffer "*javac*")))
        (print "Compilation succeeded."))
    (pop-to-buffer "*javac*")))

; Turn on ya-snippet
(load "~/.emacs.d/snippet-functions.el")
(require 'yasnippet)
(yas-global-mode 1)

(defun fmt-epoch (&optional epoch)
  "Prints a human readable date given an epoch time in milliseconds"
  (interactive "P")
  (let* ((millis (or
                  (when current-prefix-arg
                    (string-to-number (read-from-minibuffer "Millis: ")))
                  epoch
                  (string-to-number (thing-at-point 'word))))
        (formatted (format-time-string "%Y-%m-%d %T UTC" (seconds-to-time (/ millis 1000)))))
    (message formatted)
    formatted))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;Temporary workaround - see https://github.com/technomancy/emacs-starter-kit/issues/99
(remove-hook 'ruby-mode-hook 'esk-run-coding-hook)

;; Saner regexp syntax, as recommended by
;; http://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;;; TO WRITE ;;;;
;; Write or find these functions online, as they would probably be useful
(defun fuzzy-find-buff (name)
  (interactive)
  "Returns a possibly empty list of buffers whose name partially matches NAME")

;; Commands to rearrange windows
(require 'buffer-move)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(defun whack-whitespace (arg)
  "Delete whitespace until the next word.
   Deletes across newlines if prefixed."
  (interactive "P")
  (let ((regex (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regex nil t)
    (replace-match "" nil nil)))

(global-set-key (kbd "C-x d") 'whack-whitespace)

;; Courtesy of: http://rejeep.github.io/emacs/elisp/2010/11/16/delete-file-and-buffer-in-emacs.html
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun replace-epoch (start end)
  "Replace any epoch timestamps (in milliseconds) with a human-readable date string"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward-regexp "[0-9]\\{13\\}")
      (replace-match (concat "\"" (fmt-epoch (string-to-number (match-string 0))) "\"") nil t))))

;; EasyPG for wiki encryption
(require 'epa-file)
(epa-file-enable)

(defun kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

(define-key isearch-mode-map [(control k)] 'kill-isearch-match)

(org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (R . t)
          (shell . t)
          (python . t)
          (perl . t)
          (ruby . t)
          (scala . t)
          (clojure . t)
          (java . t)
          (gnuplot . t)
          (calc . t)
          (sql . t)
          (latex . t)))

;; Turn on evil before we load anything else so that we can use
;; evil-define-key
(require 'evil)
(evil-mode 1)

;; start the server on init
(server-start)

;; Load all of our literate configuration
(dolist (f (directory-files "~/.emacs.d/literate" t "^[^\\.]+.org$"))
  (message "Loading customizations from %s..." f)
  (org-babel-load-file f))

;; Load everything that's machine-specific
(when (file-exists-p "~/.emacs.d/user_local.el")
  (load "~/.emacs.d/user_local.el"))

;; Load all our patches
(dolist (f (directory-files "~/.emacs.d/patches" t ".+\.el"))
  (message "Loading patches from %s..." f)
  (load f))
