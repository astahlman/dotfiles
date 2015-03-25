;; This is where your customizations should live
(setq my-packages
  '( math-symbol-lists ac-math muse
                       paredit async pkg-info auctex
                       popup auto-complete projectile
                       cider queue cl-lib rainbow-delimiters
                       clojure-mode clojure-test-mode s
                       dash elisp-slime-nav smex epl
                       starter-kit ess
                       starter-kit-eshell exec-path-from-shell
                       starter-kit-bindings find-file-in-project
                       starter-kit-lisp
                       google-translate
                       helm markdown-mode
                       helm inf-ruby
                       highlight-indentation idle-highlight-mode
                       ido-ubiquitous w3m latex-preview-pane
                       yasnippet magit solarized-theme))

;; Activate all the packages
(package-initialize)

;; Fetch list of packages available
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; Install anything that's missing
(dolist (package my-packages)
  (unless (package-installed-p package)
    (message "Installing %s..." package)
    (package-install package)
    (message "Done.")))

;; https://github.com/purcell/exec-path-from-shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

(load "~/.emacs.d/vendor/clojure")

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))


;; Load ESS for R
;ess-mode configuration
(setq ess-ask-for-ess-directory nil) 
(setq inferior-R-program-name "/usr/local/bin/R") 
(setq ess-local-process-name "R") 
(setq ansi-color-for-comint-mode 'filter) 
(setq comint-scroll-to-bottom-on-input t) 
(setq comint-scroll-to-bottom-on-output t) 
(setq comint-move-point-for-output t)
(setq ess-eval-visibly-p nil)
(require 'ess-site)

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
(load "~/.emacs.d/my-muse.el")

; eshell autocompletion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

; CIDER stuff
(setq nrepl-log-messages t)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

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
  (let ((jar "/Users/astahlman/Documents/Programming/Tools/java-repl/build/artifacts/javarepl-dev.build.jar")
        buff (buffer-name)) ; TODO: why is this nil?
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
  (let ((millis (or
                 (when current-prefix-arg (string-to-number (read-from-minibuffer "Millis: ")))
                 epoch
                 (string-to-number (thing-at-point 'word)))))
    (message (format-time-string "%Y-%m-%d %T UTC" (seconds-to-time (/ millis 1000))))))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;Temporary workaround - see https://github.com/technomancy/emacs-starter-kit/issues/99
(remove-hook 'ruby-mode-hook 'esk-run-coding-hook)
