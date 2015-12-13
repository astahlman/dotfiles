;; TODO: Bootstrap this...
(require 'epl)

;; This is where your customizations should live
(setq my-packages
  '(math-symbol-lists ac-math muse
                       paredit async pkg-info auctex
                       popup auto-complete projectile
                       cider queue cl-lib rainbow-delimiters
                       clojure-mode clojure-test-mode cljdoc s
                       dash elisp-slime-nav smex epl
                       starter-kit ess
                       starter-kit-eshell exec-path-from-shell
                       starter-kit-bindings find-file-in-project
                       starter-kit-lisp
                       google-translate
                       helm markdown-mode
                       helm-ag inf-ruby
                       highlight-indentation idle-highlight-mode
                       ido-ubiquitous w3m latex-preview-pane
                       yasnippet
                       ;magit ; This causes problems on Emacs < 24,
                       solarized-theme
                       buffer-move
                       org
                       org-bullets
                       org-beautify-theme
                       org-mime
                       smart-mode-line
                       htmlize
                       json-mode
                       yaml-mode
                       ox-mediawiki
                       erc-terminal-notifier))

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
;(load "~/.emacs.d/my-muse.el")

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

;; Turn on Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Saner regexp syntax, as recommended by
;; http://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; TODO: Move this to some Clojure specific file
(add-hook 'clojure-mode-hook
          (lambda () (local-set-key (kbd "C-h SPC") 'cider-doc-at-point)))

(defun cider-doc-at-point ()
  "Send the symbol at point to the cider repl
   e.g., (doc $thing-at-point). We need to
   (use 'clojure.repl) before this will work"
  (interactive)
  (let* ((thing (thing-at-point 'symbol))
         (doc-fn (concat "(doc " thing ")")))
    (save-excursion
      (other-window 1)
      (end-of-buffer)
      (insert doc-fn)
      (cider-repl-return))))

;; Enable rainbow-delimiters in all programming-related modes
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

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

;; Load the following languages in Babel
(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (R . t)
        (shell . t)
        (python . t)
        (ruby . t)
        (clojure . t)
        (latex . t)))

(require 'ob-clojure)
(setq org-src-fontify-natively t)
(setq org-babel-clojure-backend 'cider)

; In case of this message: Invalid function: org-babel-header-args-safe-fn
; Byte recompile ob-R.el as described at: http://irreal.org/blog/?p=4295

; The Cider API changed recently, so this commit needs to be applied
; locally in emacs.d/elpa/org-$version/ob-clojure.el until it gets pushed to ELPA:
; http://orgmode.org/w/org-mode.git?p=org-mode.git;a=commitdiff;h=4eccd7c7b564874e0e13513e06161e657832ef49

; In case of this message: Invalid function: org-with-silent-modifications
; Re-install org from ELPA *before* any org-functions have been called
; http://tonyballantyne.com/tech/elpa-org-mode-and-invalid-function-org-with-silent-modifications/

(setq org-confirm-babel-evaluate nil)

(defun kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

(define-key isearch-mode-map [(control k)] 'kill-isearch-match)

(defun send-to-cider ()
  "If region is active, send it to the other window (presumably Cider).
   If no region is active, send the current line to the other window."
  (interactive)
  (let ((content (if (use-region-p)
                      (buffer-substring (mark) (point))
                    (thing-at-point 'sexp))))
    (progn
      (other-window 1)
      (end-of-buffer)
      (insert content))))

(global-set-key (kbd "C-c C-s") 'send-to-cider)

;; TODO: Come up with some sort of directory structure and autoload
;; things like this
(load "~/.emacs.d/org-extensions.el")

;; Load everything that's machine-specific
(load "~/.emacs.d/user_local.el")

