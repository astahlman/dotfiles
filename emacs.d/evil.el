;; Courtesy of
;; http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/

(require 'evil)
;; This is too annoying in Paredit
;; TODO: Figure out a way to disable evil and revert the cursor anytime Paredit is active
;;(evil-mode 1)

;; Change the cursor color depending on what mode we're in
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; Set leader to comma
(global-evil-leader-mode)
(evil-leader/set-leader ",")
;; Make leader key work in all modes
(setq evil-leader/in-all-states 1)

;; C-u scroll up - feels a little weird not having this be numeric-prefix...
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
