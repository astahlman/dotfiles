;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("*" "*${1:text}*$0" "star" nil nil nil "/home/astahlman/.emacs.d/snippets/org-mode/star" nil nil)
                       ("src" "\n#+BEGIN_SRC $1\n    $0\n#+END_SRC\n" "start a src block, e.g. #+BEGIN_SRC" nil nil nil "/home/astahlman/.emacs.d/snippets/org-mode/src" nil nil)
                       ("quote" "\n#+BEGIN_QUOTE\n    $0\n#+END_QUOTE\n" "start a quote block, e.g. #+BEGIN_QUOTE" nil nil nil "/home/astahlman/.emacs.d/snippets/org-mode/quote" nil nil)
                       ("example" "\n#+BEGIN_EXAMPLE\n    $0\n#+END_EXAMPLE\n" "start an example block, e.g. #+BEGIN_EXAMPLE" nil nil nil "/home/astahlman/.emacs.d/snippets/org-mode/example" nil nil)))


;;; Do not edit! File generated at Mon Nov 21 02:19:26 2016
