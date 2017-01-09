;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
		     '(("*" "*${1:text}*$0" "star" nil nil nil "/Users/andrewstahlman/.emacs.d/snippets/org-mode/star" nil nil)
		       ("src" "\n#+BEGIN_SRC $1\n    $0\n#+END_SRC\n" "start a src block, e.g. #+BEGIN_SRC" nil nil nil "/Users/andrewstahlman/.emacs.d/snippets/org-mode/src" nil nil)
		       ("quote" "\n#+BEGIN_QUOTE\n    $0\n#+END_QUOTE\n" "start a quote block, e.g. #+BEGIN_QUOTE" nil nil nil "/Users/andrewstahlman/.emacs.d/snippets/org-mode/quote" nil nil)
		       ("lhspg" "\n#+header: :engine postgresql\n#+header: :dbhost analyticsdb-staging.lyft.net\n#+header: :dbuser lyftmaster\n#+header: :database lyfthousestaging\n#+header: :cmdline -p 5439 --no-align -F'|' -q\n#+BEGIN_SRC sql\n  $0\n#+END_SRC\n" "SQL query against lyfthouse staging as lyftmaster" nil nil nil "/Users/andrewstahlman/.emacs.d/snippets/org-mode/lhspg_local" nil nil)
		       ("example" "\n#+BEGIN_EXAMPLE\n    $0\n#+END_EXAMPLE\n" "start an example block, e.g. #+BEGIN_EXAMPLE" nil nil nil "/Users/andrewstahlman/.emacs.d/snippets/org-mode/example" nil nil)))


;;; Do not edit! File generated at Thu Jan  5 12:46:32 2017
