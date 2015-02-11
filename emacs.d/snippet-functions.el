(defun matching-brace (opener)
  "Return the closing brace character matching opener e.g. '{' -> '}'"
  (let ((pairs '(
                 (")" . "(")
                 ("\\}" . "\\{")
                 ("]" . "["))))
    (car (rassoc opener pairs))))
