(in-package #:academia-utils)

(defgeneric less (lhs rhs)
  (:method (lhs rhs)
    (less (format nil "~A" lhs) (format nil "~A" rhs)))
  (:method ((lhs number) (rhs number))
    (< lhs rhs))
  (:method ((lhs string) (rhs string))
    (string< lhs rhs))
  (:method ((lhs symbol) (rhs symbol))
    (string< (symbol-name lhs) (symbol-name rhs))))
