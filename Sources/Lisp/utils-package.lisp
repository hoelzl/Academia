(in-package #:common-lisp-user)

(defpackage #:academia-utils
  (:use #:common-lisp
        ;; From ALisp
        #:utils
        #:exploration-policy)
  (:export #:<epsilon-policy>
           #:less))

