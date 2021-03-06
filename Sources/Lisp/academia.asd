;;;; academia.asd

(asdf:defsystem #:academia
  :serial t
  :description "rescuing robots"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
               #:hrl
               #:cl-heap
               #:hexameter)
  :components ((:file "utils-package")
               (:file "utils")
               (:file "epsilon-policy")
               (:file "package")
               ;(:file "env")
               ;(:file "rescue-env")
               (:file "extract-q-function")
               ;(:file "prog")
               ;(:file "rescue-prog")
               ;(:file "features")
               ;(:file "rescue-features")
               (:file "didaskalos")))
