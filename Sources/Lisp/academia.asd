;;;; academia.asd

(asdf:defsystem #:academia
  :serial t
  :description "Waste-collecting robots"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:hrl #:hexameter)
  :components ((:file "utils-package")
               (:file "epsilon-policy")
               (:file "package")
               (:file "env")
               (:file "extract-q-function")
               (:file "prog")
               (:file "features")
               (:file "example")))
