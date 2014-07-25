;;;; academia-rescue.asd

(asdf:defsystem #:academia-rescue
  :serial t
  :description "Rescue force robots"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>, Thomas Gabor <thomas@denkfrei.de>"
  :license "MIT, see file LICENSE"
  :depends-on (#:academia)
  :components ((:file "env")
               (:file "prog")
               (:file "features")))
