(in-package #:academia-prog)

(def-feature location-feature (state action)
  (rs-location env-state))

(def-feature at-target-location (state action)
  (member (rs-location env-state)
          (rs-target-locations env-state)))

(def-feature cargo-feature (state action)
  (object-id (rs-cargo env-state)))

(def-feature damage-feature (state action)
  (rs-damage env-state))

(def-feature robot-locations-feature (state action)
  (rs-robot-locations env-state))

(def-feature objects-feature (state action)
  (rs-objects env-state))

(defparameter *rescue-featurizer-0*
  (make-3partq-featurizer
   ()
   (random-action-choice
    (:qr-depends choice location-feature at-target-location cargo-feature)
    (:qc-depends choice location-feature at-target-location cargo-feature)
    (:qe-depends choice location-feature at-target-location cargo-feature))))

(defparameter *rescue-bucket-function-0*
  (make-bucket-fun '(location-feature at-target-location cargo-feature)))

(defparameter *rescue-featurizers*
  `((hordq-a-0 . , *rescue-featurizer-0*)
    (hordq-a-1 . , *rescue-featurizer-0*)
    (hordq-a-2 . , *rescue-featurizer-0*)
    (hordq-a-3 . , *rescue-featurizer-0*)))

(defparameter *rescue-bucket-functions*
  `((hordq-a-0 . , *rescue-bucket-function-0*)
    (hordq-a-1 . , *rescue-bucket-function-0*)
    (hordq-a-2 . , *rescue-bucket-function-0*)
    (hordq-a-3 . , *rescue-bucket-function-0*)))
