(in-package #:academia-prog)

(def-feature fuel-feature (state action)
  (ac-fuel env-state))

(def-feature fuel-critical-p (state action)
  (let ((env (ac-env env-state)))
    ;; We're bingo fuel...
    (if (<= (ac-fuel env-state) (fuel-amount-per-step env))
        t
        nil)))

(def-feature loc (state action)
  (ac-robot-loc env-state))

(def-feature have-waste? (state action)
  (eql (ac-waste-status env-state) :on-robot))

(def-feature waste-source-feature (state action)
  (ac-waste-source env-state))

(def-feature waste-target-feature (state action)
  (ac-waste-target env-state))

(def-feature robot-dest (state action)
  (stack-var-val 'loc t t))

(def-feature navigating-to-waste (state action)
  (cond ((stack-contains-frame 'pickup-waste)
         #+ (or)
         (assert (not (stack-contains-frame 'drop-waste)))
         t)
        (t nil)))

;;; Superfluous, since it currently is always the negation of NAVIGATING-TO-WASTE
(def-feature navigating-to-dropoff (state action)
  (stack-contains-frame 'drop-waste))

(def-feature target-loc (state action)
  (if (navigating-to-waste state action)
      (waste-source-feature state action)
      (waste-target-feature state action)))

(def-feature waste-dist (state action)
  (grid-world:shortest-path-dist (ac-env env-state)
                                 (ac-waste-source env-state)
                                 (ac-waste-target env-state)))

(def-feature act-dist (state action)
  (grid-world:shortest-path-dist (ac-env env-state)
                                 (ac-robot-loc env-state)
                                 (ac-waste-target env-state)))

(def-feature shortest-path-distance (state action)
  (let* ((robot-loc (ac-robot-loc env-state))
         (target-loc (stack-var-val 'loc t t)))
    (grid-world:shortest-path-dist (ac-env env-state)
                                   robot-loc
                                   target-loc)))

(defun direction-from-to (from to)
  (destructuring-bind (from-x from-y) from
    (destructuring-bind (to-x to-y) to
      (cond ((< from-x to-x) 's)
            ((> from-x to-x) 'n)
            ((< from-y to-y) 'e)
            ((> from-y to-y) 'w)
            (t 'rest)))))

(def-feature target-direction (state action)
  (direction-from-to (loc state action) (target-loc state action)))

(def-feature shortest-path-direction (state action)
  (let* ((robot-loc (ac-robot-loc env-state))
         (target-loc (stack-var-val 'loc t t))
         (target-path (grid-world:shortest-path (ac-env env-state)
                                               robot-loc
                                               target-loc)))
    (assert target-loc (target-loc) "No target location?")
    (cond ((equal robot-loc target-loc) 'rest)
          (t
           (assert (>= (length target-path) 2))
           (direction-from-to robot-loc (second target-path))))))

(defun make-bucket-fun (features)
  (lambda (state)
    (mapcar (lambda (fun)
              (funcall fun state nil))
            features)))

#+ (or)
(defparameter *waste-featurizer-0*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc target-direction choice)
    (:qc-depends loc target-direction choice)
    (:qe-depends loc target-loc choice))
   (navigate-to-waste
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-waste-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))))

(defparameter *waste-featurizer-0*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends choice loc target-loc)
    (:qc-depends choice loc target-loc)
    (:qe-depends choice loc target-loc))
   (navigate-to-waste
    (:qr-depends have-waste?)
    (:qc-depends have-waste?)
    (:qe-depends have-waste?))
   (navigate-to-dropoff
    (:qr-depends have-waste?)
    (:qc-depends have-waste?)
    (:qe-depends have-waste?))
   (choose-waste-removal-action
    (:qr-depends choice have-waste?)
    (:qc-depends choice have-waste?)
    (:qe-depends choice have-waste?))))

(defparameter *waste-bucket-function-0*
  (make-bucket-fun '(loc target-direction target-loc have-waste?)))

(defparameter *waste-featurizer-1*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends choice loc target-loc)
    (:qc-depends choice loc target-loc)
    (:qe-depends choice loc target-loc))
   (navigate-to-waste
    (:qr-depends have-waste?)
    (:qc-depends have-waste?)
    (:qe-depends have-waste?))
   (navigate-to-dropoff
    (:qr-depends have-waste?)
    (:qc-depends have-waste?)
    (:qe-depends have-waste?))
   (choose-waste-removal-action
    (:qr-depends choice have-waste?)
    (:qc-depends choice have-waste?)
    (:qe-depends choice have-waste?))))

(defparameter *waste-bucket-function-1*
  (make-bucket-fun '(loc target-loc have-waste?)))

(defparameter *waste-featurizer-2*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends choice loc navigating-to-waste
                 shortest-path-distance shortest-path-direction)
    (:qc-depends choice loc navigating-to-waste
                 shortest-path-distance shortest-path-direction)
    (:qe-depends choice loc navigating-to-waste
                 shortest-path-distance shortest-path-direction))
   (navigate-to-waste
    (:qr-depends choice)
    (:qc-depends choice)
    (:qe-depends choice))
   (navigate-to-dropoff
    (:qr-depends choice)
    (:qc-depends choice)
    (:qe-depends choice))
   (choose-waste-removal-action
    (:qr-depends choice have-waste?)
    (:qc-depends choice have-waste?)
    (:qe-depends choice have-waste?))))

(defparameter *waste-bucket-function-2*
  (make-bucket-fun '(loc navigating-to-waste shortest-path-distance
                     shortest-path-direction have-waste?)))

(defparameter *waste-featurizer-3*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends choice loc navigating-to-waste shortest-path-direction)
    (:qc-depends choice loc navigating-to-waste shortest-path-direction)
    (:qe-depends choice loc navigating-to-waste shortest-path-direction))
   (navigate-to-waste
    (:qr-depends choice)
    (:qc-depends choice)
    (:qe-depends choice))
   (navigate-to-dropoff
    (:qr-depends choice)
    (:qc-depends choice)
    (:qe-depends choice))
   (choose-waste-removal-action
    (:qr-depends choice have-waste?)
    (:qc-depends choice have-waste?)
    (:qe-depends choice have-waste?))))

(defparameter *waste-bucket-function-3*
  (make-bucket-fun '(loc navigating-to-waste 
                     shortest-path-direction have-waste?)))

(defparameter *waste-featurizers*
  `((hordq-a-0 . ,*waste-featurizer-0*)
    (hordq-a-1 . ,*waste-featurizer-1*)
    (hordq-a-2 . ,*waste-featurizer-2*)
    (hordq-a-3 . ,*waste-featurizer-3*)))

(defparameter *waste-bucket-functions*
  `((hordq-a-0 . ,*waste-bucket-function-0*)
    (hordq-a-1 . ,*waste-bucket-function-1*)
    (hordq-a-2 . ,*waste-bucket-function-2*)
    (hordq-a-3 . ,*waste-bucket-function-3*)))
