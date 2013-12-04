(in-package #:academia-env)

(defparameter *algorithm-names* (list 
                                 ;; 'hordq
                                 ;; 'gold-standard
                                 'hordq-a-0
                                 'hordq-a-1
                                 'hordq-a-2
                                 'hordq-a-3
                                 ))

(defvar *current-exploration-strategy*)
(defvar *step-number-multiplier* 1)

(defun algorithm-index (algorithm)
  (let ((index (position algorithm *algorithm-names*)))
    (assert index (index) "Algorithm ~A not defined." algorithm)
    index))

(defvar *algorithms* (make-array (list (length *algorithm-names*))
                                 :adjustable t :fill-pointer 0))

(defstruct (algorithm-description (:conc-name ad-))
  algorithm
  (bucket-function #'canonicalize)
  (test #'equal))

(defun algorithm-descriptions ()
  *algorithms*)

(defun algorithm-description-for (name)
  (aref *algorithms* (algorithm-index name)))

(defun algorithm-for (name)
  (ad-algorithm (algorithm-description-for name)))

(defun algorithms ()
  (map-array 'ad-algorithm *algorithms*))

(defun current-algorithm ()
  (aref (algorithms) 0))


;;; A grid environment in which a robot should move from a source field to the location of a
;;; waste item, collect the waste and then drop it in a target area.  Modeled loosely after the
;;; td-taxi world.

(defstruct (academia-state (:conc-name #:ac-))
  robot-loc
  (waste-status :at-source) ; :on-robot :at-source :at-dest
  waste-source
  waste-target
  fuel
  env)

(defmethod clone ((state academia-state))
  (make-academia-state
   :robot-loc (ac-robot-loc state)
   :waste-status (ac-waste-status state)
   :waste-source (ac-waste-source state)
   :waste-target (ac-waste-target state)
   :fuel (ac-fuel state)
   :env (ac-env state)))

(defmethod same ((s1 academia-state) (s2 academia-state))
  (and (equal (ac-robot-loc s1) (ac-robot-loc s2))
       (eql (ac-waste-status s1) (ac-waste-status s2))
       (equal (ac-waste-source s1) (ac-waste-source s2))
       (equal (ac-waste-target s1) (ac-waste-target s2))
       (= (ac-fuel s1) (ac-fuel s2))
       (eql (ac-env s1) (ac-env s2))))

(defmethod canonicalize ((state academia-state))
  (list 'robot-loc (ac-robot-loc state)
        'waste-status (ac-waste-status state)
        'waste-source (ac-waste-source state)
        'waste-target (ac-waste-target state)
        'fuel (ac-fuel state)))

(defvar *print-graphically* nil)
(defmethod print-object ((state academia-state) stream)
  (if *print-graphically*
      (loop
        with env = (ac-env state)
        with d = (dimensions env)
        for i from -1 to (first d)
        do (terpri stream)
        do (loop
             for j from -1 to (second d)
             do (cond ((or (= i -1) (= i (first d)))
                       (if (or (= j -1) (= j (second d)))
                           (format stream "XX")
                           (format stream "~AX" (mod j 10))))
                      ((or (= j -1) (= j (second d)))
                       (format stream "~AX" (mod i 10)))
                      ((eq (loc-value env (list i j)) 'wall) 
                       (format stream "XX"))
                      ((equal (ac-robot-loc state) (list i j))
                       (if (eq (ac-waste-status state) :on-robot)
                           (format stream "RR")
                           (format stream "rr")))
                      ((equal (ac-waste-source state) (list i j))
                       (if (eq (ac-waste-status state) :at-source)
                           (format stream "WW")
                           (format stream "ww")))
                      (t (format stream "  "))))
        finally (format stream "~&Waste Targets: ~A  Fuel: ~A"
                        (waste-targets env)  (ac-fuel state)))
      (call-next-method)))


(defclass <academia-env> (<fully-observable-env>)
  ())

(defvar *available-actions* '(n e s w pickup drop refuel))

(defmethod avail-actions ((env <academia-env>) state)
  (declare (ignore state))
  ;; All actions are possible in every state.
  *available-actions*)

(defmethod is-terminal-state ((env <academia-env>) state)
  "is-terminal-state WASTE-ENV STATE
A state is terminal if we have unloaded the waste at one of the waste targets."
  (eq (ac-waste-status state) :at-dest))

(defclass <hades-env> (<academia-env>)
  ())

(defmethod sample-next ((env <hades-env>) state action)
  (assert (member action (avail-actions env state)) (action)
          "Action ~A is not possible in state ~A." action state)
  ;; ...
  )

(defmethod sample-init ((env <hades-env>))
  ;; ...
)


(defclass <waste-env> (<academia-env> <grid-world>)
  ((move-success-prob :type float
                      :initarg :move-success-prob :initform 1.0 ;0.95
                      :accessor move-success-prob)
   (wall-collision-cost :type float
                        :initarg :wall-collision-cost :initform 0.5
                        :accessor wall-collision-cost)
   (cost-of-living :type float
                   :initarg :cost-of-living :initform 0.1
                   :accessor cost-of-living)
   (init-fuel :type number
              :initarg :init-fuel :initform 100.0
              :accessor init-fuel)
   (fuel-decrease-prob :type float
                       :initarg :fuel-decrease-prob :initform 0.8
                       :accessor fuel-decrease-prob)
   (fuel-amount-per-step :type float
                         :initarg :fuel-amount-per-step :initform 1.0
                         :accessor fuel-amount-per-step)
   (no-fuel-cost :type float
                 :initarg :no-fuel-cost :initform 100.0
                 :accessor no-fuel-cost)
   (refuel-success-prob :type float
                        :initarg :refuel-success-prob :initform 0.95
                        :accessor refuel-success-prob)
   (waste-sources :initarg :waste-sources
                  :accessor waste-sources)
   (waste-targets :type list
                  :initarg :waste-targets :initform '((0 0))
                  :accessor waste-targets)
   (waste-delivery-reward :type float
                          :initarg :waste-delivery-reward :initform 50.0
                          :accessor waste-delivery-reward))
  (:default-initargs :legality-test (lambda (val)
                                      (not (eq val 'wall)))))

(defmethod initialize-instance ((env <waste-env>)
                                &rest initargs &key waste-sources world-map)
  (declare (ignorable initargs))
  ;; TODO: We have to try repeatedly to find an initial waste source because we don't take into
  ;; account inaccessible grid fields here.  Maybe we should just generate a list of valid
  ;; fields and sample from this list?
  (unless waste-sources
    (let ((sources (make-instance '<prod-set>
                     :sets (array-dimensions world-map)
                     :alist-keys '(0 1))))
      (setf (waste-sources env) sources)))
  (call-next-method))

(defun move-would-hit-wall-p (env state action)
  (destructuring-bind (robot-x robot-y) (ac-robot-loc state)
    (case action
      ((n) (= robot-x 0))
      ((w) (= robot-y 0))
      ((s) (destructuring-bind (dim-x dim-y) (dimensions env)
             (declare (ignore dim-y))
             (= (1+ robot-x) dim-x)))
      ((e) (destructuring-bind (dim-x dim-y) (dimensions env)
             (declare (ignore dim-x))
             (= (1+ robot-y) dim-y))))))

(defun reward (env state action new-state)
  (let* ((waste-at-dest? (eq (ac-waste-status new-state) :at-dest))
         (waste-reward (if waste-at-dest?
                           (waste-delivery-reward env)
                           0.0))
         (hit-wall? (move-would-hit-wall-p env state action))
         (wall-cost (if hit-wall? (wall-collision-cost env) 0.0))
         (no-fuel? (zerop (ac-fuel state)))
         (no-fuel-cost (if no-fuel? (no-fuel-cost env) 0.0)))
    (if (is-terminal-state env state)
        0
        (- waste-reward
           (cost-of-living env)
           wall-cost
           no-fuel-cost))))

(defun move-action-p (action)
  (case action
    ((n e s w) t)
    (otherwise nil)))

(defun compute-next-loc (env state action)
  (if (and (move-action-p action)
           (> (ac-fuel state) 0))
      (let* ((loc (ac-robot-loc state))
             (succ-prob (move-success-prob env))
             (slip-prob (/ (- 1.0 succ-prob) 2.0))
             (forward-loc (result loc action))
             (forward-prob (if (is-legal-loc env forward-loc) succ-prob 0.0))
             (left-loc (result loc (rot-counterclockwise action)))
             (left-prob (if (is-legal-loc env left-loc) slip-prob 0.0))
             (right-loc (result loc (rot-clockwise action)))
             (right-prob (if (is-legal-loc env right-loc) slip-prob 0.0))
             (stay-prob (- 1.0 (+ forward-prob left-prob right-prob))))
        (sample-multinomial (list forward-loc right-loc left-loc loc)
                            forward-prob right-prob left-prob stay-prob))
      (ac-robot-loc state)))

(defun compute-waste-status (env state action)
  (let ((waste-status (ac-waste-status state)))
    (case action
      ((pickup)
       (if (equal (ac-robot-loc state) (ac-waste-source state))
           :on-robot
           waste-status))
      ((drop)
       (if (and (eq (ac-waste-status state) :on-robot)
                (member (ac-robot-loc state) (waste-targets env) :test 'equal))
           :at-dest
           waste-status))
      (otherwise
       waste-status))))

(defun compute-fuel (env state action)
  (cond ((move-action-p action)
         (let ((fuel (ac-fuel state))
               (fuel-prob (fuel-decrease-prob env)))
           (sample-multinomial (list (max 0.0 (- fuel (fuel-amount-per-step env))) fuel)
                               fuel-prob (- 1.0 fuel-prob))))
        ((eq action 'refuel)
         (let ((refuel-prob (refuel-success-prob env)))
           (sample-multinomial (list (ac-fuel state) (init-fuel env))
                               (- 1.0 refuel-prob) refuel-prob)))
        (t (ac-fuel state))))

(defmethod sample-next ((env <waste-env>) state action)
  (assert (member action (avail-actions env state)) (action)
          "Action ~A is not possible in state ~A." action state)
  (let* ((next-loc (compute-next-loc env state action))
         (waste-status (compute-waste-status env state action))
         (fuel (compute-fuel env state action))
         (new-state (make-academia-state
                     :robot-loc next-loc
                     :waste-status waste-status
                     :waste-source (ac-waste-source state)
                     :waste-target (ac-waste-target state)
                     :fuel fuel
                     :env (ac-env state))))
    (values new-state (reward env state action new-state))))

(defvar *max-initial-waste-source-tries* 100)

(defun compute-intial-waste-source (env)
  (loop for i from 0 to *max-initial-waste-source-tries*
        do (let ((loc (mapcar #'cdr (sample-uniformly (waste-sources env)))))
             (when (is-legal-loc env loc)
               (return-from compute-intial-waste-source loc))))
  (error "Could not find an initial position in ~A." env))

(defmethod sample-init ((env <waste-env>))
  (make-academia-state
   :robot-loc (funcall (unif-grid-dist-sampler env))
   :waste-status :at-source
   :waste-source (compute-intial-waste-source env)
   :waste-target (first (waste-targets env))
   :fuel (init-fuel env)
   :env env))

(defun set-up-exploration ()
  (format t "~&Welcome to the \"academia\" robotic waste collection example.

This environment demonstrates a robot that moves around on a rectangular grid, picks up waste
and delivers it to a drop-off zone.  X's on the map represent walls, blank spaces are roads.
The robot is represented by 'r' as long as it does not carry waste, by 'R' as soon as it has
picked up waste.  You can move by entering N, E, S, W; if the robot is on the same grid field as
the waste you can pick it up by entering PICKUP, if you are in a drop-off zone you can drop the
waste and thereby end the episode (and collect the reward) by entering DROP.  To quit the
environment, enter NIL.  (All input can be in lower or upper case.)")
  (setf *print-graphically* t))

(defmethod io-interface :before ((env <waste-env>))
  (set-up-exploration))

(defun make-waste-env-0 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-1 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (setf (aref world 0 2) 'wall
          (aref world 1 2) 'wall)
    (apply #'make-instance '<waste-env>
           :world-map world 
           initargs)))

(defun make-waste-env-2 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(8 8) :initial-element 'road)))
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-3 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(8 8) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-4 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(20 20) :initial-element 'road)))
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-5 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(20 20) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))

(defun make-waste-env-6 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(10 10) :initial-element 'road))
        (walls '((0 3)
                 (1 3) (1 4) (1 5) (1 6) (1 7) (1 9)
                 (2 0) (2 1) (2 3) (2 5)
                 (3 3) (3 5) (3 6) (3 7) (3 8)
                 (4 1) (4 2) (4 3) (4 5)
                 (5 5) (5 7) (5 8) (5 9)
                 (6 0) (6 1) (6 2) (6 3) (6 5)
                 (7 1) (7 5) (7 6)
                 (8 1) (8 3) (8 5) (8 7)
                 (9 3))))
    (mapc (lambda (wall)
            (setf (aref world (first wall) (second wall)) 'wall))
          walls)
    (apply #'make-instance '<waste-env>
           :world-map world
           initargs)))
