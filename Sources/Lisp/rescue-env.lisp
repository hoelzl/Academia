(in-package #:academia-env)

;;; Graph Structure
;;; ===============

;;; To avooid forward references, GRAPH is defined here.  The functions are below.

(defstruct (graph (:constructor %make-graph))
  (nodes '())
  (edges '()))

(defgeneric object-id (object)
  (:method (self)
    self))


;;; Robot
;;; =====

(defvar *robot-id-counter* 0)

(defstruct robot
  (id (incf *robot-id-counter*))
  (carriage nil)
  (damage 0))


;;; World Objects
;;; =============

(defvar *object-id-counter* 0)

(defstruct (world-object (:conc-name "OBJECT-")
                         (:constructor make-world-object
                             (&key ((:id %id) (incf *object-id-counter*)))))
  (%id (incf *object-id-counter*)))

(defmethod object-id ((self world-object))
  (object-%id self))


;;; Construction Material

(defstruct (construction-material (:include world-object)))


;;; Victims
;;; =======

(defstruct (victim (:include world-object)))


;;; Nodes
;;; =====

(defvar *default-node-cost* 1.0)
(defvar *node-id-counter* 0)

(defstruct (node (:constructor make-node (x y &key (cost *default-node-cost*)
                                                   (id (incf *node-id-counter*)))))
  id
  (x 0.0 :type number)
  (y 0.0 :type number)
  (cost *default-node-cost* :type number)
  (edges '())
  (graph nil :type (or null graph)))

#+ (or)
(defun node-objects (node)
  (node-%objects node))

#+ (or)
(defun (setf node-objects) (new-value node)
  (let ((graph (node-graph node)))
    (when graph
      (let ((hash (graph-objects graph))
            (old-objects (node-%objects node)))
        (dolist (object old-objects)
          (remhash (object-id object) hash))
        (dolist (object new-value)
          (assert (object-id object) () "Object ~A has no ID." object)
          (setf (gethash (object-id object) hash) object)))))
  (setf (node-%objects node) new-value))

(defmethod print-object ((self node) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~A: Edges ~A"
            (node-id self)
            (mapcar #'edge-id (node-edges self)))))

(defun node-distance (node-1 node-2)
  (let* ((x1 (node-x node-1))
         (x2 (node-x node-2))
         (dx (- x1 x2))
         (y1 (node-y node-1))
         (y2 (node-y node-2))
         (dy (- y1 y2)))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;; Edge
;;; ====

(defvar *default-edge-cost* 1.0)
(defvar *edge-id-counter* 0)

(defstruct (edge (:constructor %make-edge (id from to cost)))
  id
  (from (make-node 0.0 0.0) :type node)
  (to (make-node 0.0 0.0) :type node)
  (cost *default-edge-cost* :type number))

(defun make-edge (from to &key (cost *default-edge-cost*)
                               (id (incf *edge-id-counter*)))
  (let ((result (%make-edge id from to cost)))
    (push result (node-edges from))
    result))

(defmethod print-object ((self edge) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~A: from ~A to ~A"
            (edge-id self)
            (node-id (edge-from self))
            (node-id (edge-to self)))))

#||
(progn
  (defparameter *node1* (make-node 0.0 0.0))
  (defparameter *node2* (make-node 0.0 1.0))
  (defparameter *node3* (make-node 1.0 1.0))

  (defparameter *edge12* (make-edge *node1* *node2*))
  (defparameter *edge21* (make-edge *node2* *node1*))
  (defparameter *edge13* (make-edge *node1* *node3*)))
||#

;;; Graph Functions
;;; ===============

(defun find-node (graph id)
  (when (node-p id)
    (if (member id (graph-nodes graph))
        (return-from find-node id)
        (error "Node ~A is not in ~A" id graph)))
  (dolist (node (graph-nodes graph))
    (when (eql (node-id node) id)
      (return-from find-node node)))
  (error "No node with ID ~A in ~A" id graph))

(defun node-description-to-node (node-description)
  (if (node-p node-description)
      node-description
      (apply #'make-node node-description)))

(defun edge-description-to-edge (graph edge-description)
  (if (edge-p edge-description)
      edge-description
      (destructuring-bind (from to &key (cost *default-edge-cost*)
                                        (id (incf *edge-id-counter*)))
          edge-description
        (make-edge (find-node graph from)
                   (find-node graph to)
                   :cost cost :id id))))

(defun make-graph (nodes edges)
  (let ((graph (%make-graph)))
    (setf (graph-nodes graph)
          (mapcar 'node-description-to-node nodes))
    (setf (graph-edges graph)
          (mapcar (lambda (edge)
                    (edge-description-to-edge graph edge)) edges))
    graph))

#||
(make-graph '((0.0 0.0 :id node-1) (0.0 1.0 :id node-2) (1.0 1.0 :id node-3))
            '((node-1 node-2) (node-2 node-3)))
||#

;;; Rescue State
;;; ============

(defstruct (rescue-state (:conc-name #:rs-))
  (location nil :type node)
  (carriage nil)
  (damage 0 :type number)
  ;; List of nodes in which robots are located
  ;; (robot-locations (make-hash-table))
  (victim-health (make-hash-table))
  (object-locations (make-hash-table)))


(defmethod clone ((state rescue-state))
  (make-rescue-state
   ;; :robot-locations (copy-hash-table (rs-robot-locations state))
   :location (rs-location state)
   :carriage (rs-carriage state)
   :damage (rs-damage state)
   :object-locations (copy-hash-table (rs-object-locations state))
   :victim-health (copy-hash-table (rs-victim-health state))))

(defmethod same ((lhs rescue-state) (rhs rescue-state))
  (or (eql lhs rhs)
      (every (lambda (f) (funcall f lhs rhs))
             '(rs-location rs-carriage rs-damage rs-object-locations
               rs-victim-health))))

(defmethod canonicalize ((state rescue-state))
  (list 'location (rs-location state)
        ;; 'robot-locations (hash-to-alist (rs-robot-locations state))
        'location (hash-to-alist (rs-location state))
        'damage (hash-to-alist (rs-damage state))
        'object-location (hash-to-alist (rs-object-locations state))
        'victim-health (hash-to-alist (rs-victim-health state))))

(defmethod print-object ((state rescue-state) stream)
  (print-unreadable-object (state stream :type t :identity t)
    (format stream "~A" (rs-location state))))

(defun node-objects (state node)
  (let ((object-hash (rs-object-locations state))
        (result '()))
    (flet ((find-result (obj obj-node)
             (when (eql node obj-node)
               (push obj result))))
      (maphash #'find-result object-hash))
    result))

(defclass <rescue-env> (<fully-observable-env>)
  ((nav-graph :accessor nav-graph :initarg :nav-graph
              :initform (required-initarg :map))
   (home-node :accessor home-node :initarg :home-node
              :initform (required-initarg :home-node))
   (max-damage :accessor max-damage :initarg :max-damage
               :initform nil)))


(defmethod initialize-instance :after ((self <rescue-env>) &key)
  (let ((home-node (home-node self)))
    (unless (node-p home-node)
      (setf (home-node self)
            (find-node (nav-graph self) home-node)))))

#||
(progn
  (defparameter *re-graph*
    (make-graph '((0.0 0.0 :id node-1) (0.0 1.0 :id node-2) (1.0 1.0 :id node-3))
                '((node-1 node-2) (node-2 node-3))))

  (defparameter *rs*
    (let ((node-1 (find-node *re-graph* 'node-1))
          (node-2 (find-node *re-graph* 'node-2))
          (node-3 (find-node *re-graph* 'node-3)))
      (make-rescue-state :location node-1
                         :object-locations (alist-to-hash
                                            `((v1 . ,node-1) (v2 . ,node-2) (m1 . ,node-1))))))

  (defparameter *re*
    (make-instance '<rescue-env> :nav-graph *re-graph* :home-node (find-node *re-graph* 'node-3))))
||#

(defmethod avail-actions ((env <rescue-env>) (state rescue-state))
  (let* ((node (rs-location state))
         (go-actions (mapcar (lambda (node) `(go ,(node-id node)))
                             (mapcar #'edge-to (node-edges node))))
         (pickup-actions (mapcar (lambda (obj) `(pickup ,(object-id obj)))
                                 (node-objects state node))))
    (append '(nop) go-actions pickup-actions)))

(defmethod is-terminal-state ((env <rescue-env>) (state rescue-state))
  (let ((max-damage (max-damage env)))
    (and max-damage (> (rs-damage state) max-damage))))

(defmethod sample-next ((env <rescue-env>) (state rescue-state) action)
  (if (is-terminal-state env state)
      state
      (let ((new-state (clone state)))
        (incf (rs-damage new-state) (node-cost (rs-location state)))
        (cond ((eql action 'nop))
              ((eql action 'drop)
               (let ((carriage (rs-carriage new-state)))
                 (assert carriage ()  "No carriage?")
                 (when carriage
                   (setf (gethash carriage (rs-object-locations new-state))
                         (rs-location new-state))
                   (setf (rs-carriage new-state) nil))))
              (t
               (destructuring-bind (action-type &rest parameters) action
                 (ecase action-type
                   ((go)
                    ;; TODO: Check for errors
                    (let ((new-location (find-node (nav-graph env) (first parameters))))
                      (setf (rs-location new-state) new-location)))
                   ((pickup)
                    (let ((item (first parameters))
                          (object-hash (rs-object-locations new-state)))
                      (unless (rs-carriage new-state)
                        (setf (rs-carriage new-state) item)
                        (remhash item object-hash))))))))
        new-state)))

(defmethod sample-init ((env <rescue-env>))
  (let* ((graph (nav-graph env))
         (node-1 (find-node graph 'node-1))
         (node-2 (find-node graph 'node-2))
         (node-3 (find-node graph 'node-3)))
    (make-rescue-state
     :location node-1
     :victim-health (alist-to-hash '((v1 . 24)))
     :object-locations (alist-to-hash `((v1 . ,node-2) (obj-1 . ,node-3))))))

(defmethod io-interface :before ((env <rescue-env>))
  (format t "~&Welcome to the \"academia\" robotic rescue example.

This environment demonstrates a robot that moves around a navigation network, tries to pick-up
victims and deliver them to a drop-off zone.  You can move to a neighbor node by entering (GO
<NODE>); if the robot is on the same node as an object or a victim you can pick it up by
entering (PICKUP <OBJECT>), if you are in the drop-off zone you can drop the object (and collect
the reward) by entering DROP.  To quit the environment, enter NIL.  (All input can be in lower
or upper case.)"))

(defun make-rescue-env-0 ()
  (make-instance '<rescue-env>
    :nav-graph (make-graph '((0.0 0.0 :id node-1)
                             (0.0 1.0 :id node-2)
                             (1.0 1.0 :id node-3))
                           '((node-1 node-2)
                             (node-2 node-3)))
    :home-node 'node-1))
