(in-package #:academia-env)


(defparameter *algorithm-names* (list
                                 'smdpq
                                 'hordq
                                 'gold-standard
                                 'hordq-a-0
                                 'hordq-a-1
                                 'hordq-a-2
                                 'hordq-a-3))

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
  (cargo nil)
  (damage 0))


;;; World Objects
;;; =============

(defvar *object-id-counter* 0)

(defstruct (world-object (:type list)
                         :named
                         (:conc-name "OBJECT-")
                         (:constructor make-world-object
                             (&key ((:id %id) (incf *object-id-counter*))
                                   location)))
  (%id (incf *object-id-counter*))
  (location))

(defmethod object-id ((self list))
  (object-%id self))


;;; Construction Material

(defstruct (rubble (:type list)
                   :named
                   (:include world-object)
                   (:constructor make-rubble (%id location))))


;;; Victims
;;; =======

(defstruct (victim (:type list)
                   :named
                   (:include world-object)
                   (:constructor make-victim (%id location health)))
  (health 50))


;;; Points
;;; ======

(defstruct (point (:constructor make-point (x y)))
  (x 0.0 :type number)
  (y 0.0 :type number))

(defun distance (point-1 point-2)
  (let* ((x1 (point-x point-1))
         (x2 (point-x point-2))
         (dx (- x1 x2))
         (y1 (point-y point-1))
         (y2 (point-y point-2))
         (dy (- y1 y2)))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;; Nodes
;;; =====

(defvar *default-node-cost* 1.0)
(defvar *node-id-counter* 0)

(defstruct (node (:constructor make-node (&optional (x 0.0) (y 0.0)
                                          &key (cost *default-node-cost*)
                                               (id (incf *node-id-counter*))))
                 (:include point))
  id
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
  (if *print-readably*
      (let ((*print-circle* t))
        (call-next-method))
      (print-unreadable-object (self stream :identity t :type t)
        (format stream "~A; edges to: ~{~A~^, ~}"
                (node-id self)
                (sort (mapcar (compose #'node-id #'edge-to) (node-edges self)) #'less)))))

(defmethod object-id ((node node))
  (node-id node))

(defun node-neighbors (node)
  (mapcar #'edge-to (node-edges node)))

;;; Edge
;;; ====

(defvar *default-edge-cost* 1.0)
(defvar *edge-id-counter* 0)

(defstruct (edge (:constructor %make-edge (id from to cost)))
  id
  (from (make-node 0.0 0.0) :type node)
  (to (make-node 0.0 0.0) :type node)
  (cost 0 :type number))

(defun make-edge (from to &key (cost (+ *default-edge-cost* (distance from to)))
                               (id (incf *edge-id-counter*)))
  (let ((result (%make-edge id from to cost)))
    (push result (node-edges from))
    result))

(defmethod print-object ((self edge) stream)
  (if *print-readably*
      (let ((*print-circle* t))
        (call-next-method))
      (print-unreadable-object (self stream :identity t :type t)
        (format stream "~A cost ~3F from ~A to ~A"
                (edge-id self)
		(edge-cost self)
                (node-id (edge-from self))
                (node-id (edge-to self))))))

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
      (destructuring-bind (from to &rest keys)
          edge-description
        (apply 'make-edge (find-node graph from) (find-node graph to) keys))))

(defun make-graph (nodes edges)
  (let ((graph (%make-graph)))
    (setf (graph-nodes graph)
          (mapcar 'node-description-to-node nodes))
    (setf (graph-edges graph)
          (mapcar (lambda (edge)
                    (edge-description-to-edge graph edge)) edges))
    graph))

#||
(defparameter *graph*
  (make-graph '((0.0 0.0 :id node-1) (0.0 1.0 :id node-2)
                (1.0 1.0 :id node-3) (2.0 1.0 :id node-4)
                (3.0 2.0 :id node-5) (3.0 3.0 :id node-6))
              '((node-1 node-2) (node-2 node-1)
                (node-2 node-3) (node-3 node-2)
                (node-3 node-4) (node-4 node-3)
                (node-2 node-4) (node-4 node-2)
                (node-3 node-5) (node-5 node-3)
                (node-3 node-6) (node-6 node-3))))
||#

;;; A* Search
;;; ---------

(defstruct (search-node (:conc-name "SN-")
                        (:constructor %make-search-node
                            (location parent edge cost-from-start cost-to-goal)))
  location
  parent
  edge
  cost-from-start
  cost-to-goal
  total-cost
  heap-index)

(defun make-search-node (location parent edge cost-from-start cost-to-goal)
  (let ((snode (%make-search-node location parent edge cost-from-start cost-to-goal)))
    (setf (sn-total-cost snode)
          (+ (sn-cost-from-start snode) (sn-cost-to-goal snode)))
    snode))

(defmethod print-object ((self search-node) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A" (node-id (sn-location self)))))

(defun build-path (search-node &optional (end-segment '()) (cost 0))
  "Return the path from the start node of the search to the goal node
represented by SEARCH-NODE.  Returns the cost of this path as second
value; the cost takes the node-costs of all nodes visited during the
path into account.  (This does not include the goal node since its
cost is encountered after the path is completed."
  (let ((edge (sn-edge search-node)))
    (if (null edge)
	(values end-segment cost)
	(build-path (sn-parent search-node)
		    (cons edge end-segment)
		    (+ cost (edge-cost edge) (node-cost (sn-location search-node)))))))

(defun a*-search (graph start-node goal-node-or-nodes
                  &optional (path-cost-estimate
			     (if (consp goal-node-or-nodes)
				 (lambda (start-node goal-nodes)
				   (apply #'min
					  (mapcar (curry #'distance start-node) goal-nodes)))
				 #'distance)))
  (declare (optimize debug (speed 0) (compilation-speed 0)))
  (setf start-node (find-node graph start-node))
  (cond ((atom goal-node-or-nodes)
	 (setf goal-node-or-nodes (find-node graph goal-node-or-nodes))
	 (check-type goal-node-or-nodes node))
	(t
	 (setf goal-node-or-nodes (mapcar (curry #'find-node graph) goal-node-or-nodes))
	 (check-type goal-node-or-nodes (or null (cons node)))))
  (let ((open (make-instance 'fibonacci-heap :key 'sn-total-cost))
	(closed '()))
    (labels ((push-open (snode)
	       (check-type snode search-node)
	       (multiple-value-bind (sn heap-index) (add-to-heap open snode)
		 (declare (ignore sn))
		 (setf (sn-heap-index snode) heap-index)))
	     (pop-open ()
	       (let ((result (pop-heap open)))
		 (check-type result search-node)
		 result))
	     (sn-member (snode nodes)
	       (check-type snode search-node)
	       (check-type nodes (or null (cons node)))
	       (member snode nodes :test (lambda (n g) (eql (sn-location n) g))))
	     (goalp (snode)
	       (if (consp goal-node-or-nodes)
		   (sn-member snode goal-node-or-nodes)
		   ;; goal-node is a single node
		   (progn
		     (check-type snode search-node)
		     (eql (sn-location snode) goal-node-or-nodes))))
	     (find-target-snode (edge)
	       (let ((node (edge-to edge)))
		 ;; Hack, since cl-heap does not expose a way to iterate over priority heaps
		 (cl-heap::do-each-node (heap-node (slot-value open 'cl-heap::root))
		   (let ((snode (cl-heap::node-item heap-node)))
		     (when (eql (sn-location snode) node)
		       (return-from find-target-snode (values snode :open)))))
		 (dolist (snode closed)
		   (when (eql (sn-location snode) node)
		     (return-from find-target-snode (values snode :closed))))
		 (values nil nil))))
      (push-open (make-search-node start-node nil nil 0
				   (funcall path-cost-estimate start-node goal-node-or-nodes)))
      (while (not (is-empty-heap-p open))
	(let ((snode (pop-open)))
	  (if (goalp snode)
	      (return-from a*-search snode)
	      (dolist (edge (node-edges (sn-location snode)))
		(let* ((new-cost-from-start (+ (sn-cost-from-start snode) (edge-cost edge)))
		       (new-cost-estimate
			 (funcall path-cost-estimate (edge-to edge) goal-node-or-nodes)))
		  (multiple-value-bind (new-snode open/closed) (find-target-snode edge)
		    (if new-snode
			(when (> (sn-cost-from-start new-snode) new-cost-from-start)
			  (let* ()
			    (setf (sn-parent new-snode) snode
				  (sn-edge new-snode) edge
				  (sn-cost-from-start snode) new-cost-from-start
				  (sn-cost-to-goal snode) new-cost-estimate
				  (sn-total-cost snode) (+ new-cost-from-start new-cost-estimate))
			    (ecase open/closed
			      (:open
			       (decrease-key open (sn-heap-index new-snode) new-cost-from-start))
			      (:closed
			       (setf closed (remove new-snode closed))))))
			(progn
			  (setf new-snode (make-search-node (edge-to edge)
							    snode
							    edge
							    new-cost-from-start
							    new-cost-estimate))
			  (push-open new-snode)))))))
	  (push snode closed)))
      nil)))

#||
(sn-total-cost (a*-search *graph* 'node-1 'node-5))
(sn-total-cost (a*-search *graph* 'node-1 '(node-4 node-5 node-5)))

(build-path (a*-search *graph* 'node-1 'node-5))
(build-path (a*-search *graph* 'node-1 '(node-4 node-5 node-5)))
||#


;;; Rescue State
;;; ============

(defstruct (rescue-state (:conc-name #:rs-))
  ;; The location of the current robot
  (location nil :type node)
  ;; A list of target locations
  (target-locations '() :type (or null (cons node)))
  ;; The cargo the robot is currently carrying
  (cargo nil)
  ;; The damage the robot has taken so far
  (damage 0 :type number)
  ;; Nodes in which other robots are located
  (robot-locations '())
  ;; Data of WORLD-OBJECTs that can be picked up
  (objects '()))


(defmethod clone ((state rescue-state))
  (make-rescue-state
   :location (rs-location state)
   :cargo (rs-cargo state)
   :damage (rs-damage state)
   :robot-locations (rs-robot-locations state)
   :objects (rs-objects state)))

(defmethod same ((lhs rescue-state) (rhs rescue-state))
  (or (eql lhs rhs)
      (every (lambda (f) (equalp (funcall f lhs) (funcall f rhs)))
             '(rs-location rs-cargo rs-damage rs-robot-locations
               rs-objects))))

(defmethod canonicalize ((state rescue-state))
  (list :location (rs-location state)
        :cargo (rs-cargo state)
        :damage (rs-damage state)
        :robot-locations (rs-robot-locations state)
        :objects (rs-objects state)))

;;; The variable *PRINT-GRAPHICALLY* is defined in env.lisp.

(defmethod print-object ((state rescue-state) stream)
  (if *print-readably*
      (let ((*print-circle* t))
        (call-next-method))
      (if *print-graphically*
          (let ((node (rs-location state)))
            (format stream "~&State:~12T~A, carrying ~:[nothing~;~:*~A~], damage ~A~%"
                    (node-id node)
                    (object-id (rs-cargo state))
                    (rs-damage state))
            (format stream "Neighbors:~12T~{~A~^, ~}~%"
                    (sort (mapcar #'node-id (node-neighbors node)) #'less))
            (format stream "Victims:~12T~{~A~^, ~}~%"
                    (mapcar (lambda (v) (list (object-id v)
                                              (object-id (object-location v))
                                              (victim-health v)))
                            (remove-if-not #'victim-p (rs-objects state))))
            (format stream "Rubble:~12T~{~A~^, ~}~%"
                    (mapcar (lambda (v) (list (object-id v)
                                              (object-id (object-location v))))
                            (remove-if-not #'rubble-p (rs-objects state)))))
          (print-unreadable-object (state stream :type t :identity t)
            (format stream "~A;~:[~; cargo: ~:*~A,~] dammage: ~A"
                    (node-id (rs-location state))
                    (rs-cargo state) (rs-damage state))))))

(defun rs-actions (state)
  (let* ((node (rs-location state))
         (go-actions (mapcar (lambda (node) `(go ,(node-id node)))
                             (mapcar #'edge-to (node-edges node))))
         (pickup-actions (mapcar (lambda (obj) `(pickup ,(object-id obj)))
                                 (node-objects state node)))
         (drop-action (if (rs-cargo state) '(drop) '()))
         (result (append '(nop) go-actions pickup-actions drop-action)))
    result))


;;; The Environment
;;; ===============

(defun node-objects (state node)
  (let ((objects (rs-objects state)))
    (remove-if (lambda (obj)
                 (not (eql (object-location obj) node)))
               objects)))

(defclass <rescue-env> (<fully-observable-env>)
  ((nav-graph :accessor nav-graph :initarg :nav-graph
              :initform (required-initarg :map))
   (home-node :accessor home-node :initarg :home-node
              :initform (required-initarg :home-node))
   (max-damage :accessor max-damage :initarg :max-damage
               :initform 500)
   (invalid-action-cost :accessor invalid-action-cost :initarg :invalid-action-cost
                        :initform 2.0)))


(defmethod initialize-instance :around ((self <rescue-env>) &key home-node nav-graph)
  (call-next-method self
                    :home-node (find-node nav-graph home-node)
                    :nav-graph nav-graph))

#||
(progn
  (defparameter *re-graph*
    (make-graph '((0.0 0.0 :id 1)
                  (0.0 1.0 :id 2)
                  (1.0 1.0 :id 3)
                  (1.0 0.0 :id 4))
                '((1 2) (1 4) (2 3) (4 1))))

  (defparameter *rs*
    (let ((node-1 (find-node *re-graph* 1))
          (node-2 (find-node *re-graph* 2))
          (node-3 (find-node *re-graph* 3)))
      (make-rescue-state :location node-1
                         :objects (list
                                   (make-victim 'v1 node-1 24)
                                   (make-victim 'v2 node-2 71)
                                   (make-rubble 'r1 node-1)))))

  (defparameter *re*
    (make-instance '<rescue-env>
      :nav-graph *re-graph*
      :home-node (find-node *re-graph* 3))))

#+ (or)
(sample-next *re* (sample-next *re* *rs* '(pickup m1)) '(go node-3))
||#

(defmethod avail-actions ((env <rescue-env>) (state rescue-state))
  (declare (ignorable env))
  (rs-actions state))

(defmethod is-terminal-state ((env <rescue-env>) (state rescue-state))
  (let ((max-damage (max-damage env)))
    (and max-damage (> (rs-damage state) max-damage))))

(defun rescue-reward (env state action new-state)
  (let* (; (graph (nav-graph env))
         (from (rs-location state))
         (to (rs-location new-state))
         (edge (find to (node-edges from) :key #'edge-to))
         (reward (if (and (same (rs-location new-state) (home-node env))
                          (eql action 'drop))
                     1000
                     0)))
    (+ reward (- (node-cost (rs-location state))
                 (if edge (edge-cost edge) (invalid-action-cost env))))))

(defmethod sample-next ((env <rescue-env>) (state rescue-state) action)
  (if (is-terminal-state env state)
      state
      (let ((new-state (clone state)))
        (incf (rs-damage new-state) (node-cost (rs-location state)))
        (cond ((eql action 'nop))
              ((eql action 'drop)
               (let ((cargo (rs-cargo new-state)))
                 (when cargo
		   (let ((new-obj (clone cargo)))
		     (setf (object-location new-obj) (rs-location new-state))
		     (push new-obj (rs-objects new-state))
		     (setf (rs-cargo new-state) nil)))))
              (t
               (destructuring-bind (action-type &rest parameters) action
                 (ecase action-type
                   ((go)
                    (let* ((new-location (find-node (nav-graph env) (first parameters)))
                           (edge (find new-location (node-edges (rs-location state))
                                       :key 'edge-to)))
                      (cond (edge
                             (setf (rs-location new-state) new-location)
                             (incf (rs-damage new-state) (edge-cost edge)))
                            (t
                             (incf (rs-damage new-state) (invalid-action-cost env))))))
                   ((pickup)
                    (let* ((item-id (first parameters))
			   (item (find item-id (rs-objects state) :key #'object-id)))
                      (unless (rs-cargo new-state)
                        (setf (rs-cargo new-state) item)
                        (setf (rs-objects new-state)
			      (remove item (rs-objects state))))))))))
        (values new-state (rescue-reward env state action new-state)))))

(defmethod sample-init ((env <rescue-env>))
  (let* ((graph (nav-graph env))
         ;; (node-1 (find-node graph 1))
         (node-2 (find-node graph 2))
         (node-3 (find-node graph 3)))
    (make-rescue-state
     :location (home-node env)
     :target-locations (list (home-node env))
     :objects (list (make-victim :v1 node-2 24)
                    (make-rubble :r1 node-3)))))

(defmethod env:io-interface :before ((env <rescue-env>))
  (format t "~&Welcome to the \"academia\" robotic rescue example.

This environment demonstrates a robot that moves around a navigation network, tries to pick-up
victims and deliver them to a drop-off zone.  You can move to a neighbor node by entering (GO
<NODE>); if the robot is on the same node as an object or a victim you can pick it up by
entering (PICKUP <OBJECT>), if you are in the drop-off zone you can drop the object (and collect
the reward) by entering DROP.  To quit the environment, enter NIL.  (All input can be in lower
or upper case.)~2%")
  (setf *print-graphically* t))

(defun make-rescue-env-0 ()
  (make-instance '<rescue-env>
    :nav-graph (make-graph '((0.0 0.0 :id 1)
                             (0.0 1.0 :id 2)
                             (1.0 1.0 :id 3)
                             (1.0 0.0 :id 4))
                           '((1 2) (2 1)
                             (1 4) (4 1)
                             (2 3) (3 2)))
    :home-node 1))

(defun load-rescue-env (file)
  (with-open-file (stream file :direction :input)
    (destructuring-bind (&key nav-graph home-node) (read stream)
      (make-instance '<rescue-env>
        :nav-graph (apply 'make-graph nav-graph)
        :home-node home-node))))

(defun make-rescue-env-1 ()
  (load-rescue-env "rescue-scenario-01.lisp"))
