(require :hexameter)
(require :academia)
(in-package :academia-prog)

(defun item (&rest args)
  (apply 'hexameter:hex-item args))

;; TODO: Standard symbols for initialize-algorithms from package academia-env do not match literals in academia-prog
;; TODO: hordq-a-<n> with n > 1 throw errors

(make-new-environment :medium)
(initialize-algorithms (list 'hordq-a-0))
(learn-behavior)

(let ((knowledge-base (build-state-table))
      (knowledge-selection (make-hash-table))
      (knowledge-lesson (item :class "commit goal"))
      (situation-count 0))

  (defun getgoal (choice-point)
    'navigate)

  (loop for consideration being the hash-keys of knowledge-base
        using (hash-value utility)
        do (let ((choice-point (svref consideration 0))
                 (action (svref consideration 1)))
             (if (equalp choice-point 'navigate-choice)
                 (let* ((start (svref consideration 2))
                        (target (svref consideration 3))
                        (situation (vector choice-point start target))
                        (feature-advice (item :class "commit feature"
                                              1 (item :on (item :x (first start) :y (second start) :targetx (first target) :targety (second target))
                                                      :do (item :class "motor"
                                                                :type "move"
                                                                :control (item :dir action))
                                                      :for utility))))
                   (format t "~A -> ~A : ~A [ ~A ]~%" start target action utility)
                   (when (< utility (gethash situation knowledge-selection 100000))
                     (setf (gethash situation knowledge-selection) utility)
                     (setf (gethash (getgoal choice-point) knowledge-lesson) (cons feature-advice (gethash (getgoal choice-point) knowledge-lesson)))))
                 (format t "--~%"))))


  (gbbopen-tools:define-class logic-space (spondeios:hexameter-space)
    ())

  (defmethod spondeios:handle ((self logic-space) msgtype author space parameter
                               &optional recipient)
    (values
     (list (item 
            :answer (list (item 
                           :type "move"
                           :control (item :up 1))
                          (item
                           :type "shout"
                           :control (item :content knowledge-lesson)))))
     t))

  (let ((context (hex:init :me "localhost:55559" :space 'logic-space)))
    (format t "~&Entering infinite response loop, please abort manually.")
    (loop while t do (hex:respond context 0))))