(require :hexameter)
(require :academia)
(require :sb-posix)
(in-package :academia-prog)

(defun item (&rest args)
  (apply 'hexameter:hex-item args))

;; TODO: Standard symbols for initialize-algorithms from package academia-env do not match literals in academia-prog
;; TODO: hordq-a-<n> with n > 1 throw errors

(make-new-environment :medium) ;;(make-rescue-env-0)
(initialize-algorithms (list 'hordq-a-0 'hordq-a-1))
(learn-behavior)

(let ((me (or (cadr sb-ext:*posix-argv*) "localhost:55559"))
      (apocalypse nil)
      (knowledge-base (build-state-table))
      (knowledge-selection (make-hash-table :test 'equalp))
      (knowledge-lesson (item :class "commit goal"))
      (situation-count 0)
      (plan nil))

  (defun getgoal (choice-point)
    'navigate)
  
  (defun newx (oldx dir)
    (cond ((equalp dir 'n) oldx)
          ((equalp dir 'e) (+ oldx 1))
          ((equalp dir 's) oldx)
          ((equalp dir 'w) (- oldx 1))))
  
  (defun newy (oldy dir)
    (cond ((equalp dir 'n) (- oldy 1))
          ((equalp dir 'e) oldy)
          ((equalp dir 's) (+ oldy 1 ))
          ((equalp dir 'w) oldy)))


  (loop for consideration being the hash-keys of knowledge-base
        using (hash-value utility)
        do (let ((choice-point (svref consideration 0))
                 (action (svref consideration 1)))
             (if (equalp choice-point 'navigate-choice)
                 (let* ((start (svref consideration 2))
                        (target (svref consideration 3))
                        (situation (vector choice-point start target))
                        (feature-advice (item :class "commit feature"
                                              1 (item :on (item :x (second start) :y (first start) :targetx (second target) :targety (first target))
                                                      :do (item :class "motor"
                                                                :type "move"
                                                                :control (item :dir action))
                                                      :for utility))))
                   (format t "~A -> ~A : ~A [ ~A ]~%" start target action utility)
                   (setf plan
                         (append
                          (list
                           (item :class "anchor" :x (second start) :y (first start) :targetx (second target) :targety (first target) :appeal utility)
                           (item :class "anchor" :x (second start) :y (first start) :targetx (second target) :targety (first target) :appeal utility :meta "concept")
                           (item :class "motor" :type "move" :control (item :dir action))
                           (item :class "motor" :type "move" :control (item :dir action) :meta "concept")
                           (item :class "feature" :x (newx (second start) action) :y (newy (first start) action))
                           (item :class "feature" :x (newx (second start) action) :y (newy (first start) action) :meta "concept")
                           (item :class "release" :meta "concept")
                           (item :class "release"))
                          plan))
                   (when (> utility (gethash situation knowledge-selection -100000))
                     (setf (gethash situation knowledge-selection) utility)
                     (setf (gethash (getgoal choice-point) knowledge-lesson) (cons feature-advice (gethash (getgoal choice-point) knowledge-lesson)))))
                 (format t "-- ~A, ~A : ~A [ ~A ]~%" choice-point (svref consideration 2) action utility))))


  (gbbopen-tools:define-class logic-space (spondeios:hexameter-space)
    ())

  (defmethod spondeios:handle ((self logic-space) msgtype author space parameter
                               &optional recipient)
    (cond ((string= space "charon.halt")
           (progn
             (setf apocalypse t)
             (values nil nil)))
          ((string= space "solve")
           (values
            (list (item 
                   :answer (list (item 
                                  :type "move"
                                  :control (item :up 1))
                                 (when (and (first parameter) (< (gethash "period" (first parameter) 0) 4))
                                   (item
                                    :type "shout"
                                    :control (item :content knowledge-lesson))))))
            t))
          ((string= space "plan")
           (values
            (list (item :answer (list (item :type "shout" :control (item :content plan)))))
            t))
          ))

  (let ((context (hex:init :me me :space 'logic-space)))
    (format t "~&Entering infinite response loop, please abort manually.~%")
    (loop while (not apocalypse) do (hex:respond context 0))))

(format t "~&Halt.~%")