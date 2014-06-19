(let ((appdir (concatenate 'string (sb-posix:getcwd) "/../../")) (librootdir (concatenate 'string (sb-posix:getcwd) "/../../Sources/Lisp/")))
(asdf:initialize-source-registry
  `(:source-registry
     (:directory ,appdir)
     (:tree ,librootdir)
     :inherit-configuration))
(print appdir)
(print librootdir)
(print asdf:*central-registry*))

(require :hexameter)
(require :academia-rescue)
(require :sb-posix)
(in-package :academia-prog)

(defun item (&rest args)
  (apply 'hexameter:hex-item args))

;; TODO: Standard symbols for initialize-algorithms from package academia-env do not match literals in academia-prog
;; TODO: hordq-a-<n> with n > 1 throw errors

(let* ((rescenv (make-new-environment :rescue :medium)) ;;(make-rescue-env-0)
       (graph (nav-graph rescenv)))
  (initialize-algorithms (list 'hordq-a-0 'hordq-a-1))
  (learn-behavior)
  
  (let ((me (or (cadr sb-ext:*posix-argv*) "localhost:55559"))
        (apocalypse nil)
        (knowledge-base (build-state-table))
        (plan nil))

    (loop for consideration being the hash-keys of knowledge-base
          using (hash-value utility)
          do (let ((choice-point (svref consideration 0))
                   (action (svref consideration 1)))
               (if (equalp choice-point 'random-action-choice)
                   (let* ((position (svref consideration 2))
                          (at-target-p (if (svref consideration 3) "yes" "no"))
                          (cargo (svref consideration 4)))
                     (format t
                             "position: (~A,~A), at-target: ~A, cargo: ~A -> ~A [ ~A ]~%"
                             (point-x position)
                             (point-y position)
                             at-target-p
                             cargo
                             action
                             utility)
                     (setf plan
                           (append
                            (list
                             (item :class "anchor"
                                   :x (point-x position)
                                   :y (point-y position)
                                   :ontarget at-target-p
                                   :cargo cargo
                                   :appeal utility)
                             (if (listp action)
                                 (item :class "motor"
                                       :type (first action)
                                       :control (item :to (second action) :id (second action)))
                                 (item :class "motor"
                                       :type action))
                             (item :class "teach")
                             (item :class "release"))
                            plan))))))



    (gbbopen-tools:define-class logic-space (spondeios:hexameter-space)
      ())
    
    (defmethod spondeios:handle ((self logic-space) msgtype author space parameter
                                 &optional recipient)
      (format t "PROCESSING~%")
      (cond ((string= space "charon.halt")
             (progn
               (setf apocalypse t)
               (values nil nil)))
            ((string= space "plan")
             (format t "ANSWER~%")
             (values
              (list (item :answer (list (item :type "shout" :control (item :content plan)))))
              t))
            ))

    (let ((context (hex:init :me me :space 'logic-space)))
      (format t "~&Entering infinite response loop, please abort manually.~%")
      (loop while (not apocalypse) do (hex:respond context 0)))))

  (format t "~&Halt.~%")