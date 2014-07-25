(require :asdf)
(require :sb-posix)

(let ((appdir (concatenate 'string (sb-posix:getcwd) "/../../")) (librootdir (concatenate 'string (sb-posix:getcwd) "/../../Sources/Lisp/")))
    (asdf:initialize-source-registry
      `(:source-registry
         (:directory ,appdir)
         (:tree ,librootdir)
         :inherit-configuration))
    ;(print appdir)
    ;(print librootdir)
    ;(print asdf:*central-registry*)
)
(setf *load-verbose* nil
      *load-print* nil
      *compile-print*  nil
      *compile-verbose* nil
      asdf:*asdf-verbose* nil)

(require :hexameter)
(require :academia-rescue)
(in-package :academia-prog)

(defvar me (or (cadr sb-ext:*posix-argv*) "localhost:55559"))
(defvar model (or (caddr sb-ext:*posix-argv*) "./model-01.lisp"))

(defun item (&rest args)
  (apply 'hexameter:hex-item args))

;; TODO: Standard symbols for initialize-algorithms from package academia-env do not match literals in academia-prog
;; TODO: hordq-a-<n> with n > 1 throw errors

(defun print-recursive (value &optional (indent ""))
    (if (hash-table-p value)
        (progn
            (format t "~A<hash-table>~%" indent)
            (maphash
                (lambda (key value)
                    (format t "~A~A:~%" indent key)
                    (print-recursive value (concatenate 'string indent "  ")))
                value)
            (format t "~A</hash-table>~%" indent))
        (if (listp value)
            (progn
                (format t "~A<list>~%" indent)
                (map nil
                    (lambda (value)
                        (print-recursive value (concatenate 'string indent "  ")))
                    value)
                (format t "~A</list>~%" indent))
            (format t "~A~A~%" indent value))))

(let ((counter 0)
      (names (make-hash-table :test 'equal)))
    (defun reset-ids ()
        (setf counter 0))
    (defun new-id (x y)
        (setf counter (+ counter 1 ))
        (setf (gethash (list x y) names) counter)
        counter)
    (defun get-id (x y)
        (gethash (list x y) names))
    (defun get-id-of (reference)
        (get-id (gethash "x" reference) (gethash "y" reference))))

(defun process-model (model)
    ;(print-recursive model)
    (reset-ids)
    (let ((nodes (map 'list (lambda (node) `(,(gethash "x" node) ,(gethash "y" node) :id ,(new-id (gethash "x" node) (gethash "y" node)))) (gethash "nodes" model)))
        (edges (map 'list (lambda (edge) (progn `(,(get-id-of (gethash "from" edge)) ,(get-id-of (gethash "to" edge))))) (gethash "edges" model))))
        `(:nav-graph (,nodes ,edges) :home-node ,(get-id-of (car (gethash "homes" model))))))

(let* ((rescenv nil) ;;(make-rescue-env-0)
       (graph nil))
       
  (defun learn-stuff (model-generator)
      (setf rescenv (setup-experiment model-generator 'random-rescue 100 10 *rescue-featurizers* *rescue-bucket-functions*))
      (setf graph (nav-graph rescenv))
      (initialize-algorithms (list 'hordq-a-0 'hordq-a-1))
      (learn-behavior)

      (let ((knowledge-base (build-state-table))
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
      plan)))

(let ((apocalypse nil)
      (current-plan (learn-stuff (lambda () (academia-env::load-rescue-env model))))
      (current-model model))


    (gbbopen-tools:define-class logic-space (spondeios:hexameter-space)
      ())
    
    (defmethod spondeios:handle ((self logic-space) msgtype author space parameter
                                 &optional recipient)
      ;(format t "PROCESSING~%")
      (cond ((string= space "charon.halt")
             (progn
               (setf apocalypse t)
               (values nil nil)))
            ((string= space "didaskalos.model")
              (dolist (item parameter)
                (multiple-value-bind (model model-p) (gethash "model" item)
                (when model-p
                  (format t "**  received model update, trying to update and learn anew~%")
                  ;(format t "$$ ~A~%" current-model)
                  ;(format t "%% ~A~%" model)
                  (setf current-model (process-model model))
                  (setf current-plan (learn-stuff (lambda () (academia-env::make-rescue-env current-model))))
                  (format t "**  relearning done~%"))))
              (values (list (item :model t)) t))
            ((string= space "didaskalos.relearn")
              (format t "**  received order to restart learning process, trying to do so~%")
              (setf current-plan (learn-stuff (lambda () (academia-env::load-rescue-env current-model))))
              (format t "**  relearning done~%")
              (values (list (item :relearning t)) t))
            ((string= space "plan")
             ;(format t "ANSWER~%")
             (values
              (list (item :answer (list (item :type "shout" :control (item :content current-plan)))))
              t))
            ))

    (let ((context (hex:init :me me :space 'logic-space)))
      (format t "~&**  Entering infinite response loop, please abort manually.~%")
      (loop while (not apocalypse) do (hex:respond context 0))))

  (format t "~&**  Didaskalos halted~%")