(in-package #:academia-prog)

;;deprecated, write these vars out here
(defvar *experiment-kind* :rescue) ;; :waste or :rescue
(defvar *environment-type* :small)
(defvar *use-complex-environment* nil)

(defvar *environment* nil)
(defvar *environment-generator* (lambda () nil))
(defvar *episode-count* 100)
(defvar *episode-length* 100)
(defvar *featurizers* nil)
(Defvar *bucket-functions* nil)

(defvar *program* nil)

;;; Possible values: :random, :epsilon, :boltzman
(defvar *exploration-strategy* :random)

(defun setup-experiment (environment-generator program episode-count episode-length featurizers bucket-functions)
    (setf *environment-generator* environment-generator)
    (setf *environment* (funcall environment-generator))
    (setf *program* program)
    (setf *episode-count* episode-count)
    (setf *episode-length* episode-length)
    (setf *featurizers* featurizers)
    (setf *bucket-functions* bucket-functions)
    *environment*)
    
(defun make-new-environment ()
    (setf *environment* (funcall *environment-generator*))
    *environment*)
    
(defun update-environment (environment)
    (setf *environment* environment))

(defun number-of-episodes ()
  *episode-count*)

(defun max-steps-per-episode ()
  *episode-length*)

(defun initialize-environment (&optional (force t))
  (when (or force (not *environment*))
    (setf *environment* (make-new-environment))))

(defun explore-environment (&optional (recreate nil))
  (initialize-environment (not recreate))
  (env:io-interface *environment*))

(defparameter *hordq-learning-rate* 0.1)
(defparameter *hordq-discount* 1)

(defun hordq-featurizer-for (algorithm kind)
     (cdr (assoc algorithm *featurizers*)))

(defun hordq-bucket-function-for (algorithm kind)
     (cdr (assoc algorithm *bucket-functions*)))

(defun initialize-algorithms (&optional (algorithm-names *algorithm-names*)
                                        (hordq-learning-rate *hordq-learning-rate*)
                                        (hordq-discount *hordq-discount*))
  (setf *algorithm-names* algorithm-names)
  (setf (fill-pointer *algorithms*) 0)
  (mapc (lambda (alg)
          (when (member (first alg) *algorithm-names*)
            (vector-push-extend (apply 'make-algorithm-description (rest alg)) *algorithms*)))
        (list*
         (list 'smdpq :algorithm (alisp-smdpq:make-smdpq-alg :hist-out-dir "Temp/"))
         (list 'hordq :algorithm (make-instance 'ahq:<hordq>))
         (list 'gold-standard 
               :algorithm (alisp-gold-standard:make-alisp-gold-standard-learning-alg))
         (mapcar (lambda (algorithm)
                   (list algorithm
                         :algorithm (make-instance 'ahq:<hordq>
                                      :features (hordq-featurizer-for algorithm *experiment-kind*)
                                      :lrate hordq-learning-rate
                                      :discount hordq-discount)
                         :bucket-function (hordq-bucket-function-for algorithm *experiment-kind*)
                         :test #'equalp))
                 '(hordq-a-0 hordq-a-1 hordq-a-2 hordq-a-3))))
  (values))

(defun explore-policies (&optional (show-advice t))
  (initialize-environment nil)
  (set-up-exploration)
  (alisp:io-interface *program* *environment*
                (if show-advice
                    (let ((hists (map 'list #'get-q-hist (algorithms))))
                      (mapcan (lambda (hist)
                                (if (and (typep hist 'sequence) (> (length hist) 0))
                                    (list (aref hist (1- (length hist))))
                                    '()))
                              hists))
                    '())))

(defun pick-exploration-strategy (algorithm-description
                                  &optional (strategy *exploration-strategy*))
  (let ((result 
          (ecase strategy
            ((:random)
             'random)
            ((:epsilon)
             (make-instance '<epsilon-policy>
               :q-learning-alg (ad-algorithm algorithm-description)
               :bucket-fn (ad-bucket-function algorithm-description)
               :test (ad-test algorithm-description)))
            ((:boltzman)
             (make-instance 'exp-pol:<epsilon-boltzmann-exp-pol>
               :q-learning-alg (ad-algorithm algorithm-description)
               :bucket-fn (ad-bucket-function algorithm-description)
               :test (ad-test algorithm-description)
               ;; TODO: Make first parameter depend on number of trials
               :temp-fn (lambda (n) (/ 1000.0 (1+ n)))
               :epsilon-decay-fn (exp-pol:make-linear-epsilon-decay-fn 10000 0.01))))))
    (setf *current-exploration-strategy* result)
    result))

(defun learn-behavior (&key (program *program*)
                            environment-type
                            (initialize-algorithms t)
                            (use-complex-environment nil use-complex-environment-p)
                            (exploration-strategy *exploration-strategy* exploration-strategy-p)
                            (algorithm-names *algorithm-names* algorithm-names-p)
                            (hordq-learning-rate *hordq-learning-rate* hordq-learning-rate-p)
                            (hordq-discount *hordq-discount* hordq-discount-p))
  (when environment-type
    (setf *environment-type* environment-type))
  (when use-complex-environment-p
    (setf *use-complex-environment* use-complex-environment))
  (when exploration-strategy-p
    (setf *exploration-strategy* exploration-strategy))
  (when algorithm-names-p
    (setf *algorithm-names* algorithm-names))
  (when hordq-learning-rate-p
    (setf *hordq-learning-rate* hordq-learning-rate))
  (when hordq-discount-p
    (setf *hordq-discount* hordq-discount))
  (initialize-environment)
  (when initialize-algorithms
    (initialize-algorithms algorithm-names hordq-learning-rate hordq-discount))
  (case exploration-strategy 
    ((:random)
     (format t "~&Learning behavior using random exploration strategy~%")
     (learn program *environment* 'random
            (coerce (algorithms) 'list)
            (* (max-steps-per-episode) (number-of-episodes))
            :hist-length 100 :step-print-inc 1000 :episode-print-inc 100))
    (otherwise
     (format t "~&Learning behavior using exploration strategy ~A~%"
             exploration-strategy)
     (map nil
          (lambda (ad)
            (learn program *environment*
                   (pick-exploration-strategy ad exploration-strategy)
                   (ad-algorithm ad)
                   (* (max-steps-per-episode) (number-of-episodes))
                   :hist-length 100 :step-print-inc 1000 :episode-print-inc 100))
          (algorithm-descriptions)))))

(defparameter *evaluation-steps* 50)
(defparameter *evaluation-trials* 25)

(defun evaluation-for (name)
  (evaluate *program* *environment* (get-policy-hist (algorithm-for name))
            :num-steps *evaluation-steps* :num-trials *evaluation-trials*))

(defparameter *gnuplot-file-template*
  "set title 'Learning Curve for ~A (WR, ~A, ~A)'
set title font \",15\"
set xlabel 'episodes (%)'
set xlabel font \",12\"
set ylabel 'reward'
set ylabel font \",12\"
set key off
set autoscale xy
unset grid
set style line 1 lt rgb \"blue\" lw 2 pt 6

set terminal pngcairo
set output '~A'

plot '~A' with linespoints ls 1
")

(defvar *file-exists-action* :error)

(defun evaluate-performance (&key (algorithm-names *algorithm-names*)
                                  (output-directory
                                   (merge-pathnames
                                    (make-pathname :directory '(:relative ".." "Gnuplot"))
                                    (asdf:system-definition-pathname :academia)))
                                  (gnuplot-file-prefix)
                                  (data-file-prefix "data")
                                  (png-file-prefix gnuplot-file-prefix)
                                  (output-to-terminal (not gnuplot-file-prefix)))
  (when output-to-terminal
    (format t "~2&Learning curves for ~{~A~^, ~} are:~%" algorithm-names)
    (pprint (map 'list #'list 
                 (mapcar 'evaluation-for algorithm-names))))
  (when gnuplot-file-prefix
    (ensure-directories-exist output-directory)
    (dolist (alg-name algorithm-names)
      (let* ((file-postfix (concatenate 'string
                                        "-academia-"
                                        (string-downcase (symbol-name alg-name)) "-" 
                                        (string-downcase (symbol-name *environment-type*))
                                        (if *use-complex-environment* "-complex-" "-simple-")
					(format nil "~A" (max-steps-per-episode))))
             (data-file (merge-pathnames
                          (make-pathname :name (concatenate 'string
                                                            data-file-prefix
                                                            file-postfix)
                                         :type "dat")
                          output-directory))
            (gnuplot-file (merge-pathnames
                           (make-pathname :name (concatenate 'string
                                                             gnuplot-file-prefix
                                                             file-postfix)
                                          :type "plt")
                           output-directory))
            (png-file (merge-pathnames
                       (make-pathname :name (concatenate 'string
                                                         png-file-prefix
                                                         file-postfix)
                                      :type "png")
                       output-directory)))
        (format t "~&Writing data to file ~A." (enough-namestring data-file))
        (with-open-file (stream data-file :direction :output
                                            :if-exists *file-exists-action*
                                            :if-does-not-exist :create)
          (map nil 
               (lambda (value)
                 (format stream "~&~A~%" value))
               (let ((*evaluation-steps* (floor (* *evaluation-steps* 2.5))))
                 (evaluation-for alg-name))))
        (format t "~&Writing gnuplot driver file ~A." (enough-namestring gnuplot-file))
        (with-open-file (stream gnuplot-file :direction :output
                                            :if-exists *file-exists-action*
                                            :if-does-not-exist :create)
          (format stream *gnuplot-file-template*
                  alg-name
                  *environment-type*
                  (if *use-complex-environment* "Complex" "Simple")
                  (namestring png-file)
                  (namestring data-file)))))))

#+(or)
(defun clean-up ()
  (reset *smdpq*))

(defun print-hash-values (q-getter algorithm)
  (let ((hash (fn-approx:params (q-fn:fn-approx (funcall q-getter algorithm)))))
    (maphash (lambda (k v)
               (format t "~&~A: ~A" k v))
             hash)))

(defun print-qr-values (&optional (algorithm (aref (algorithms) 0)))
  (print-hash-values #'ahq::qr algorithm))
(defun print-qc-values (&optional (algorithm (aref (algorithms) 0)))
  (print-hash-values #'ahq::qc algorithm))
(defun print-qe-values (&optional (algorithm (aref (algorithms) 0)))
  (print-hash-values #'ahq::qe algorithm))
