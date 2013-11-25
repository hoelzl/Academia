(in-package :academia-env)

(defun extract-q-function-table (partial-q-designator algorithm)
  (let ((q-fn (ecase partial-q-designator
                ((:qr) #'ahq::qr)
                ((:qc) #'ahq::qc)
                ((:qe) #'ahq::qe))))
    (fn-approx:params (q-function:fn-approx (funcall q-fn algorithm)))))

(defun extract-q-function-tables (algorithm)
  (values (extract-q-function-table :qr algorithm)
          (extract-q-function-table :qc algorithm)
          (extract-q-function-table :qe algorithm)))

;;; This presupposes that QR, QC and QE all have the same featurizers and equality function.  In
;;; addition, the second element of each entry (i.e., the first feature from the featurizer)
;;; should be CHOICE.

(defun build-combined-table (algorithm)
  (multiple-value-bind (qr qc qe) (extract-q-function-tables algorithm)
    (let ((result (make-hash-table :test (hash-table-test qr) :size (hash-table-size qr))))
      (maphash (lambda (k v)
                 (setf (gethash k result)
                       (+ v (gethash k qc 0) (gethash k qe 0))))
               qr)
      result)))

(defun build-state-table ()
  (build-combined-table (current-algorithm)))
