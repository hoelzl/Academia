(in-package #:academia-prog)

;;; Accessors for the current state

(def-env-accessor location rs-location
    "The current location of the rescue robot")
(def-env-accessor cargo rs-cargo
    "The cargo the rescue robot is currently carrying")
(def-env-accessor damage rs-damage
    "How damaged is the rescue robot currently")
(def-env-accessor robot-locations rs-robot-locations
    "The locations of the other robots in the swarm")
(def-env-accessor objects rs-objects
    "The objects currently in the environment.  This includes victims
    and rubble that can be used for building walls to shield from
    radiation.")
(def-env-accessor actions rs-actions
    "The actions that are currently possible")

(defun* random-action ()
  "Perform a random action"
  (with-choice random-action-choice (act (actions))
    (action perform-random-action act)))

(defun* random-rescue ()
  "Repeatedly perform a random action"
  (loop
    (random-action)))
