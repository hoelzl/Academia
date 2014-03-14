(in-package #:common-lisp-user)

(defpackage #:academia-env
  (:use #:common-lisp
        ;; #:hexameter
        ;; From ALisp
        #:utils
        #:academia-utils
        #:cl-heap
        #:direct-product-set
        #:prob
        #:create-env
        #:grid-world)
  (:import-from #:alexandria
		#:compose #:curry #:rcurry)
  (:nicknames :ae)
  (:export
   ;; Algorithm info
   #:*algorithm-names*
   #:*current-exploration-strategy*
   #:*step-number-multiplier*
   #:*algorithms*
   #:algorithm-description
   #:make-algorithm-description #:algorithm-description-p
   #:ad-algorithm
   #:ad-bucket-function
   #:ad-test
   #:algorithm-descriptions
   #:algorithm-description-for
   #:algorithm-for
   #:algorithms
   #:current-algorithm

   #:extract-q-function-table
   #:extract-q-function-tables
   #:build-combined-table
   #:build-state-table

   ;; The Environment
   #:<waste-env>
   #:<hades-env>
   #:*print-graphically*
   #:set-up-exploration
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W
   #:pickup 
   #:drop
   #:refuel

   ;; Accessors for <waste-env>
   #:init-fuel
   #:fuel-decrease-prob
   #:fuel-amount-per-step
   #:no-fuel-cost
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:waste-sources
   #:waste-targets
   #:waste-delivery-reward

   #:make-waste-env-0
   #:make-waste-env-1
   #:make-waste-env-2
   #:make-waste-env-3
   #:make-waste-env-4
   #:make-waste-env-5
   #:make-waste-env-6

   ;; States (Not sure whether this should really be exposed, but we need the functions
   ;; to define state accessors)
   #:academia-state
   #:ac-robot-loc
   #:ac-waste-status
   #:ac-waste-source
   #:ac-waste-target
   #:ac-fuel
   #:ac-env

   ;; The waste status 
   #:on-robot
   #:at-source
   #:at-dest

   ;; The rescue scenario
   #:graph #:make-graph #:graph-nodes #:graph-edges
   #:find-node
   #:robot #:make-robot #:robot-id #:robot-cargo #:robot-damage
   #:world-object #:make-world-object #:object-id #:object-location
   #:rubble #:make-rubble
   #:victim #:make-victim #:victim-health
   #:point #:make-point #:point-x #:point-y #:distance
   #:node #:make-node #:node-id #:node-cost #:node-edges #:node-graph #:node-neighbors
   #:edge #:make-edge #:edge-id #:edge-from #:edge-to #:edge-cost
   #:search-node #:make-search-node #:sn-location #:sn-parent #:sn-edge
   #:sn-cost-from-start #:sn-cost-to-goal #:sn-total-cost #:sn-heap-index
   #:build-path #:a*-search

   ;; Rescue states
   #:rescue-state #:make-rescue-state
   #:rs-location #:rs-cargo #:rs-damage #:rs-robot-locations #:rs-objects
   #:rs-actions
   #:node-objects

   ;; The rescue environment
   #:<rescue-env>
   #:nav-graph #:home-node #:max-damage #:invalid-action-cost

   #:make-rescue-env-0
   #:make-rescue-env-1
   #:make-rescue-env-2
   #:make-rescue-env-3
   #:make-rescue-env-4
   #:make-rescue-env-5
   #:make-rescue-env-6

   ;; Algorithm names
   #:smdpq
   #:hordq
   #:gold-standard
   #:hordq-a-0
   #:hordq-a-1
   #:hordq-a-2
   #:hordq-a-3))

(defpackage #:academia-prog
  (:use #:common-lisp
        #:academia-utils
        ;; From ALisp
        #:utils
        #:alisp-prog
        #:alisp-features
        #:alisp-user
        #:academia-env)
  (:nicknames :ap)
  (:export
   ;; The Top-Level Program
   #:academia-robot-prog
   
   ;; Partial Programs
   #:navigate
   #:pickup-waste
   #:drop-wate
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W
   #:pickup 
   #:drop
   #:refuel
   
   ;; Environment
   #:<waste-env>
   #:init-fuel
   #:fuel-decrease-prob
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:waste-sources
   #:waste-targets
   #:waste-delivery-reward))
