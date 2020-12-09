;; tagged_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.tagged-regression-cc
  (:use [clojush util globals pushstate simplification individual]
        [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]
        [clojush.instructions.tag]
        [clojush.instructions.environment])
  (:require [clojure.string :as string]
            [local-file]))

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the
;; trivial geography chapter) with minimal integer instructions
;; ALSO uses tags, although there is little reason to think they would
;; help on such a simple problem.

;; Before executing the individual programs for the purpose of error calculation,
;; initialize its tagspace by tagging the whole program with a certain number for tags.
;; Types of values to be tagged include signle instructions/literals and list of instructions/literals
;; enclosed within brackets.

;;
;; Implements (x^3+1)^5 + 1
;;
(def fitness-train-cases
  (for [input (range -3.5 4.0 0.5)]
    [input
     (let [x-new (+ (* input input input) 1)]
       (+ (* x-new x-new x-new x-new x-new)
          1))]))

(def fitness-test-cases
  (for [input (range -3.75 4.25 0.5)]
    [input
     (let [x-new (+ (* input input input) 1)]
       (+ (* x-new x-new x-new x-new x-new)
          1))]))


(defn tagged-regression-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Last Index of Zero problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    ))

(def argmap
  {:error-function (fn error-function
                     ([individual]
                      (error-function individual :train))
                     ([individual data-cases] ;; data-cases should be :train or :test
                      (error-function individual data-cases false))
                     ([individual data-cases print-outputs]
                      (let [errors (doall
                                     (for [[input target] (case data-cases
                                                            :train fitness-train-cases
                                                            :test fitness-test-cases
                                                            data-cases)]
                                       (let [state (run-push (:program individual)
                                                             (push-item input :input
                                                                        (push-item input :float
                                                                                   ;(make-push-state)
                                                                                   (assoc (make-push-state) :tag (:library individual))
                                                                                   )))
                                             top-float (top-item :float state)]
                                         ;calculate errors
                                         (if (number? top-float)
                                           (abs (- top-float target))
                                           1000.0))))]
                        (if (= data-cases :test)
                          (assoc individual :test-errors errors)
                          (assoc individual :errors errors :tagspace (:library individual))
                          )
                        )))
   :atom-generators (concat (list (fn [] (lrand 10))
                                  'in1
                                  ;(tag-instruction-erc [:float :exec] 100)
                                  ;(untag-instruction-erc 100)
                                  ;(tagged-instruction-erc 100)
                                  )
                            (registered-for-stacks [:float :integer :exec])
                            )
   :training-cases                     fitness-train-cases
   :sub-training-cases                 '()
   :genetic-operator-probabilities     {[:uniform-addition-and-deletion  :module-replacement :module-unroll]  1}
   :module-replacement-rate 0.5
   :module-unroll-rate 0.1
   :uniform-addition-and-deletion-rate 0.09
   :parent-selection :epsilon-lexicase
   :max-generations 500
   :report-simplifications 0
   :problem-specific-report tagged-regression-report
   :final-report-simplifications 5000
   :genome-representation :plushy
   :meta-error-categories [:tag-usage :size]
   })