;; last_index_of_zero.clj
;; Nic McPhee, mcphee@morris.umn.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers of length <= 50, each integer in the range [-50,50],
;; at least one of which is 0, return the index of the last occurance of 0 in the vector.
;;
;; input stack has 1 input vector of integers

(ns clojush.problems.software.last-index-of-zero-c
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

(def exec-reuse-instrs '(exec_dup, exec_dup_times, exec_dup_items, exec_yankdup, exec_do*range, exec_do*count,  exec_do*times, exec_while, exec_do*while, exec_s, exec_y, exec_do*vector_integer, exec_do*vector_float, exec_do*vector_boolean, exec_do*vector_string))

; Atom generators
(def last-index-of-zero-atom-generators
  (concat (list
            ^{:generator-label "Random numbers in the range [-50,50]"}
            (fn [] (- (lrand-int 101) 50))
            ;;; end ERCs
            ;(tag-instruction-erc [:exec] 10)
            ;(tagged-instruction-erc 10)
            ;'integer_tagged_instruction
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          ;(registered-for-type "return_")
          ;(remove (set exec-reuse-instrs) (registered-for-stacks [:integer :boolean :vector_integer :exec]))
          (registered-for-stacks [:integer :boolean :vector_integer :exec])
          ))

;; Define test cases
(defn random-sequence-with-at-least-one-zero
  [max-extra-zeros max-additional-values]
  (shuffle
   (concat
    [0] ; To ensure at least one zero
    (repeat (lrand-int (inc max-extra-zeros)) 0)
    (repeatedly (lrand-int (inc max-additional-values)) #(- (lrand-int 101) 50)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def last-index-of-zero-data-domains
  [^{:domain-label "length 2 vectors"}
   [(list [0 1]
          [1 0]
          [7 0]
          [0 8]
          [0 -1]
          [-1 0]
          [-7 0]
          [0 -8]) 8 0]
   ^{:domain-label "vectors of all zeros"}
   [(map #(vec (repeat (inc %) 0)) (range 50)) 30 20]
   ^{:domain-label "permutations of a 4 item vector with one zero"}
   [(map vec (permutations [0 5 -8 9])) 20 4]
   ^{:domain-label "permutations of a 4 item vector with two zeros"}
   [(map vec (permutations [0 0 -8 9])) 10 2]
   ^{:domain-label "permutations of a 4 item vector with three zeros"}
   [(map vec (permutations [0 0 0 9])) 4 0]
   ^{:domain-label "random cases"}
   [(fn [] (random-sequence-with-at-least-one-zero 5 44)) 78 974]
   ])

;;Can make Last Index of Zero test data like this:
;(test-and-train-data-from-domains last-index-of-zero-data-domains)

; Helper function for error function
(defn last-index-of-zero-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector % (.lastIndexOf % 0))
       inputs))

(defn make-last-index-of-zero-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-last-index-of-zero-error-function
    ([individual]
      (the-actual-last-index-of-zero-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-last-index-of-zero-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            state-with-tagspace-filled (run-push (:library individual) (push-item '(exec_noop) :input (make-push-state)))
            errors (doall
                     (for [[input correct-output] (case data-cases
                                                    :train train-cases
                                                    :test test-cases
                                                    data-cases)]
                       (let [final-state (run-push (:program individual)       ;'(tagged_0)
                                                   (push-item input :input
                                                              (assoc (make-push-state) :tag (:tag state-with-tagspace-filled))))
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %2d | Program output: %s"
                                            correct-output
                                            (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is absolute distance from correct index
                         (if (number? result)
                           (abs (- result correct-output)) ; distance from correct integer
                           1000000) ; penalty for no return value
                         )))
            modules-fitness (if true                            ;(= (count (keys (:tag state-with-tagspace-filled))) 1)
                              (assoc {} (first (keys (:tag state-with-tagspace-filled))) (repeat (count errors) 1))
                              (loop [tags (keys (:tag state-with-tagspace-filled))
                                     fitness {}]
                                (if (empty? tags)
                                  fitness
                                  (let [new-errors (doall
                                                     (for [[input correct-output] (case data-cases
                                                                                    :train train-cases
                                                                                    :test test-cases
                                                                                    data-cases)]
                                                       (let [final-state (run-push '(tagged_0)
                                                                                   (->> (assoc (make-push-state) :tag (dissoc (:tag state-with-tagspace-filled) (first tags)))
                                                                                        (push-item input :input)))
                                                             result (top-item :integer final-state)]
                                                         ; Error is Levenshtein distance
                                                         ; Error is absolute distance from correct index
                                                         (if (number? result)
                                                           (abs (- result correct-output)) ; distance from correct integer
                                                           1000000) ; penalty for no return value
                                                         )))]
                                    (recur (rest tags) (assoc fitness (first tags) (map - new-errors errors)))))
                                ))]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual :behaviors @behavior :errors errors :tagspace (:tag state-with-tagspace-filled) ;(merge-with cons (:tag state-with-tagspace-filled) modules-fitness)
                            ))))))

(defn get-last-index-of-zero-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map last-index-of-zero-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def last-index-of-zero-train-and-test-cases
  (get-last-index-of-zero-train-and-test last-index-of-zero-data-domains))

(defn last-index-of-zero-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first last-index-of-zero-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second last-index-of-zero-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn last-index-of-zero-report
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
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function                     (make-last-index-of-zero-error-function-from-cases (first last-index-of-zero-train-and-test-cases)
                                                                                          (second last-index-of-zero-train-and-test-cases))
   :training-cases                     (first last-index-of-zero-train-and-test-cases)
   :sub-training-cases                 '()
   :atom-generators                    last-index-of-zero-atom-generators
   :max-points                         1200
   :max-genome-size-in-initial-program 150
   :evalpush-limit                     600
   :population-size                    1000
   :max-generations                    300
   :parent-selection                   :lexicase
   :genetic-operator-probabilities     {:uniform-addition-and-deletion 1
                                        ;:tagged-segment-addition-and-deletion 0.25
                                        }
   :uniform-addition-and-deletion-rate 0.09
   :tagged-segment-addition-and-deletion-rate 0.5
   ;:genetic-operator-probabilities {:alternation                     0.2
   ; :uniform-mutation                0.2
   ; :uniform-close-mutation          0.1
   ; [:alternation :uniform-mutation] 0.5}
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.05                               ;temporarily changing it for uniform-tag-mutation
   :problem-specific-report last-index-of-zero-report
   :problem-specific-initial-report last-index-of-zero-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   :genome-representation :plushy
   :meta-error-categories [:tag-usage :size]
   ; filter by tag0 size (10) is off
   ;:tag-enrichment-types [:exec]
   ;:tag-enrichment 2
   })
