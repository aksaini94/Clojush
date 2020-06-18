;; double_letters.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string, print the string, doubling every letter character, and
;; trippling every exclamation point. All other non-alphabetic and non-exclamation
;; characters should be printed a single time each. The input string will have
;; maximum length of 20 characters.
;;
;; input stack has the input string

(ns clojush.problems.software.double-letters-c
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals individual]
        clojush.instructions.tag
        clojure.math.numeric-tower
        )
  (:require [clojure.math.numeric-tower :as math]))

; Atom generators
(def double-letters-atom-generators
  (concat (list
           \!
            ;;; end constants
            ;;; end ERCs
           ;(tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
           ;(tagged-instruction-erc 1000)
           ;(untag-instruction-erc 1000)
           ; (registered-for-type "return_")
            ;;; end tag ERCs
           'in1
            ;;; end input instructions
           )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; Define test cases
(defn double-letters-input
  "Makes a Double Letters input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(lrand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def double-letters-data-domains
  [[(list "", "A", "!", " ", "*", "\t", "\n", "B\n", "\n\n", "CD", "ef", "!!", "q!", "!R", "!#", "@!", "!F!", "T$L", "4ps", "q\t ", "!!!"
          (apply str (take 13 (cycle (list \i \: \!))))
          (apply str (repeat 20 \8))
          (apply str (repeat 20 \space))
          (apply str (repeat 20 \s))
          (apply str (repeat 20 \!))
          (apply str (take 20 (cycle (list \H \a \space))))
          (apply str (take 20 (cycle (list \x \newline \y \!))))
          (apply str (take 20 (cycle (list \1 \!))))
          (apply str (take 20 (cycle (list \G \5))))
          (apply str (take 20 (cycle (list \> \_ \= \]))))
          (apply str (take 20 (cycle (list \k \! \!))))) 32 0] ;; "Special" inputs covering some base cases
   [(fn [] (double-letters-input (inc (lrand-int 20)))) 68 1000]
   ])

;;Can make Double Letters test data like this:
;(test-and-train-data-from-domains double-letters-data-domains)

; Helper function for error function
(defn double-letters-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply str (flatten (map #(cond
                                             (Character/isLetter %) (list % %)
                                             (= % \!) (list % % %)
                                             :else %)
                                          in)))))
       inputs))

(defn make-double-letters-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-double-letters-error-function
    ([individual]
      (the-actual-double-letters-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-double-letters-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           tidiness (atom '())
           ;local-tagspace (case data-cases
           ;  :train (atom @global-common-tagspace)
           ;  :simplify (atom (:tagspace individual))       ; during simplification and testing, the tagspace should not be changed.
           ;  :test (atom (:tagspace individual))
           ;  (atom @global-common-tagspace))
           cases (case data-cases
                   :train train-cases
                   :simplify train-cases
                   :test test-cases
                   data-cases)
           errors (let [init-state (if (= (first cases) :permute) (assoc (make-push-state) :simplification-by-permutation true) (make-push-state))
                        cases (if (= (first cases) :permute) (rest cases) cases)]
                    (doall
                      (for [[input correct-output] cases]
                        (let [final-state (run-push (:program individual)
                                                    (->> (push-item input :input init-state)
                                                         (push-item "" :output)))
                              printed-result (stack-ref :output 0 final-state)]
                          (when print-outputs
                            (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str printed-result))))

                          (swap! tidiness conj (reduce + (map #(math/abs (- (first %) (second %))) (filter #(some? (second %)) (vec @global-id-arg)) )))
                          ;update the modularity metrics
                          ;(if (= [input correct-output] ran)
                          ;  (let [metrics (mod-metrics (:trace final-state) (:trace_id final-state))]
                          ;    (swap! reuse-metric conj (first metrics))
                          ;    (swap! repetition-metric conj (last metrics))))
                          ; Record the behavior
                          (swap! behavior conj printed-result)
                          ; Error is Levenshtein distance
                          (levenshtein-distance correct-output printed-result)))))
           ;_ (if (and (not= data-cases :test) (not= data-cases :simplify)) ;(= data-cases :train)
           ; (if (let [x (vec errors)
           ;          ;_ (prn x)
           ;          y (first (:history individual))
           ;          ;_ (prn y)
           ;]
           ; (if (nil? y)
           ;  true
           ;  ; (some? (some true? (map #(< %1 %2) x y))))) ; child is better than mom on at least one test case; can be worse on others
           ;  (every? true? (map #(<= %1 %2) x y))))
           ; (do
           ;  (reset! global-common-tagspace @local-tagspace)
           ;(prn @global-common-tagspace)
           ;)))
           ]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual :behaviors @behavior :errors errors :tidiness  @tidiness))))))

(defn get-double-letters-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map double-letters-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def double-letters-train-and-test-cases
  (get-double-letters-train-and-test double-letters-data-domains))

(defn double-letters-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first double-letters-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second double-letters-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn double-letters-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Double Letters problem report - generation %s\n" generation)(flush)
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
  {:error-function                     (make-double-letters-error-function-from-cases (first double-letters-train-and-test-cases)
                                                                                      (second double-letters-train-and-test-cases))
   :training-cases (first double-letters-train-and-test-cases)
   :sub-training-cases '()
   :atom-generators                    double-letters-atom-generators
   :max-points                         3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit                     1600
   :population-size                    1000
   :max-generations                    300
   :parent-selection                   :lexicase
   :genetic-operator-probabilities {:modified-uniform-addition-and-deletion 1}
   :uniform-addition-and-deletion-rate 0.09
   :add-instruction-from-other-rate 0.5
   ;:genetic-operator-probabilities {:alternation                     0.2
   ; :uniform-mutation                0.2
   ; :uniform-close-mutation          0.1
   ; [:alternation :uniform-mutation] 0.5
   ;}
   ;                                    :alternation-rate 0.01
   ;                                    :alignment-deviation 10
   ;                                    :uniform-mutation-rate 0.01
   :problem-specific-report double-letters-report
   :problem-specific-initial-report double-letters-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   ;:print-history true
   ;:pop-when-tagging false
   })
