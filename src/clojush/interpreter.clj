(ns clojush.interpreter
  (:use [clojush pushstate globals util mod_metrics]
        [clojush.instructions tag input-output]
        [clojush.experimental.tagged-code-macros])
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push interpreter


(defn execute-instruction
  "Executes a single Push instruction."
  [instruction state]
  ;; for debugging only, e.g. for stress-test
  ;(def debug-recent-instructions (cons instruction debug-recent-instructions))
  ;(def debug-recent-state state)
  (swap! point-evaluations-count inc)
  (if (= instruction nil) ;; tests for nil and ignores it
    state
    (let [literal-type (recognize-literal instruction)
          new-instruction (if (and (:simplification-by-permutation state) (or literal-type (and (vector? instruction) (= [] instruction))))
                        (assoc {} :value instruction :creation-time (first (:auxiliary state)))
                        instruction)
          _ (if (or literal-type (and (vector? instruction) (= [] instruction)))
              nil
              (if (:simplification-by-permutation state) (swap! global-id-arg assoc (first (:auxiliary state)) (first (:auxiliary state)))))
          ]
      (cond
        ;
        literal-type
        (push-item new-instruction literal-type state)
        ;
        (and (vector? instruction) (= [] instruction))
        (push-item new-instruction :vector_integer
                   (push-item new-instruction :vector_float
                              (push-item new-instruction :vector_string
                                         (push-item new-instruction :vector_boolean state))))
        ;
        (and (symbol? instruction)
             (re-seq #"in\d+" (name instruction)))
        (handle-input-instruction new-instruction state)
        ;
        (tag-instruction? instruction)
        (handle-tag-instruction new-instruction state)
        ;
        (tagged-code-macro? instruction)
        (handle-tag-code-macro new-instruction state)
        ;
        (contains? @instruction-table instruction)
        ((new-instruction @instruction-table) state)
        :else
        (throw (Exception. (str "Undefined instruction: " (pr-str instruction))))))))

(def saved-state-sequence (atom []))

(defn eval-push
  "Executes the contents of the exec stack, aborting prematurely if execution limits are
   exceeded. The resulting push state will map :termination to :normal if termination was
   normal, or :abnormal otherwise."
  ([state] (eval-push state false false false))
  ([state print-steps] (eval-push state print-steps false false))
  ([state print-steps trace] (eval-push state print-steps trace false))
  ([state print-steps trace save-state-sequence]
   ;(when (empty? @global-atom-generators)
   ;  (println "global-atom-generators is empty. You should do something like: (reset! global-atom-generators '(exec_if boolean_not true false))"))
   (loop [iteration 1
          s state
          time-limit (if (zero? @global-evalpush-time-limit)
                       0
                       (+' @global-evalpush-time-limit (System/nanoTime)))]
     (if (or (> iteration @global-evalpush-limit)
             (and (empty? (:exec s)) (empty? (:environment s)))
             (and (not (zero? time-limit))
                  (> (System/nanoTime) time-limit)))
       (assoc s :termination (if (and (empty? (:exec s)) (empty? (:environment s)))
                               :normal
                               :abnormal))
       (if (empty? (:exec s))
         (let [s (end-environment s)]
           (when print-steps
             (printf "\nState after %s steps (last step: %s):\n"
                     iteration "end_environment_from_empty_exec")
             (state-pretty-print s))
           (when save-state-sequence
             (swap! saved-state-sequence #(conj % s)))
           (recur (inc iteration) s time-limit))
         (let [exec-top (first (:exec s))
               ;exec-top (top-item :exec s)
               ;s (pop-item :exec s)
               s (update s :exec rest)]
           (let [s (let [exec-result (if (seq? exec-top)
                                       (let [_ (swap! global-id-arg assoc (- iteration 1) (- iteration 1))]
                                         (assoc s :exec (concat exec-top (:exec s))))
                                       (if (:calculate-mod-metrics state) ; additional operations only when calculating mod metrics
                                         (let [result (execute-instruction (:instr exec-top) s)]
                                           (assoc result :exec (correct-ids (:exec result) (atom 1000000))))
                                         (if (:simplification-by-permutation state)
                                           (let [s (push-item (- iteration 1) :auxiliary s)
                                                 exec-top (if (map? exec-top) (:value exec-top) exec-top)
                                                 result (execute-instruction exec-top s)]
                                             result)
                                           (execute-instruction exec-top s))
                                         ))]
                     (cond
                       (and (= trace false) (not (:calculate-mod-metrics state))) exec-result
                       (or (= trace true) (:calculate-mod-metrics state)) (assoc exec-result
                                                                            :trace
                                                                            (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))
                       (= trace :changes) (if (= exec-result s)
                                            exec-result
                                            (assoc exec-result
                                              :trace
                                              (cons exec-top (let [t (:trace s)] (if (seq? t) t ())))))))]
             (when print-steps
               (printf "\nState after %s steps (last step: %s):\n"
                       iteration (if (seq? exec-top) "(...)" exec-top))
               (state-pretty-print s))
             (when save-state-sequence
               (swap! saved-state-sequence #(conj % s)))
             (recur (inc iteration) s time-limit))))))))

(defn run-push
  "The top level of the push interpreter; calls eval-push between appropriate code/exec
   pushing/popping. The resulting push state will map :termination to :normal if termination was
   normal, or :abnormal otherwise."
  ([code state]
   (run-push code state false false false))
  ([code state print-steps]
   (run-push code state print-steps false false))
  ([code state print-steps trace]
   (run-push code state print-steps trace false))
  ([code state print-steps trace save-state-sequence]
   (swap! program-executions-count inc)
   (let [s (if @global-top-level-push-code (push-item code :code state) state)]
     (let [s (if (or (:calculate-mod-metrics state) (:simplification-by-permutation state))
               (let [_ (if (:simplification-by-permutation state)
                         (reset! global-id-arg {}))
                     _ (if (:simplification-by-permutation state)
                         (swap! global-id-arg merge (zipmap (range (count-points code) ) (repeat nil))))]
                 (push-item (not-lazy code) :exec s)
                 )
               (push-item (not-lazy code) :exec s))]
       (when print-steps
         (printf "\nState after 0 steps:\n")
         (state-pretty-print s))
       (when save-state-sequence
         (reset! saved-state-sequence [s]))
       (let [s (eval-push s print-steps trace save-state-sequence)
             ;_ (if (:simplification-by-permutation state)
             ;  (prn @global-id-arg))
             ]
         (if @global-top-level-pop-code
           (pop-item :code s)
           s))))))
