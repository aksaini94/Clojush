(ns clojush.pushgp.selection.lexicase
  (:use [clojush random]))

(defn shuffle-cases
  [pop argmap]
  (if (= (:sort-meta-errors-for-lexicase argmap) :random)
    (lshuffle (range (count (:errors (first pop)))))
    (let [num-all-errors (count (:errors (first pop))) ;; will included meta-errors, added in select
          num-meta-errors (count (:meta-errors (first pop)))
          num-true-errors (- num-all-errors num-meta-errors)
          true-error-indices (range num-true-errors)
          meta-error-indices (map #(+ % num-true-errors)
                                  (range num-meta-errors))]
      (case (:sort-meta-errors-for-lexicase argmap)
        :first (concat (lshuffle meta-error-indices)
                       (lshuffle true-error-indices))
        :last (concat (lshuffle true-error-indices)
                      (lshuffle meta-error-indices))))))

(defn lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))


(defn select-complimentary-mate
  "Outputs the complimentary mate of the given individual. Basically, if for the given individual the ranking of cases is:
  i (best at case i), i+1, .... j (worst at case j), the function returns the individual which is lexicase-selected using
  the case sequence: j, ..., i+1, i."
  [ind pop argmap]
  (let [ranks (loop [cases (shuffle-cases pop argmap)
                     rankings (atom {})]
                (if (empty? cases)
                  @rankings
                  (let [num-worse-inds (count (filter #(> % (nth (:errors ind) (first cases)))  (map #(nth % (first cases)) (map :errors pop)) ))
                        _ (swap! rankings assoc (first cases) num-worse-inds)]
                    (recur (rest cases) rankings))))]
    (loop [survivors pop
           cases (map first (sort-by val < ranks))] ;if ind is not good at some case, it comes first
      (if (or (empty? cases) (empty? (rest survivors)) (< (lrand) (:lexicase-slippage argmap)))
        (rand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))]
          (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                         survivors)
                 (rest cases)))))
    ))

