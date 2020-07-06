(ns clojush.pushgp.selection.preselection
  (:use [clojush random util]))

(defn closest-association1
  "Returns the key-val pair for the closest match to the given tag
   in the given state."
  [tag state]
  (loop [associations (conj (vec (:tag state)) (first (:tag state)))] ;; conj does wrap
    (if (or (empty? (rest associations))
            (<= tag (ffirst associations)))
      (first associations)
      (recur (rest associations)))))


(defn one-individual-per-error-vector-for-lexicase
  "When :parent-selection is a lexicase method, returns only one random individual 
  to represent each error vector."
  [pop {:keys [parent-selection]}]
  (if (some #{parent-selection}
            #{:lexicase :leaky-lexicase :epsilon-lexicase :elitegroup-lexicase 
              :random-threshold-lexicase :random-toggle-lexicase 
              :randomly-truncated-lexicase})
    (map lrand-nth (vals (group-by #(:errors %) pop)))
    pop))

(defn nonempties-for-autoconstruction
  "When :autoconstuctive is truthy, and at least one individual in pop has a non-empty
  genome, returns only those individuals with non-empty genomes."
  [pop {:keys [autoconstructive]}]
  (if autoconstructive
    (let [with-non-empty-genomes (filter #(not (empty? (:genome %))) pop)]
      (if (not (empty? with-non-empty-genomes))
        with-non-empty-genomes
        pop))
    pop))

(defn age-mediate
  "If age-mediated-parent-selection is falsy, returns pop. Otherwise, 
  age-mediated-parent-selection should be a vector of [pmin pmax] with pmin and pmax both 
  being between 0 and 1 (inclusive) with pmin + pmax <= 1.0. Then, with probability pmin,
  returns individuals in pop with the minimum age; with probability pmax, returns all of pop;
  with probability (- 1.0 pmin pmax), selects an age cutoff uniformly from those present
  in the population and returns individuals with the cutoff age or lower. If a third
  element of :invert is included in age-mediated-parent-selection then with probability
  pmin, returns individuals in pop with the maximum age; with probability pmax, returns 
  all of pop; with probability (- 1.0 pmin pmax), selects an age cutoff uniformly from
  those present in the population and returns individuals with the cutoff age or higher."
  [pop {:keys [age-mediated-parent-selection]}]
  (if (not age-mediated-parent-selection)
    pop
    (let [rand-val (lrand)
          amps age-mediated-parent-selection ;; just abbreviate
          invert (> (count amps) 2)] ;; assume any more args are just :invert
      (if (<= rand-val (first amps))
        (let [extreme-age (reduce (if invert max min) (map :age pop))]
          (filter #(= (:age %) extreme-age) pop))
        (if (<= rand-val (+ (first amps) (second amps)))
          pop
          (let [age-limit (lrand-nth (distinct (map :age pop)))]
            (filter (fn [ind] ((if invert >= <=) (:age ind) age-limit))
                    pop)))))))

(defn screen
  "If random-screen is falsy, returns pop. Otherwise, random-screen should be a map with
  values for :criterion and :probability. Then, with probability (- 1 :probability), again
  returns pop. Otherwise, a value is chosen randomly from the :grain-size values of
  the individuals in pop, and returns the individuals with that :grain-size or smaller."
  [pop {:keys [random-screen]}]
  (if (not random-screen)
    pop
    (if (> (lrand) (:probability random-screen))
      pop
      (let [grain-size-limit (lrand-nth (distinct (map :grain-size pop)))]
        (filter (fn [ind] ((if (:reversible random-screen)
                             (lrand-nth [<= >=])
                             <=)
                           (:grain-size ind) 
                           grain-size-limit))
                pop)))))

(defn filter-by-design-values
  "Select the individuals having top x percent design values."
  [pop {:keys [filter-params population-size]}]
  (if (not filter-params)
    pop
    (vec (take-last (int (* (first (:thresholds filter-params)) population-size)) (sort-by #(first (:reuse-info %)) pop)))))

(defn filter-by-tag0-size
  "Only those individuals are allowed where size of tag0 segment is less than or equal to 10."
  [pop]
  (filterv #(<= (count-points (second (closest-association1 0 {:tag (:tagspace %)}))) 10) pop)
  )

(defn preselect
  "Returns the population pop reduced as appropriate considering the settings for
  age-mediation, screening, selection method, and autoconstruction."
  [pop argmap]
  (-> pop
      ;(filter-by-design-values argmap)
      (filter-by-tag0-size)
      (nonempties-for-autoconstruction argmap)
      (age-mediate argmap)
      (screen argmap)
      (one-individual-per-error-vector-for-lexicase argmap)))

