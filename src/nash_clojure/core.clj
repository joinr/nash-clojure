(ns
  #^{:author "Josh Peterson"
     :doc "Functions used to categorize Nash solutions to two-player games"} 
  nash-clojure.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :refer :all]
            [clojure.tools.cli :refer [cli]]))

;
; Two player game strategy generation
;

(defn vector-bit-or
  "Perform a bitwise OR operation on two or more vectors."
  [& vectors]
  (apply map bit-or vectors))

(defn vector-bit-and
  "Perform a bitwise AND operation on two or more vectors."
  [& vectors]
  (apply map bit-and vectors))

;;no primitive math, boxed.
(defn one-index
  "Given a player-agnostic strategy index, find the index of the 1 for the given row." 
  [row-selector number-of-columns strategy-index]
  (+ (* row-selector number-of-columns)
     (mod (quot strategy-index (expt number-of-columns row-selector))
          number-of-columns)))

;;intermediate seqs, not necessary if we just care about sparse indices.
(defn one-indexes
  "Given a player-agnostic strategy index, find the index of the 1 for each row."
  [number-of-rows number-of-columns strategy-index]
  (for [row-selector (range number-of-rows)]
    (one-index row-selector number-of-columns strategy-index)))

;;possible bottleneck too.  looks like it could be memoized too.
;;creates new vectors needlessly, tosses them for seqs.  hmm.

(defn player-agnostic-strategy
  "Given a strategy index, find the player-agnostics strategy for a game of the given size."
  [number-of-rows number-of-columns strategy-index]
  (apply vector-bit-or (map #(assoc (vec (repeat (* number-of-rows number-of-columns) 0)) % 1)
                            (one-indexes number-of-rows number-of-columns strategy-index))))

;;very slow due to apply map vector...
(defn column-rank-order
  "Apply the column rank order transformation (effectivel a transpose) to the given player
   agnostic strategy."
  [number-of-columns player-agnostic-strategy]
  (flatten (vec (apply map vector (partition number-of-columns player-agnostic-strategy)))))


(defn player-one-strategy
  "Find the player 1 strategy for the given strategy index."
  [number-of-rows number-of-columns strategy-index]
  (column-rank-order number-of-columns
                     (player-agnostic-strategy number-of-rows number-of-columns strategy-index)))

;;slow from count ?
(defn player-two-strategy
  "Find the player 2 strategy for the given strategy index."
  [number-of-rows number-of-columns strategy-index]
  (player-agnostic-strategy number-of-rows number-of-columns strategy-index))
;
; Nash game categorization
;

;;invoking count on sequences.  why count this when we are taking 2?
(defn categorize-nash-solution
  "Categorize the Nash solutions for the given two player game."
  [player-one-strategy player-two-strategy]
  (count (take 2 (filter #{1} (vector-bit-and player-one-strategy player-two-strategy)))))


(defn categorize-nash-game
  "Generate the player 1 and 2 strategies and categorize the Nash solutions for the given game index."
  [number-of-rows number-of-columns game-index]
  (categorize-nash-solution (player-one-strategy number-of-rows number-of-columns
                                                 (quot game-index (expt number-of-rows number-of-columns)))
                            (player-two-strategy number-of-rows number-of-columns
                                                 (mod game-index (expt number-of-rows number-of-columns)))))

;;fine
(defn number-of-nash-games
  "Determine the number of Nash games of a given size"
  [number-of-rows number-of-columns]
  (* (expt number-of-rows number-of-columns) (expt number-of-columns number-of-rows)))

;;not good.
#_(defn partition-nash-games
  "Determine the start and end indices of each group of Nash games"
  [number-of-partitions number-of-games]
  (let [entries-per-partition (quot number-of-games number-of-partitions)
        ;;holds onto head of lazy sequence.
        partitions ]
    (if (= 0 (rem number-of-games number-of-partitions))
      (nash-game-partitions entries-per-partition number-of-games)
      (let [first-indices (take (- number-of-partitions 1) partitions)
            last-indices-start (nth (nth (take-last 1 partitions) 0) 0)
            new-last-indices (conj [] last-indices-start (- number-of-games 1))]
        (apply concat (conj () (conj () new-last-indices) first-indices))))))


(defn partition-indices
  ([parts n]
   (if (= parts 1)
     [[0 n]]
     (partition-indices parts 0 (dec n) (dec (quot n parts)))))
  ([parts l r step]
   (cond (> parts 1)
         (let [lnext (+ l step)]
           (lazy-seq
            (cons [l lnext] (partition-indices (dec parts) (inc lnext) r step))))
         (== parts 1)
         [[l r]])))

;;should be instant.
(defn partition-nash-games
  "Determine the start and end indices of each group of Nash games"
  [number-of-partitions number-of-games]
  (partition-indices number-of-partitions number-of-games))

(defn categorize-given-nash-games
  "Categorize the Nash solutions for games with the given games indoces."
  [number-of-rows number-of-columns start-and-end-indices]
  (frequencies (map #(categorize-nash-game number-of-rows number-of-columns %)
                    (range (nth start-and-end-indices 0) (+ (nth start-and-end-indices 1) 1)))))

;;ok
(defn categorize-nash-games
  "Categorize the Nash solutions for games of a given size, using the given number of paritions
   using one thread."
  [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2) 
                               (map #(categorize-given-nash-games number-of-rows number-of-columns %)
                                    (partition-nash-games number-of-partitions number-of-games))))))

(defn pcategorize-nash-games
  "Categorize the Nash solutions for games of a given size, using the given number of partitions
   using one thread per partition."
  [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2)
                               (pmap #(categorize-given-nash-games number-of-rows number-of-columns %)
                                     (partition-nash-games number-of-partitions number-of-games))))))

; Main function
;

(defn -main [& args]
  (let [[options args banner]
    (cli args
      ["-h" "--help" "Show help" :flag true]
      ["-r" "--rows" "Number of rows in the game" :parse-fn #(Integer. %)] 
      ["-c" "--columns" "Number of columns in the game" :parse-fn #(Integer. %)]
      ["-t" "--threads" "Number of threads to use" :parse-fn #(Integer. %)])]
  (when (:help options)
      (println banner)
      (System/exit 0))
    
    (println (time (pcategorize-nash-games (:rows options) (:columns options) (:threads options)))))
  (System/exit 0))


(comment

  ;;about 117x faster now to do a game.

  ;; nash-clojure.core> (c/quick-bench (categorize-nash-game 5 5 100))
  ;; Evaluation count : 2838 in 6 samples of 473 calls.
  ;; Execution time mean : 210.810692 µs
  ;; Execution time std-deviation : 6.454449 µs
  ;; Execution time lower quantile : 204.950579 µs ( 2.5%)
  ;; Execution time upper quantile : 220.192915 µs (97.5%)
  ;; Overhead used : 1.852568 ns
  ;; nil

  ;; nash-clojure.core> (c/quick-bench (categorize-nash-game-fast 5 5 100))
  ;; Evaluation count : 347490 in 6 samples of 57915 calls.
  ;; Execution time mean : 1.782560 µs
  ;; Execution time std-deviation : 55.777733 ns
  ;; Execution time lower quantile : 1.720418 µs ( 2.5%)
  ;; Execution time upper quantile : 1.838751 µs (97.5%)
  ;; Overhead used : 1.852568 ns
  ;; nil


  ;;Same results over 100K random games.  Looking good!

  ;; nash-clojure.core> (->>  (repeatedly 100000 #(rand-int (number-of-nash-games 5 5)))
  ;;                          (map (fn [idx]  [idx (categorize-nash-game 5 5 idx) (categorize-nash-game-fast 5 5 idx)]))
  ;;                          (some (fn [[idx l r]] (when (not= l r) idx))))
  ;; nil

  ;;took ~11 minutes wit 4 threads.
  (defn test [& {:keys [n] :or {n 1}}]
    (println (time (pcategorize-nash-games 5 5 n))))

  ;;Done in 6 seconds on 4 threads.
  (defn test-fast [& {:keys [n] :or {n 1}}]
    (println (time (pcategorize-nash-games-fast 5 5 n))))

  )
