(ns nash-clojure.fast
  (:require [nash-clojure.core :as core]))

;;Faster path using primitive arrays and direct indexing.
(set! *unchecked-math* true)

(defn one-index-fast
  "Given a player-agnostic strategy index, find the index of the 1 for the given row."
  ^long [^long row-selector ^long number-of-columns ^long strategy-index]
  (+ (* row-selector number-of-columns)
     (mod (quot strategy-index (Math/pow number-of-columns row-selector))
          number-of-columns)))

;;eliminates intermediate seq; use native ctor since clojure.core/eduction ends up
;;invoking a bunch of seq stuff just to compose the functions passed to it; we
;;only have 2 inputs so can directly build the thing.
(defn one-indexes-fast
  "Given a player-agnostic strategy index, find the index of the 1 for each row."
  [number-of-rows number-of-columns strategy-index]
  (clojure.core.Eduction.
   (map #(one-index-fast % number-of-columns strategy-index))
   (range number-of-rows)))

;;only or's where we know there are values.  eliminates intermediate
;;vectors/seqs, sparse traversal.
(defn or-ones [rows columns indices]
  (reduce (fn [^longs arr idx]
            (do (aset arr idx 1)
                arr))
          (long-array (* ^long rows ^long columns)) indices))

;;no need to delineate if we have no need for column-rank-order..
;;~5-6x faster
(defn player-agnostic-strategy-fast
  "Given a strategy index, find the player-agnostics strategy for a game of the given size."
  [number-of-rows number-of-columns strategy-index]
  (or-ones number-of-rows number-of-columns
           (one-indexes-fast number-of-rows number-of-columns strategy-index)))

;;~57x faster
#_(defn row-col-and [^longs row-major ^longs col-major rows cols]
  (let [result     (long-array  (* ^long rows ^long cols))
        n          (volatile! 0)]
    (dotimes [i rows]
      (dotimes [j cols]
        (let [^long idx @n]
          (aset result ^long idx ^long (bit-and (aget row-major (+ (* j rows) i))
                                                (aget col-major idx)))
          (vreset! n (unchecked-inc idx)))))
    result))

;;funny, replacing the volatile with an array gets us 4.63x faster than
;;volatile impl..
(defn row-col-and [^longs row-major ^longs col-major rows cols]
  (let [result     (long-array  (* ^long rows ^long cols))
        n          (long-array 1 0)]
    (dotimes [i rows]
      (dotimes [j cols]
        (let [idx (aget n 0)]
          (aset result idx ^long (bit-and (aget row-major (+ (* j ^long rows) i))
                                          (aget col-major idx)))
          (aset n 0 (unchecked-inc idx)))))
    result))

(defn categorize-nash-solution-fast
  "Categorize the Nash solutions for the given two player game."
  [player-one-strategy player-two-strategy rows cols]
  (let [^longs arr (row-col-and player-one-strategy player-two-strategy rows cols)]
    (->> (areduce arr idx acc 0 (if (zero? (aget arr idx))
                             acc
                             (unchecked-inc acc)))
         (min 2)))) ;;replaces the old filter for 1's, take 2, count from before.

;;about 100x faster.
(defn categorize-nash-game-fast
  "Generate the player 1 and 2 strategies and categorize the Nash solutions for the given game index."
  [number-of-rows number-of-columns game-index]
  (categorize-nash-solution-fast
   (player-agnostic-strategy-fast number-of-rows number-of-columns
      (quot game-index (Math/pow ^long number-of-rows ^long number-of-columns)))
   (player-agnostic-strategy-fast number-of-rows number-of-columns
                                  (mod game-index (Math/pow  ^long number-of-rows ^long number-of-columns)))
   number-of-rows number-of-columns))



;;Fast path; just lame copy/paste and replacing existing
;;functions with their -fast counterpart.

(defn categorize-given-nash-games-fast
  "Categorize the Nash solutions for games with the given games indoces."
  [number-of-rows number-of-columns start-and-end-indices]
  (frequencies (map #(categorize-nash-game-fast number-of-rows number-of-columns %)
                    (range (nth start-and-end-indices 0) (+ (nth start-and-end-indices 1) 1)))))

(defn categorize-nash-games-fast
  "Categorize the Nash solutions for games of a given size, using the given number of paritions
   using one thread."
  [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (core/number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2) 
                               (map #(categorize-given-nash-games-fast number-of-rows number-of-columns %)
                                    (core/partition-nash-games number-of-partitions number-of-games))))))

(defn pcategorize-nash-games-fast
  "Categorize the Nash solutions for games of a given size, using the given number of partitions
   using one thread per partition."
  [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (core/number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2)
                               (pmap #(categorize-given-nash-games-fast number-of-rows number-of-columns %)
                                     (core/partition-nash-games number-of-partitions number-of-games))))))

(set! *unchecked-math* false)

;;took ~11 minutes wit 4 threads.
(defn bench [& {:keys [rows cols n] :or {rows 5 cols 5 n 1}}]
  (println (time (core/pcategorize-nash-games rows cols n))))

;;Done in 6 seconds on 4 threads.
(defn bench-fast [& {:keys [rows cols n] :or {rows 5 cols 5 n 1}}]
  (println (time (pcategorize-nash-games-fast rows cols n))))


(comment

  ;;about 117x faster now to do a game.

  ;; nash-clojure.core> (c/quick-bench (core/categorize-nash-game 5 5 100))
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

  ;; nash-clojure.core> (->>  (repeatedly 100000 #(rand-int (core/number-of-nash-games 5 5)))
  ;;                          (map (fn [idx]  [idx (core/categorize-nash-game 5 5 idx) (categorize-nash-game-fast 5 5 idx)]))
  ;;                          (some (fn [[idx l r]] (when (not= l r) idx))))
  ;; nil


  ;; So we get - as expected - about linear speedup on my
  ;; rig up through 4x.

  ;; After that, the dminishing returns start kicking in.
  ;; I have 4 logical cores, 8 "processors" likely hyper threads on mine.

  ;;This is typical scaling behavior I've observed with Clojure.

  ;; nash-clojure.fast> (bench-fast :n 1)
  ;; "Elapsed time: 18339.8527 msecs"
  ;; {0 2764880, 1 4581226, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 2)
  ;; "Elapsed time: 9959.0236 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 3)
  ;; "Elapsed time: 7139.6892 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 4)
  ;; "Elapsed time: 5855.4898 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 5)
  ;; "Elapsed time: 5070.2283 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 6)
  ;; "Elapsed time: 4530.2396 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 7)
  ;; "Elapsed time: 4143.6082 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 8)
  ;; "Elapsed time: 3958.4207 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 9)
  ;; "Elapsed time: 3927.3654 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil
  ;; nash-clojure.fast> (bench-fast :n 10)
  ;; "Elapsed time: 3937.4671 msecs"
  ;; {0 2764880, 1 4581225, 2 2419520}
  ;; nil

  (require '[tech.viz.vega :as vega])
  (def times [18339.8527
              9959.0236
              7139.6892
              5855.4898
              5070.2283
              4530.2396
              4143.6082
              3958.4207
              3927.3654
              3937.4671])
  (-> (->> times
           (map-indexed  (fn [idx t] {:threads idx :time-ms t})))
       (vega/scatterplot :threads :time-ms)
       (vega/vega->svg-file "samples.svg"))
  )


;;row-col experiments with storage..
(comment
  ;;original volatile impl us 269 ns
  (defn row-col-and1 [^longs row-major ^longs col-major rows cols]
    (let [result     (long-array  (* ^long rows ^long cols))
          n          (volatile! 0)]
      (dotimes [i rows]
        (dotimes [j cols]
          (let [^long idx @n]
            (aset result ^long idx ^long (bit-and (aget row-major (+ (* j ^long rows) i))
                                                  (aget col-major idx)))
            (vreset! n (unchecked-inc idx)))))
      result))

  (defmacro dotimes-totaled
    [bindings & body]
    (assert (= (count bindings) 4) "expected 2 bindings!")
    (let [[i n total k]   bindings]
      `(let [n#     (long ~n)
             total# (long ~k)]
         (loop [~i 0
                ~total total#]
           (if (< ~i n#)
             (let [new-total# (long ~@body)]
               (recur (unchecked-inc ~i)  new-total#))
             ~total)))))

  (defmacro dotimes-indexed
    ([bindings idx body]
     `(dotimes-indexed ~bindings ~idx 0 ~body))
    ([bindings idx depth body]
     (if (seq bindings)
       (let [[l r] (take 2 bindings)]
         `(dotimes-totaled [~l ~r ~idx ~(if (pos? depth) idx 0)]
                           (dotimes-indexed [~@(drop 2 bindings)] ~idx ~(inc depth) ~body)))
       `(do ~body
            (unchecked-inc ~idx)))))

  ;;optimized volatile-less gets us 72 ns (3.73x faster)
  (defn row-col-and2 [^longs row-major ^longs col-major rows cols]
    (let [result     (long-array  (* ^long rows ^long cols))]
      (dotimes-indexed [i 5 j 5] idx
                       (aset result idx ^long (bit-and (aget row-major (+ (* j ^long rows) i))
                                                       (aget col-major  idx))))
      result))

  ;;notably, direct method invocation on a hinted volatile doesn't help (maybe inlining)

  ;;Storing the cached valued in a long array gets us 58 ns..lol.
  (defn row-col-and [^longs row-major ^longs col-major rows cols]
    (let [result     (long-array  (* ^long rows ^long cols))
          n          (long-array 1 0)]
      (dotimes [i rows]
        (dotimes [j cols]
          (let [idx (aget n 0)]
            (aset result idx ^long (bit-and (aget row-major (+ (* j ^long rows) i))
                                            (aget col-major idx)))
            (aset n 0 (unchecked-inc idx)))))
      result))
  )
