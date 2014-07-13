(ns p-p-p-pokerface)

(defn rank [card]
  (let [[encoded-rank _] card]
  (if (Character/isDigit encoded-rank)
    (Integer/valueOf (str encoded-rank))
    (let [ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
      (get ranks encoded-rank)))))

(defn suit [card]
  (let [[_, suit] card]
    (str suit)))

(defn rank-counts [hand]
  (frequencies (map rank hand)))

(defn has-n-of-a-kind [n hand]
    (>= (apply max (vals (rank-counts hand))) n))

(defn pair? [hand]
    (has-n-of-a-kind 2 hand))


(defn three-of-a-kind? [hand]
  (has-n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (has-n-of-a-kind 4 hand))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= `(2, 3) (sort (vec (vals (rank-counts hand))))))

(defn two-pairs? [hand]
  (let [gte2? (fn [x] (>= x 2))
        pair-or-better (filter gte2? (vals (rank-counts hand)))]
    (gte2? (count pair-or-better))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        low-rank (first sorted-ranks)]
    (or
     (= sorted-ranks (range low-rank (+ low-rank 5)))
     ; A is both 1 and 14, special-case the low ace straight
     (= sorted-ranks '(2 3 4 5 14)))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (let [checker-handvalue [
                           [pair? 1]
                           [two-pairs? 2]
                           [three-of-a-kind? 3]
                           [straight? 4]
                           [flush? 5]
                           [full-house? 6]
                           [four-of-a-kind? 7]
                           [straight-flush? 8]]
        valid-hand-values (map (fn [[checker?, value]] (if (checker? hand) value 0)) checker-handvalue)]
    (apply max valid-hand-values)))
