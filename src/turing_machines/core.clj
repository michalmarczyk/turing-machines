(ns turing-machines.core)

(defmacro compile-turing-program [& state-cell-print-move-state*]
  (let [lefts  (gensym "lefts")
        rights (gensym "rights")
        state  (gensym "state")
        move-head (fn move-head [from-sym to-sym left?]
                    (let [to-form `(conj ~to-sym (first ~from-sym))
                          from-form `(next ~from-sym)
                          from-form-1 `'(~(boolean false))]
                      ((fn [[from to]]
                         (if left? [from to] [to from]))
                       [`(if (== 1 (count ~from-sym))
                           ~from-form-1
                           ~from-form)
                        to-form])))]
    `(fn [~lefts ~rights]
       (loop [~state  0
              ~lefts  ~lefts
              ~rights ~rights]
         (case [~state (first ~rights)]
           ~@(mapcat (fn [[state cell new-cell move new-state]]
                       ((if (not= cell new-cell)
                          (fn [[condition recur-form]]
                            [condition
                             `(let [~rights (conj (next ~rights) ~new-cell)]
                                ~recur-form)])
                          identity)
                        [[state cell]
                         `(recur ~new-state
                                 ~@(condp = move
                                     :left
                                     (move-head lefts rights true)
                                     :right
                                     (move-head rights lefts false)))]))
                     state-cell-print-move-state*)
           [~state ~lefts ~rights])))))

(defn make-tape
  ([] (let [init '(false)]
        [init init]))
  ([initial-content]
     (let [init-rights (apply list initial-content)]
       ['(false) init-rights])))

(defn numbers-to-tape [& nums]
  (->> nums
       (map #(repeat % (boolean true)))
       (interpose [(boolean false)])
       (apply concat)
       make-tape))

(defn final-tape [[_ lefts rights]]
  (concat (reverse lefts) [:>>] rights))

(defn final-number [tm-run-result]
  (->> tm-run-result
       final-tape
       (drop-while #(not= :>> %))
       next
       (take-while true?)
       count))

(comment
  (apply
   (tm/compile-turing-program
    [0 false true :right 1]
    [1 false true :right 2]
    [2 false true :right 3])
   (tm/make-tape)))
