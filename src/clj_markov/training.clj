(ns clj-markov.training)

(defn shift-and-append
  [state token]
  (case (count state)
    1 [token]
    2 [(nth state 1) token]
    3 [(nth state 1) (nth state 2) token]
    4 [(nth state 1) (nth state 2) (nth state 3) token]
    5 [(nth state 1) (nth state 2) (nth state 3) (nth state 4) token]
    6 [(nth state 1) (nth state 2) (nth state 3) (nth state 4) (nth state 5) token]
    7 [(nth state 1) (nth state 2) (nth state 3) (nth state 4) (nth state 5) (nth state 6) token]
    (throw (IllegalArgumentException. "unsupported state length"))))

(defn- train*
  [chain token]
  (let [token-kw (keyword token)
        state (::state chain)
        state' (shift-and-append state token-kw)]
    (-> chain
      (update-in [state token-kw] (fnil inc 0))
      (assoc ::state state'))))

(def default-training-opts
  {:length 2})

(defn train
  ([tokens] (train tokens {}))
  ([tokens opts]
   (let [{:keys [length]} (merge default-training-opts opts)
         chain {::state (-> (repeat length nil) vec)}]
     (-> (reduce train* chain tokens)
       (dissoc ::state)))))
