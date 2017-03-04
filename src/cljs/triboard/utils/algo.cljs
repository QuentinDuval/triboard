(ns triboard.utils.algo)


(defn group-by-reducer
  "Create a reducer that groups by the provided key"
  [& key-fns]
  (fn
    ([] {})
    ([result] result)
    ([result val]
      (let [keys ((apply juxt key-fns) val)]
        (update-in result keys conj val)))))

(defn map-values
  "Apply a function to the values of a key-value collection"
  ([xf] (map (fn [[k v]] [k (xf v)])))
  ([xf coll]
    ;; Much faster than (into {} (map-values xf) coll)
    (persistent!
      (reduce-kv
        (fn [m k v] (assoc! m k (xf v)))
        (transient {})
        coll))
    ))
