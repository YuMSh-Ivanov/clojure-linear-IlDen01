(ns linear)

(defn v+
  [& vecs]
  (apply mapv + vecs))

(defn v-
  [& vecs]
  (apply mapv - vecs))

(defn v*
  [& vecs]
  (apply mapv * vecs))

(defn vd
  [& vecs]
  (apply mapv / vecs))

(defn dot
  [& vecs]
  (if (empty? vecs)
    0
    (reduce + (apply mapv * vecs))))

(defn v*s
  [vec & scs]
  (mapv #(* (apply * scs) %) vec))

(defn m+
  [& mats]
  (apply mapv v+ mats))

(defn m-
  [& mats]
  (apply mapv v- mats))

(defn m*
  [& mats]
  (apply mapv v* mats))

(defn md
  [& mats]
  (apply mapv vd mats))

(defn m*s
  [mat & scs]
  (mapv #(apply v*s % scs) mat))

(defn transpose
  [mat]
  (apply mapv vector mat))

(defn m*v
  [mat vec]
  (mapv #(dot % vec) mat))

(defn m*m
  [& mats]
  (reduce (fn [res next]
            (mapv #(mapv (partial dot %) (transpose next)) res))
          mats))
