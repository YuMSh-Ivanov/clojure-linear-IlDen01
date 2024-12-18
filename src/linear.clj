(ns linear)

(defn all-oper [oper & args]
  (apply mapv oper args))

(def v+
  (partial all-oper +))

(def v-
  (partial all-oper -))

(def v*
  (partial all-oper *))

(def vd
  (partial all-oper /))

(defn dot [& vecs]
  (if (empty? vecs)
    0
    (apply + (apply mapv * vecs))))

(defn v*s [vec & scs]
  (mapv #(apply * % scs) vec))

(def m+
  (partial all-oper v+))

(def m-
  (partial all-oper v-))

(def m*
  (partial all-oper v*))

(def md
  (partial all-oper vd))

(defn m*s [mat & scs]
  (mapv #(apply v*s % scs) mat))

(defn transpose [mat]
  (apply mapv vector mat))

(defn m*v [mat vec]
  (all-oper #(dot % vec) mat))

(defn m*m [& mats]
  (reduce (fn [res next]
            (all-oper #(m*v (transpose next) %) res))
          mats))
