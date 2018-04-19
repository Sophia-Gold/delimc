(ns delimc.amb
  (:require [delimc.core :refer :all]))

(def fail (atom (println "amb tree exhausted")))

(defn amb
  ([] fail)
  ([expr] expr)
  ([& exprs]
   (let [fail-save @fail]
     (cwcc
      (fn [k-success]
        (cwcc
         (fn [k-failure]
           (reset! fail (fn [] (k-failure nil)))
           (k-success (fn [] exprs))))
        (reset! fail fail-save)
        @fail-save)))))

(defn amb-possibilities  
  [expr] 
  (let [plist (atom [])] 
    (amb (let [p expr] 
           (swap! plist conj value) 
           fail)
         plist)))
