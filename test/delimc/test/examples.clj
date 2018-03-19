(ns delimc.test.examples
  (:require [delimc.core :refer :all]
            [clojure.test :refer :all]))

(defn cwcc
  "Scheme48 implementation of `call/cc` with shift/reset."
  [f]
  (reset
   (shift k
          (k (f (reset
                 (fn [x] 
                   (shift k2
                          (k x)))))))))

;; (defn yinyang []
;;   (let [yin ((fn [cc] (print "@") cc)
;;              (cwcc (fn [x] x)))
;;         yang ((fn [cc] (print "*") cc)
;;               (cwcc (fn [x] x)))]
;;     (yin yang)))

;; (defn yinyang []
;;   (letfn [(yin [x]
;;             (reset ((fn [cc] (print "@") cc)
;;                     (shift cc (cc x))))) 
;;           (yang [x]
;;             (reset ((fn [cc] (print "*") cc)
;;                     (shift cc (cc x)))))] 
;;     (yin yang)))

(defn yinyang []
  (let [id (fn [k] (k k))] 
    (id (fn [yin] (print "*")
          (id (fn [yang] (print "@") 
                (yin yang)))))))

;; (defn yinyang []
;;   (letfn [(id [k] (k k))
;;           (yin [] (id (fn [yin] (print "@") k)))
;;           (yang [] (id (fn [yang] (print "*") (yin yang))))]
;;     (trampoline yang)))

(defn f
  [return]
  (return 2)
  3)

(defn closure
  [f]
  (let [cont (atom nil)] 
    (reset
     (f (reset (shift x
                      (reset! cont x)
                      (x @cont)))))))
  
(deftest squares
  (let [squares (fn [n] 
                  (nth (map (closure #(* % %)) (range 11))
                       n))]
    (is (= (squares (range 11))
           (map #(* % %) (range 11))))))
