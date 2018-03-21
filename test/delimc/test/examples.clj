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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Yin Yang Puzzle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn yin-yang []
;;   (let [yin ((fn [cc] (print "@") cc)
;;              (cwcc (fn [x] x)))
;;         yang ((fn [cc] (print "*") cc)
;;               (cwcc (fn [x] x)))]
;;     (yin yang)))

;; (defn yin-yang []
;;   (letfn [(yin [x]
;;             (reset ((fn [cc] (print "@") cc)
;;                     (shift cc (cc x))))) 
;;           (yang [x]
;;             (reset ((fn [cc] (print "*") cc)
;;                     (shift cc (cc x)))))] 
;;     (yin yang)))

(defn yin-yang []
  (let [id (fn [k] (k k))] 
    (id (fn [yin]
          (print "*")
          (id (fn [yang]
                (print "@") 
                (yin yang)))))))

;; (defn yin-yang []
;;   (letfn [(id [k] (k k))
;;           (yin [k] (print "*") (id k))
;;           (yang [k] (print "@") (id k))] 
;;     (trampoline (yin yang))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CPS Transform
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn I& [x k]
  (k x))

(defn N& [x k]
  (print "@")
  (k x))

(defn A& [x k]
  (print "*")
  (k x))

(defn cwcc& [f& k]
  (f& k k))

(defn yin-yang&
  "CPS Yin Yang"
  [k]
  (cwcc& I& (fn [cont1]
              (N& cont1 (fn [yin]
                          (cwcc I& (fn [cont2]
                                     (A& cont2 (fn [yang]
                                                 (k (yin yang)))))))))))

(defn providecc&
  "Same as `(fn [k] (k k))`"
  [k]
  (cwcc& I& k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Closure Conversion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro map-closure
  [closure env]
  (let [bound (second closure)]
    (println bound)
    (list* (first '~closure)
            (conj bound ~env) 
            (loop [exp# (first (nnext '~closure))
                   env# ~env]
              (conj (first exp#) 
                    (map (fn [free# body#]
                           (cond
                             (and (symbol? body#)
                                  (empty? (filter #(not= % body#) bound))) free#
                             (list? body#) (recur body# env#)
                             :else body#))
                         env#
                         (next exp#)))))))

(defn map-closure2
  [closure env]
  (let [bound (second closure)]
    (list* (first closure)
           (conj bound env) 
           (loop [exp (first (nnext closure))
                  env env]
             (conj (first exp)
                   (map (fn [free body]
                          (cond
                            (and (symbol? body)
                                 (empty? (filter #(= % body) bound))) free
                            (list? body) (recur body env)
                            :else body))
                        env
                        (next exp)))))))

