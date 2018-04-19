(ns delimc.test.examples
  (:require [delimc.core :refer :all]
            [clojure.test :refer :all]))

(def cc (atom nil))

(reset
 (fn [p]
   (shift k
          (reset! cc k)
          (k (p
              (fn [x]
                (shift k1 (p x))))))))

;; (defn cwcc
;;   "Scheme48 implementation of `call/cc` with shift/reset."
;;   [p]
;;   (shift k
;;          (k (p
;;              (fn [x]
;;                (shift k1 (p x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Yin Yang Puzzle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn id [x] (x x))

(reset
 (defn yin-yang-cc []
   (letfn [(cwcc [p]
             (shift k
                    (k (p
                        (fn [x]
                          (shift k1 (p x)))))))
           (yin [k] (print "*") k)
           (yang [k] (print "@") k)] 
     ((yin (cwcc id))
      (yang (cwcc id))))))

;; (defn yin-yang-cc []
;;   (let [yin ((fn [cc] (print "@") cc)
;;              (reset (cwcc id)))
;;         yang ((fn [cc] (print "*") cc)
;;               (reset (cwcc id)))]
;;     (yin yang)))

(defn yin-yang []
  (id (fn [yin]
        (print "*")
        (id (fn [yang]
              (print "@") 
              (yin yang))))))

(defn yin-yang-trampoline []
  (letfn [(yin [k] (print "*") (id k))
          (yang [k] (print "@") (id k))] 
    (trampoline yin yang)))

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

(defn yin-yang-cps []
  (providecc& (fn [yin]
                (print "@")
                (providecc& (fn [yang]
                              (print "*")
                              (yin yang))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Anaphora
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro aif
  "Paul Graham's anaphoric if from On-Lisp."
  [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       (do ~@body))))

(deftest nested-aif
  (is (= (aif 42
              (aif 38 [it it]))
         [42 38])))
;=> [38 38]

(deftest nested-aif2
  (is (= (aif 42
              [it (aif 38 it)])
         [42 38])))

(deftest nested-if-let
  (is (= (if-let [x 42]
           (if-let [y 38]
             [x y]))
         [42 38])))
