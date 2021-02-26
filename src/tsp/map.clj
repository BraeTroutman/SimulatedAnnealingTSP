(ns tsp.map
  (:gen-class)
  (:require [clojure.java.io :refer [resource]]))

;; Generate a hashmap of xy coordinates from the map.txt file in resources

;; Declarations
(declare gen-map)
(declare make-map)
(declare add-node)
(declare expt)
(declare distance)
(declare map-from-file)

(defn gen-map
  "Generate a map of num-cities cities with random xy coords"
  [num-cities]
  (loop [i num-cities
         mp nil]
    (cond (= i 0) mp
          :else (recur (dec i)
                       (add-node mp i (rand-int 1000) (rand-int 1000))))))

;; Transform the list of nodes and coordinates into a map where each node is
;;   is the key to a sub-map holding the x and y coordinates
;;   eg (make-map '(["k" "1" "2"] ["j" "3" "4"] ["i" "5" "6"]))
;;      ==> {:k {:x 1 :y 2}, :j {:x 3 :y 4}, {:i {:x 5 :y 6}}}
(defn make-map
  "Transform the list of nodes into a hashmap of node-ids
  and coordinate pairs"
  [coord-list]
  (reduce (fn [graph-so-far node]
            (let [id (keyword (first node))
                  x  (Integer/parseInt (second node))
                  y  (Integer/parseInt (last node))]
              (add-node graph-so-far id x y)))
          nil coord-list))

;; Add a given node to the graph. Since our text-file input
;;   is specifically formatted, we needn't worry about repeat data
(defn add-node
  "Add the given node and x-y coordinates to the given map"
  [graph id x y]
  (assoc graph id (assoc {} :x x :y y)))

;; Raise a base to a power
(defn expt
  "Local power fn I made before I realized I could use Math/pow"
  [bas pow]
  (reduce * (repeat pow bas)))

;; Calculate distance between two nodes from their keys
(defn distance
  "Uses the distance formula to find the distance between the
  nodes in the passed map corresponding to the keywords in
  src and dst (expects src and dst to be keywords! eg :1 :2)"
  [map src dst]
  (let [x1 (:x (get map src))
        y1 (:y (get map src))
        x2 (:x (get map dst))
        y2 (:y (get map dst))]
    (Math/sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(defn map-from-file
  [input-file]
  (let [input-str (map #(clojure.string/split % #" ") (clojure.string/split (slurp input-file) #"\n"))]
    (loop [coll input-str
           acc nil]
      (cond (empty? coll) acc
            :else (let [name (first (first coll))
                        x (Float/parseFloat (second (first coll)))
                        y (Float/parseFloat (last (first coll)))]
                    (recur (rest coll)
                           (add-node acc name x y)))))))
