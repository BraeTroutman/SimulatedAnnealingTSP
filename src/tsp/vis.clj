(ns tsp.vis
  (:gen-class)
  (:require [tsp.map :refer :all]
            [tsp.san :refer :all]
            [clojure.java.shell :refer [sh]]))

;; Set of functions for visualizing solutions over a given map by writing
;;   to an svg file, which can be opened in a web browser

(declare polyline)
(declare sltn->points)
(declare populate-map)
(declare circle)
(declare to-file)
(declare svg-text)
(declare name-map)

;; Write the solution of the given map to the svg file specified by name
(defn visualize-sltn
  "Creates an svg image of the map and the given solution, and saves it in
  the pwd as the file <name>.html, where <name> is the value of name passed
  (name should be a symbol or a string, eg 'brae or \"brae\")"
  [m sltn name]
  (let [cities (populate-map m sltn 3 'black 'red 0)
        path (polyline m sltn 'black 1)
        filename (str name ".html")]
    (to-file filename cities path)))

(defn name-map
  "return a string of svg objects representing the names of the cities in mp
  at the appropriate locations"
  [mp sltn]
  (loop [xs (map #(:x (get mp %)) sltn)
         ys (map #(:y (get mp %)) sltn)
         coll sltn
         acc ""]
    (cond (empty? xs) acc
          :else (recur (rest xs)
                       (rest ys)
                       (rest coll)
                       (str acc "\n" (svg-text (first coll) (first xs) (first ys)))))))

(defn svg-text
  "returns text and x and y as an string representing an svg text object"
  [text x y]
  (str "<text x=\"" x "\" y=\"" y "\" fill=\"black\">" text "</text>"))

;; take map and solution and return a polyline tracing the solution path
(defn polyline
  "Returns a string representing an svg style polyline with the given map,
  solution, stroke color, and stroke width"
  [mp sltn clr wdt]
  (let [points (sltn->points mp (cons (last sltn) sltn))]
    (str "<polyline points=\"" points "\" style=\"fill:none;stroke:" clr ";strokewidth:" wdt "\" />")))

;; return a svg circle from the given values
(defn circle
  "Returns a string representing an svg circle with the passed xy coords,
  radius, stroke color, fill color, and stroke width"
  [cx cy cr strk fill wdt]
  (str "<circle cx=\"" cx "\" cy=\"" cy "\" r=\"" cr "\" stroke=\"" strk "\" stroke-width=\"" wdt "\" fill=\"" fill "\" />"))

;; return a string of svg circles: one for every city
(defn populate-map
  "Returns a string of svg circles corresponding to the nodes in mpp
  contained in solution, with the passed properties applied"
  [mp sltn rad strk fill wdt]
  (loop [xs (map #(:x (get mp %)) sltn)
         ys (map #(:y (get mp %)) sltn)
         acc ""]
    (cond (empty? xs) acc
          :else (recur (rest xs)
                       (rest ys)
                       (str acc "\n" (circle (first xs) (first ys) rad strk fill wdt))))))
;; spit the svg map to the given file
(defn to-file
  "Writes the strings of svg data in cities and paths into a properly formatted
  svg file at the specified location in file"
  [file & objects]
  (spit file (str "<svg height=\"1000\" width=\"1000\">" (apply str objects) "\n</svg>\n")))

;; return a string of xy pairs from the sltn and map, e.g. "1,2 3,4 5,6 ..."
(defn sltn->points
  "returns an svg-style string of xy coords for the path of the given solution"
  [mp sltn]
  (let [xs (map #(:x (get mp %)) sltn)
        ys (map #(:y (get mp %)) sltn)]
    (loop [xs xs
           ys ys
           acc ""]
      (cond (empty? xs) acc
            :else (recur (rest xs)
                         (rest ys)
                         (str acc
                              " "
                              (first xs)
                              ","
                              (first ys)))))))
