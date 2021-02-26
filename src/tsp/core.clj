(ns tsp.core
  (:gen-class)
  (:require [tsp.map :refer :all]
            [tsp.san :refer :all]
            [tsp.vis :refer :all]
            [clojure.java.io :refer [resource]]))

(declare -main)
(declare read-map)
(declare create-map)
(declare build-sltns)
(declare comp-sltns)
(declare help)
(declare visualize)

(def ^:private intro-string (slurp (resource "intro.txt")))

(defn- -main
  "Main repl for project: no need for the user to call.
  Simply the entry-point for the compiler"
  []
  (use 'tsp.core)
  (println intro-string)
  (create-map 15)
  (println "\nGenerating initial solutions for map of 15 cities...")
  (build-sltns)
  (comp-sltns)
  (loop []
    (print "\ntsp->> ") (flush)
    (let [in (read)]
      (cond (= in 'exit) nil
            :else (do (println (try (eval in)
                                    (catch Exception e)))
                      (recur))))))
(defn read-map
  "set m to a map read from filename. Filename should be a string."
  [filename]
  (def m (map-from-file filename)))

(defn create-map
  "Generate a new map with random nodes and set m to it.
  num specifies the number of nodes in the map."
  ([]
   (def m (gen-map 100)))
  ([num]
   (def m (gen-map num))))

(defn build-sltns
  "set variables g and s to greedy and annealed solutions respectively
  if no args are given, s is set to the annealed solution with the default
  init-temp: 100 cooling-fn: #(* 0.95 %) and iter: 10 Otherwise, if init-temp, cool-rate, and iter are specified then the annealing
  parameters are set to those."
  ([]
  (def g (greedy m))
   (def s (anneal m 100 0.5 #(* 0.95 %) 10)))
  ([init-temp cool-rate]
   (def g (greedy m))
   (def s (anneal m init-temp 0.5 #(* cool-rate %) 10)))
  ([init-temp min-temp cool-rate iter]
   (def g (greedy m))
   (def s (anneal m init-temp min-temp #(* cool-rate %) iter))))

(defn build
  "Builds a solution based on the value of the first argument
  if equal to 's, it will set global 's' to the result of applying
  the simulated annealing algorithm to the current global map. If
  'args' is specified, they are expected to be of the form
     <init-temp cooling-fn num-iterations>
  referring to the parameters for annealing. If the first parameter
  is equal to 'g, it will produce a greedy solution for the current
  global map set to 'm'"
 [global-to-set & args]
 (condp = global-to-set
   's (def s (apply anneal (if (empty? args) (list m 10000 #(* 0.95 %) 1000)
                               (cons m args))))
   'g (def g (greedy m))))

(defmacro nprn
  "executes the form without printing the return value"
  [form]
  `(do ~form nil))

(defn comp-sltns
  "Prints a comparison between the two solutions"
  []
  (let [gfit (fitness m g)
        sfit (fitness m s)]
    (println "Greedy:\t" gfit
             "\nSimAnn\t" sfit
             "\nThe annealing solution is better by" (* 100 (/ (- gfit sfit) gfit)) "%")))

(defn visualize
  "Outputs files visualizing the solutions to pwd. You can open them in your browser!"
  []
  (let [cities (populate-map m s 3 'black 'black 0)
        s-path (polyline m s 'blue 3)
        g-path (polyline m g 'red 1)
        names (name-map m s)
        file   "solutions.html"]
    (to-file file cities g-path s-path names)
    (to-file "greedy.html" cities g-path names)
    (to-file "simAnn.html" cities s-path names)))

(defn help
  "with 0 arity lists available functions
  with 1 arity lists the doc for the given function
  the function passed should be a symbol: e.g. 'anneal "
  ([]
   (println "The following functions are available to you throught this project: ")
   (doall (map #(println "\t" %) (keys (ns-publics 'tsp.core))))
   (println "For help on each function's arity and docstring, use (help <fn>)"))
  ([function-name]
   (let [metadata (meta (resolve function-name))]
     (println (:arglists metadata) "\n\t" (:doc metadata)))))
