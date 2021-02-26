(ns tsp.san
  (:gen-class)
  (:require [tsp.map :as map]))

;; Declarations
(declare anneal)
(declare accept)
(declare accept-prob)
(declare fitness)
(declare greedy)
(declare nearest-neighbor)
(declare swap-consecutive)
(declare swap-random)
(declare permute-sublist)
(declare sublist)
(declare list-after-n)
(declare reverse-sublist)
(declare transport-sublist)

;; Contains the code dealing with the simulated annealing operations on
;; the map structure:
;; i.e. the fitness function, acceptance function, function for generating
;; an initial solution, and function for generating candidate solutions.
;; the rest are helper functions to these.

;; The main loop: sets the init sltn, and then tests candidates repeatedly
;;   while checking the number of iterations and temperature. If no number
;;   of iterations is specified it defaults to the temperature. Suggested
;;   cooling-fn #(* r %), where r is a decimal such that 0.88 <= r <= 0.99
(defn old-anneal
  ([mp init-temp cooling-fn]
  (loop [curr (greedy mp)  ; let the initial solution be the greedy solution
         temp init-temp   
         iter init-temp    ; the number of iterations is init-temp by default
         best (greedy mp)] ; keep track of global best sltn
    (let [new (swap-consecutive curr)] ; generate a candidate solution
      (cond (or (<= temp 0)
                (= iter 0)) best                                      ; if we've finished iterating, then return the global best
            :else (recur (accept mp new curr temp)                    ; decide whether to accept the new sltn
                         (cooling-fn temp)                            ; cool the temperature
                         (dec iter)                                   
                         (min-key #(fitness mp %) curr new best)))))) ; make best the least of all sltns we're looking at rn
  ([mp init-temp cooling-fn iter]                                     ; the same thing but we specify the number of iterations
  (loop [curr (shuffle (range (count mp)))
         temp init-temp
         iter iter
         best (greedy mp)]
    (let [new (swap-consecutive curr)]
      (cond (or (<= temp 0)
                (= iter 0)) best
            :else (recur (accept mp new best temp)
                         (cooling-fn temp)
                         (dec iter)
                         (min-key #(fitness mp %) curr new best)))))))

(defn temp-list
  [init-temp min-temp cooling-fn]
  (loop [t init-temp
         acc '()]
    (cond (< t min-temp) (reverse acc)
          :else (recur (cooling-fn t)
                       (cons t acc)))))

(defn anneal
  "The main annealing loop: accepts a hashmap of cities and their coords,
  the initial temperature for the system, a cooling function, and optionally
  the number of iterations. If no number of iterations is specified, then
  it is set by default to the initial temperature of the system."
  [mp init-temp min-temp cooling-fn niters]

  (println "With the given parameters, calculating a solution with simulated annealing will take about"
           (* 0.005 (count (temp-list init-temp min-temp cooling-fn)) niters (count mp)) "msecs")
  
  (loop [temp init-temp                   ; outer loop: loop while the temp is greater than the min temp
         best (greedy mp)]
    
    (cond (< temp min-temp) best          ; BASE CASE: Return the best solution
          :else (recur (cooling-fn temp)  ; RECURRENCE CASE: 

                       (loop [iter niters   ; inner loop: loop until the specified number of iterations is reached
                              curr best
                              best best]

                         (cond (= 0 iter) best ; BASE CASE: Return the best solution
                               :else (let [r (rand-int 3) ;RECURRENCE CASE: generate a candidate solution randomly from the three functions
                                           new (cond (= r 2) (swap-consecutive curr)
                                                     (= r 1) (transport-sublist curr)
                                                     :else (reverse-sublist curr))]

                                       (recur (dec iter) ; iter--
                                              (accept mp new curr temp) ; determine whether to accept the given solution or not (Metropolis Function)
                                              (min-key #(fitness mp %) curr new best))))))))) ; set best to the current best solution

;; Acceptance function: if the sltn is better than the current best, then
;;   accept it immediately. Otherwise look at the value of temp
(defn accept
  "Acceptance function for the annealing process. Returns the candidate
  solution 'sltn' if it is better than the solution in 'best'. Otherwise
  it decides which solution to return based on the accep-prob function."
  [map sltn best temp]
  (let [sfit (fitness map sltn)  ;sfit = fitness sltn
        bfit (fitness map best)] ;bfit = fitness best
    (cond (< sfit bfit) sltn                                       ; if the candidate solution is less than the best solution, then return it
          :else (if (< (rand 1) (accept-prob sfit bfit temp)) sltn ; otherwise, return the candidate with a probabilty based on the current energy of the system
                    best))))

(defn accept-prob
  "The acceptance probability of the given solution fitness sfit. Based on
  the difference btw sfit and bfit and the current temperature of the system."
  [sfit bfit temp]
  (let [prob (Math/exp (/ (- (Math/abs (- sfit bfit))) temp))] ; fn computes probability such that a high temp has a high prob of being chosen and lower energy has low
;    (println "san: " sfit " greedy: " bfit " prob: " prob " temp: " temp)
    prob))

;; Fitness Fn: calculate the total distance of the given sltn
(defn fitness
  "Determines the fitness of a given solution, ie the total distance of the
  route specified in said solution"
  [map sltn]
  (loop [dist 0
         coll (cons (last sltn) sltn)] ; make the sltn end with the node it began with
    (cond (empty? (rest coll)) dist    ; base case
          :else (recur (+ dist (map/distance map (first coll) (second coll))) ; recurrence case: compute again adding to distance
                       (rest coll)))))

;; Helper fn for greedy sltn
;;   finds the nearest neighbor to a given node
(defn nearest-neighbor
  "Determines the nearest neighbor to the node specified by ky (must be a keyword)"
  [mp ky]
  (apply min-key #(map/distance mp ky %) (map first (dissoc mp ky)))) ; find the node in map to which ky is closest

;; Create an initial solution, using the greedy technique of
;;   grabbing the nearest neighbor repeatedly
(defn greedy
  "Returns a the nearest-neighbor solution to the given map of cities"
  [mp]
  (loop [mp mp
         sltn (list (first (first mp)))] ; sltn starts with the first node in the map
    (cond (empty? (rest mp)) sltn        ; base case: return solution
          :else (let [ky (first sltn)              ; the key should be the most recent addition to the sltn
                      nb (nearest-neighbor mp ky)] ; find the nearest neighbor to the key
;                  (println sltn ky nb)
                  (recur (dissoc mp ky)            ; remove key from the map: it should't appear in the sltn more than once
                         (cons nb sltn))))))       ; add the chosen neighbor to the solution

;; Following are the set of functions used for determining a new
;;   candidate solution. I'm still deciding whether to choose the min
;;   of several candidate solutions or simply use one generator fn.

;; Swap a random pair of consecutive elements
;;   splits the list around the two elements chosen, swaps them, and then pastes it back together
(defn swap-consecutive
  "Returns the passed collection with two adjacent elements swapped"
  [coll]
  (let [n (rand-int (dec (count coll)))     ; pick a random integer less than the length of the list
        m (inc n)                           ; pick the integer following n
        i (nth coll n)                      ; the nth integer in the collection
        j (nth coll (inc n))                ; the mth integer in the collection
        left (take n coll)                  ; a list of every element in coll before n
        right (list-after-n coll m)]        ; a list of every element in coll after m
    (concat left (cons j (cons i right))))) ; put the list back together and return it

(defn swap-random
  "swap the locations of two random elements in the given collection"
  [coll]
  (let [n (rand-int (dec (count coll)))
        m (loop [m (rand-int (dec (count coll)))]
            (cond (not= m n) m
                  :else (recur (rand-int (dec (count coll))))))
        i (nth coll (min n m))
        j (nth coll (max n m))
        left (take (min n m) coll)
        right (list-after-n coll (max n m))
        middle (sublist coll (min n m) (max n m))]
    (concat left (concat (cons j middle) (cons i right)))))

(defn sublist
  "return a sublist of the given collection from start (inclusive) to end (exclusive)"
  [coll start end]
  (let [vect (into [] coll)]
    (cond (= start end) '()
          :else (apply list (subvec vect start end)))))

;; Helper for swap-consecutive: returns the remains of a list
;;   after the given index
(defn list-after-n
  "Returns a list of the remaining elements in coll after element n"
  [coll n]
  (loop [n (inc n)
         coll coll]
    (cond (= 0 n) coll
          :else (recur (dec n)
                       (rest coll)))))

(defn permute-sublist
  "shuffle a random sublist of the given collection"
  [coll]
  (let [size (count coll)
        n (rand-int size)
        m (rand-int size)
        sblist (sublist coll (min n m) (max n m))
        left (take (min n m) coll)
        right (list-after-n coll (dec (max n m)))]
    (concat left (shuffle sblist) right)))

(defn reverse-sublist
  "Reverse a random sublist of the given function"
  [coll]
  (let [size (count coll)
        n (rand-int (Math/ceil (/ size 2)))
        m (+ n (rand-int (Math/ceil (/ size 2))))
        sub (sublist coll n m)
        left (take n coll)
        right (list-after-n coll (dec m))]
    (concat left (reverse sub) right)))

(defn transport-sublist
  "move a random sublist of the given collection to a new location within the collection."
  [coll]
  (let [size (count coll)
        n (rand-int (Math/ceil (/ size 2)))
        m (+ n (rand-int (Math/ceil (/ size 2))))
        sub (sublist coll n m)
        left (take n coll)
        right (list-after-n coll (dec m))
        rem (concat left right)
        i (rand-int (count rem))
        l (take i rem)
        r (list-after-n rem (dec i))]
    (concat l sub r)))
