(ns collider.core
  (:use overtone.live)
  (:require [clojure.math.combinatorics :as comb]
            [clojure.core.async :as async]))

(def cardinal-directions [:n :ne :e :se :s :sw :w :nw])

(defn random-direction []
  (get cardinal-directions (rand-int (count cardinal-directions))))

(defn create-grid-positions [w h]
  (vec (for [x (range w)
             y (range h)]
         [x y])))

(defn create-detector [id]
  {:id   id
   :kind :detector})

(defn create-detectors [n]
  (into [] (for [i (range n)]
             (create-detector i))))

(defn detector? [{:keys [kind]}]
  (= :detector kind))

(defn create-particle [n d e]
  {:kind      :particle
   :slice     n
   :direction d
   :energy    e})

(defn particle? [{:keys [kind]}]
  (= :particle kind))

(defn create-particles [n e-fn]
  (vec (for [s (range n)]
         (create-particle s (random-direction) (e-fn)))))

(defn create-empty-grid [w h]
  (vec (repeat w (vec (repeat h [])))))

(defn add-to-grid [grid [pos content]]
  (update-in grid pos (comp vec conj) content))

(defn grid-dimensions [grid]
  [(count grid) (count (first grid))])

(defn grid-content [grid x y]
  (let [objects (get-in grid [x y])
        count   (count objects)]
    [objects count]))

(defn populate-grid [grid contents]
  (let [[w h] (grid-dimensions grid)
        p (shuffle (create-grid-positions w h))]
    (reduce add-to-grid grid (partition 2 (interleave p contents)))))

(defn create-grid [w h s p]
  (let [particles (create-particles s (constantly 100))
        pylons    (create-detectors p)
        content   (into particles pylons)]
    (populate-grid (create-empty-grid w h) content)))

(defn process-grid [grid f c]
  (let [[w h] (grid-dimensions grid)]
    (reduce (fn [grid x]
              (reduce (fn [grid y]
                        (update-in grid [x y] f [x y] c)) grid (range h))) grid (range w))))

(defn align-particles [objects dir-map]
  (mapv (fn [{:keys [direction] :as object}]
          (if (particle? object)
            (assoc object :direction (direction dir-map))
            object)) objects))

(defn process-edge-cells [grid xrange yrange redirect c]
  "Process cells in a given x and y range representing a corner or edge of the grid and
  using the direction of the object decide if it's colliding with the boundary and change
  its direction accordingly."
  (reduce (fn [grid x]
            (reduce (fn [grid y]
                      (update-in grid [x y] align-particles redirect)) grid yrange)) grid xrange))

(defn process-edge-collisions [grid c]
  "Process particle objects colliding with the edge of the grid and update their direction
  accordingly."
  (let [[w h] (grid-dimensions grid)]
    (-> grid
        (process-edge-cells (range 0 1) (range 0 1) {:w :e :nw :se :n :s} c) ; nw
        (process-edge-cells (range (dec w) w) (range 0 1) {:e :w :ne :sw :n :s} c) ; ne
        (process-edge-cells (range 1 (- w 2)) (range 0 1) {:ne :se :n :s :nw :sw} c) ; n
        (process-edge-cells (range 0 1) (range (dec h) h) {:s :n :sw :ne :w :e} c) ; sw
        (process-edge-cells (range 0 1) (range 1 (- h 2)) {:sw :se :w :e :nw :ne} c) ; e
        (process-edge-cells (range (dec w) w) (range (dec h) h) {:s :n :se :nw :e :w} c) ;se
        (process-edge-cells (range (dec w) w) (range 1 (- h 2)) {:e :w :ne :nw :se :sw} c) ;w
        (process-edge-cells (range 1 (- w 2)) (range (dec h) h) {:se :ne :s :n :sw :nw} c)))) ;s

(defn react-with-detector [obj detector c]
  (let [used-e (rand-int (:energy obj))
        new-e  (- (:energy obj) used-e)]
    (async/go
      (async/>! c {:kind     :collision
                   :particle obj
                   :detector detector
                   :energy   used-e}))
    (println "detector " (:id detector) " with particle " (:slice obj) " energy used " used-e)
    (assoc obj :energy new-e)))

(defn react-with-particle [obj particle c]
  "For now we'll cheat and just used a random direction on collision"
  (let [used-e (rand-int (/ (:energy obj) 10))]
    (-> obj
        (assoc :energy (- (:energy obj) used-e))
        (assoc :direction (random-direction)))))

(defn react-with [obj others c]
  (reduce (fn [obj other]
            (if (detector? other)
              (react-with-detector obj other c)
              (react-with-particle obj other c))) obj others))

(defn collide-objects [objects c]
  (reduce (fn [coll o]
            (let [rest (filter (partial not= o) objects)]
              (if (detector? o)
                (conj coll o)
                (conj coll (react-with o rest c))))) [] objects))

(defn process-object-collisions [grid c]
  (process-grid grid (fn [objects [_ _] c]
                       (if (< (count objects) 2)
                         objects                            ; no collision with less than 2
                         (collide-objects objects c))) c))

(def movement-patterns                                      ; maps a relative position to a direction
  {[-1 -1] :se                                              ; heading from that position to take a
   [0 -1]  :s                                               ; particle to the home position
   [1 -1]  :sw
   [-1 0]  :e
   [1 0]   :w
   [-1 1]  :ne
   [0 1]   :n
   [1 1]   :nw})

(defn particles-moving-into [grid [x y] pattern]
  (let [[[dx dy] dir] pattern
        [j k] (map + [x y] [dx dy])]
    (filterv (every-pred #(= dir (:direction %)) particle?) (get-in grid [j k]))))

(defn next-step [grid c]
  (process-grid grid (fn [objects [x y] c]
                       (let [detectors (filterv detector? objects)]
                         (into detectors (flatten (for [pattern movement-patterns]
                                                    (particles-moving-into grid [x y] pattern)))))) c))

(defn fake-grid []
  (-> (create-grid 4 4 0 0)
      (add-to-grid [[0 0] (create-particle 99 :e 100)])
      (add-to-grid [[0 0] (create-detector :foo)])))

(defn run-grid [grid chan]
  (println "Running the grid")
  (-> grid
      (process-edge-collisions chan)
      (process-object-collisions chan)
      (next-step chan)))

(defn grid-runner [nome grid chan]
  (let [beat (nome)]
    (at (nome beat) (swap! grid run-grid chan))
    (apply-by (nome (inc beat)) grid-runner [nome grid chan])))

(comment
  (let [g (atom (create-grid 8 8 32 4))
        c (async/chan 10)
        t (async/timeout 30000)
        n (metronome 120)]
    (async/go
      (loop []
        (let [[v ch] (async/alts! [c t])]
          (if (= ch t)
            (println "Timeout")
            (do
              (println "Received " v)
              (recur))))))
    (grid-runner n g c)))
