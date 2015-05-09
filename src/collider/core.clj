(ns collider.core
  (:use overtone.live)
  (:require [clojure.math.combinatorics :as comb]
            [clojure.core.async :as async]
            [collider.buffer :refer [buffer-mix-to-mono]]))

; The grid model is a vector of vector of vectors. For a grid sized [w h]
; we will have w vectors representing columns each containing h vectors
; representing rows containing a vector of the objects located in that
; column:row. Each object will be represented by a map containing a value
; for the key :kind that determines if it is a :particle that moves or a
; :detector that detects the presence of particles and responds to them.

; Musically we respond to particles being detected by passing detector
; collisions to a core.async channel that can, e.g., play a sound. The
; grid processing can be driven by a metronome to have these things
; happen at a particular tempo.

(def cardinal-directions [:n :ne :e :se :s :sw :w :nw])

(defn random-direction []
  (get cardinal-directions (rand-int (count cardinal-directions))))

(defn create-grid-positions [w h]
  "Return a vector of grid coordinates [[0 0] .. [w-1 h-1]]."
  (vec (for [x (range w)
             y (range h)]
         [x y])))

(defn create-detector [id]
  "Create a detector map with an id."
  {:id   id
   :kind :detector})

(defn create-detectors [n]
  "Create a vector of detectors with ids (0..(n-1))."
  (into [] (for [i (range n)]
             (create-detector i))))

(defn detector? [{:keys [kind]}]
  (= :detector kind))

(defn create-particle [n d e]
  "Create a particle map with a slice number, a direction, and an
  amount of energy."
  {:kind      :particle
   :slice     n
   :direction d
   :energy    e})

(defn particle? [{:keys [kind]}]
  (= :particle kind))

(defn create-particles [n e-fn]
  "Create a vector of particles with slice numbers 0..(n-1) a random starting
  direction and energy returned by e-fn."
  (vec (for [s (range n)]
         (create-particle s (random-direction) (e-fn)))))

(defn create-empty-grid [w h]
  "Create an empty grid with is a vector of w columns, each column being a
   vector of h rows of empty content vectors."
  (vec (repeat w (vec (repeat h [])))))

(defn add-to-grid [grid [[x y] content]]
  "Return a new grid with content conj'd to the [x y] content vector."
  (update-in grid [x y] (comp vec conj) content))

(defn grid-dimensions [grid]
  "Return a vector [w h] containing the number of rows and columns of the grid."
  [(count grid) (count (first grid))])

(defn populate-grid [grid contents]
  "Given a grid and a sequence of contents add the contents at random location
  on the grid."
  (let [[w h] (grid-dimensions grid)
        p (shuffle (create-grid-positions w h))]
    (reduce add-to-grid grid (partition 2 (interleave p contents)))))

(defn create-grid [w h s p]
  "Create a wxh sized grid with s randomly placed particles and p randomly placed
  detectors."
  (let [particles (create-particles s (constantly 100))
        detectors (create-detectors p)
        content   (into particles detectors)]
    (populate-grid (create-empty-grid w h) content)))

(defn process-grid [grid f c]
  "Given a grid pass each position and its content to a function that returns the
  new content for that position. The function signature should be [[objects] [x y] c]
  where c is a core.async channel to which side-effects will be sent."
  (let [[w h] (grid-dimensions grid)]
    (reduce (fn [grid x]
              (reduce (fn [grid y]
                        (update-in grid [x y] f [x y] c)) grid (range h))) grid (range w))))

(defn align-particles [objects dir-map]
  "Given a sequence of objects and a map:direction->direction for each object that
  is a particle alter its direction based on a map lookup of its current direction."
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
  accordingly. Returns the updated grid."
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
  "Determine the reaction of an object with a detector. If the reaction has a side effect
  this is handed to the channel c. Returns the updated object."
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
  "Determine the reaction of an object with a particle. If the reaction has a side effect
  this is handed to the channel c. The current implementation cheats and has the rebound
  direction randomised rather than computing a direction based on current particle directions
  and/or energy levels. Returns the updated object."
  (let [used-e (rand-int (/ (:energy obj) 10))]
    (-> obj
        (assoc :energy (- (:energy obj) used-e))
        (assoc :direction (random-direction)))))

(defn react-with [obj others c]
  "Given an object react it with all other objects in its location passing any side effects
  to the channel c. Returns the updated object."
  (reduce (fn [obj other]
            (if (detector? other)
              (react-with-detector obj other c)
              (react-with-particle obj other c))) obj others))

(defn collide-objects [objects c]
  "Given a vector of objects return a new vector containing the objects updated after
  having been reacted with each other. Any side-effects are passed to the channel c."
  (reduce (fn [coll o]
            (let [rest (filter (partial not= o) objects)]
              (if (detector? o)
                (conj coll o)
                (conj coll (react-with o rest c))))) [] objects))

(defn process-object-collisions [grid c]
  "Return a new grid resulting from processing of any collisions between objects on
  the grid. Any side-effects of the collisions are passed to the chanenl c."
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
  "For a position [x y] on the grid determine which objects will move from neightbours of
  that position into the position. Return the new contents for [x y]."
  (let [[[dx dy] dir] pattern
        [j k] (map + [x y] [dx dy])]
    (filterv (every-pred #(= dir (:direction %)) particle?) (get-in grid [j k]))))

(defn next-step [grid c]
  "Given a grid return a new grid based on the movement of particles. Any side-effects are
  passed to the channel c."
  (process-grid grid (fn [objects [x y] c]
                       (let [detectors (filterv detector? objects)]
                         (into detectors (flatten (for [pattern movement-patterns]
                                                    (particles-moving-into grid [x y] pattern)))))) c))

(defn fake-grid []
  "Create a grid for testing."
  (-> (create-grid 4 4 0 0)
      (add-to-grid [[0 0] (create-particle 99 :e 100)])
      (add-to-grid [[0 0] (create-detector :foo)])))

(defn run-grid [grid chan]
  "Given a grid and a channel process all collisions passing side-effects to the channel
  chan, and return the next state of the grid."
  (-> grid
      (process-edge-collisions chan)
      (process-object-collisions chan)
      (next-step chan)))

(defn grid-runner [nome grid chan]
  "Given a metronome, a grid, and a channel sequence updating the grid on the metronome
  beat."
  (let [beat (nome)]
    (at (nome beat) (swap! grid run-grid chan))
    (apply-by (nome (inc beat)) grid-runner [nome grid chan])))

(definst grainer [b 0
                  center-pos 0
                  amp 0.25]
         (let [trate 10
               dur   (/ 2 trate)]
           (t-grains:ar 1 (impulse:ar trate) b 1.0 center-pos dur 0 0.4 2)))


(def sample-files ["~/Documents/Samples/Samplism/Memory Collection/MC01_all/Music/BD_chopsticks_end.wav"
                   "~/Documents/Samples/Samplism/Dead Piano/Kontakt/Dead Piano Nuances/detuned scrapes.wav"
                   "~/Documents/Samples/Cosmo D's Soundbank Vol. 2 Examples/loops/80-g-major-pizz-3.wav"
                   "~/Documents/Samples/RadioPhonica/Wav/Pulses/RP_Grunge_Drone.wav"])

(def slices 32)

(defn play [samplers {:keys           [energy]
                      {:keys [id]}    :detector
                      {:keys [slice]} :particle}]
  (let [sampler  (nth (:samplers samplers) id)
        buffer   (nth (:buffers samplers) id)
        duration (:duration (buffer-info buffer))
        position (* slice (/ duration slices))]
    (ctl sampler :center-pos position)
    (ctl sampler :amp (* 0.5 (/ energy 100)))))

(defn load-samplers []
  (let [buffers  (map (comp buffer-mix-to-mono load-sample) sample-files)
        samplers (map grainer buffers)]
    {:buffers  buffers
     :samplers samplers}))

(comment
  (let [grid (atom (create-grid 8 8 slices 4))
        chan (async/chan 10)
        tout (async/timeout 30000)
        nome (metronome 120)
        samp (load-samplers)]
    (async/go
      (loop [[val _] (async/alts! [chan tout])]
        (if val
          (do
            (play samp val)
            (recur (async/alts! [chan tout])))
          (stop))))
    (grid-runner nome grid chan)))
