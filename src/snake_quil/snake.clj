(ns snake-quil.snake
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(def grid-size 25)

(def state {:matrix []
            :snake (list [10 10] [9 10] [8 10])
            :alive true
            :dir [1 0]
            :lifeTime 0
            :food [10 10]})

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :rgb)
  (q/no-stroke)
  state)

(defn get-neighbors [idx vec]
  [(get vec (dec (- idx grid-size)))
   (get vec (- idx grid-size))
   (get vec (inc (- idx grid-size)))

   (get vec (dec idx))
   (get vec (inc idx))

   (get vec (dec (+ grid-size idx)))
   (get vec (+ grid-size idx))
   (get vec (inc (+ grid-size idx)))])

(defn new-head [snake dir] 
  (vec (map + (first snake) dir)) )

(defn snake-move [snk]
           (conj snk (new-head snk (:dir state)) ))

(defn update-state [state]
  (assoc state :snake
         (snake-move (:snake  state))))

(defn cell-size [] 
  (quot (q/width) grid-size))

(defn draw-cell [pos-v color]
  (let [cell-size (cell-size)]
    (apply q/fill color)
    (q/rect (first pos-v) (second pos-v) cell-size cell-size)))


(defn draw-state [state]
  (q/background 200)
  (let [cell-size (cell-size)]
    (doseq [[i v] (map-indexed vector (:snake state))]
      (let [x (* cell-size (first v))
            y (* cell-size (second v))
            cell-color (if (= i 0 ) [0 255 0] [0 0 0])]
          (draw-cell [x y] cell-color)))))


(comment 
  (q/defsketch snake
  :title "Game of life"
  :size [800 800]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]))
