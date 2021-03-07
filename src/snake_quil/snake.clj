(ns snake-quil.snake
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(def grid-size 25)

(def dbstate {:matrix []
            :snake (list [10 10] [9 10] [8 10])
            :alive true
            :dir [1 0]
            :lifeTime 0
            :food [12 10]})

(defn setup []
  (q/frame-rate 3)
  (q/color-mode :rgb)
  (q/no-stroke)
  dbstate)

(defn new-head 
  "torna la nuova posizione della testa del serpente"
  [snake dir] 
  (vec (map + (first snake) dir)) )

(defn snake-move
  "muove il serpente, e torna un serpende lungo n+1"
  [snk dir]
  (let [newHead  (new-head snk dir)
        newSnake (conj snk newHead)]
    newSnake))

(defn snake-eating? [snake food]
  (= (first snake) food))

(defn get-random-point
  "torna una posizione casuale sulla scacchiera (da 1 a nr mattonelle)"
  [n]
  [(inc (rand-int n)) (inc (rand-int n))])


(defn food-move [snake] 
  (get-random-point grid-size))

(defn update-state [state]
  (let [snake2 (snake-move (:snake state) (:dir state))
        eating? (snake-eating? snake2 (:food state))]
    (assoc state :snake
           (if eating?
             snake2
             (drop-last snake2))
           :food (if eating? 
                   (food-move snake2)
                   (:food state)))))

(defn cell-size [] 
  (quot (q/width) grid-size))


(defn draw-cell [pos-v color]
  (let [cell-size (cell-size)
        x (* cell-size (first pos-v))
        y (* cell-size (second pos-v))]
    (apply q/fill color)
    (q/rect x y cell-size cell-size)))


(defn draw-state [state]
  (q/background 200)
  (draw-cell (:food state) [0 0 255])
  (doseq [[i v] (map-indexed vector (:snake state))]
    (let [cell-color (if (= i 0 ) [0 255 0] [0 0 0])]
      (draw-cell (vec v) cell-color))))

(defn key-pressed
  [state event]
  (let [result
        (case (:key event)
          :right  (if (not= [1 0] (:dir state))
                    (assoc state :dir [1 0])
                    state)
          :up (assoc state :dir [0 -1])
          :down (assoc state :dir [0 1])
          :left (assoc state :dir [-1 0])
          state)]
    (println (:dir result))
    result))

(comment 
  (q/defsketch snake
    :title "Game of life"
    :size [800 800]
  ; setup function called only once, during sketch initialization.
    :setup setup
  ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :key-pressed key-pressed
    :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
    :middleware [m/fun-mode]))
