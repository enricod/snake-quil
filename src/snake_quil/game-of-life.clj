(ns snake-quil.game-of-life
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(def grid-size 20)

(def state {:matrix (vec
                     (repeatedly (* grid-size grid-size) #(rand-int 2)))})

(defn setup []
  (q/frame-rate 10)
  (q/color-mode :hsb)
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

(defn new-status [idx itm vec]
  (let [alive-n (get (frequencies (get-neighbors idx vec)) 1 0)]
    (if (= 1 itm)
      (if (or (> alive-n 3) (< alive-n 2)) 0 1)
      (if (= 3 alive-n) 1 0))))

(defn update-state [state]
  (assoc state :matrix
         (vec
          (map-indexed
           (fn [idx itm] (new-status idx itm (:matrix state)))
           (:matrix state)))))

(defn draw-state [state]
  (q/background 240)
  (let [cell-size (quot (q/width) grid-size)]
    (doseq [[i v] (map-indexed vector (:matrix state))]
      (let [multiplier (int (/ i grid-size))
            x (* cell-size (- i (* multiplier grid-size)))
            y (* cell-size multiplier)]
        (q/fill
         (if (= 1 v) 0 255))
        (q/rect x y cell-size cell-size)))))



(q/defsketch game-of-live
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
  :middleware [m/fun-mode])
