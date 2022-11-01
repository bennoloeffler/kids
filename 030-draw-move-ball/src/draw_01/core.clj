(ns draw-01.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [bel.vec :as v]))

(def size-x 2000)
(def size-y 2000)

(defn draw-arrow [x y dx dy]
  (let [v         (v/v x y (+ x dx) (+ y dy))
        l         (v/len v)
        a         (v/pi-angle v)
        arr-len   45
        arr-width 25]
    (q/translate x y)
    (q/rotate (+ (- a) v/pi-half))
    (q/fill 220)
    (q/triangle l             0 
                (- l arr-len) (-> arr-width (/ 2) -)
                (- l arr-len) (-> arr-width (/ 2)))
    (q/fill 55)
    (q/line 0 0 l 0)))
    


(defn setup []

  (q/frame-rate 30)

  {:speed-x 24
   :speed-y 41
   :x (/ size-x 2)
   :y (/ size-y 2)
   :size 150})

(defn dir [key]
  (case key
    :w :up
    :d :right
    :s :down
    :a :left
    key))

(dir :w)

(defn update-state [state]
  ; speed and location
  (let [kw (if (q/key-pressed?) (q/key-as-keyword) nil)
        x (:x state)
        y (:y state)
        sx (:speed-x state)
        sy (:speed-y state)
        s (:size state)
        ;s (+ s (- (rand-int 11) 5))
        ;s (max s 5)
        test-x (+ x sx)
        test-y (+ y sy)
        sx (if (< 0 test-x size-x) sx (- (* 0.6 sx)))
        sy (if (< 0 test-y size-y) sy (- (* 0.6 sy)))
        x (+ x sx)
        y (+ y sy)
        sx+ (cond (= (dir kw) :left) -0.5 (= (dir kw) :right) 0.5 :else 0)
        sy+ (cond (= (dir kw) :up) -0.5 (= (dir kw) :down) 0.5 :else 0)
        s* (cond (= kw :space) 0.9 (= kw :+) 1.01 (= kw :-) 0.99 :else 1)]
   {:x x :y y :speed-x (* (+ sx sx+) s*) :speed-y (* (+ sy sy+) s*) :size s}))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill 100 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [s (:size state)
        x (:x state)
        y (:y state)]
      ; Draw the circle.
      (q/ellipse x y s s)
      (q/fill 10 10 10)
      (q/text-size 50)
      (doseq [[ind capt fn] [[0 "key-as-keyword" q/key-as-keyword]
                             [1 "key-code" q/key-code]
                             [2 "key-coded?" (fn* [] (q/key-coded? (q/raw-key)))]
                             [3 "key-pressed?" q/key-pressed?] 
                             [4 "raw-key" q/raw-key]
                             [5 "key-modifiers" q/key-modifiers]]]
        (q/text (str capt " " (fn)) 30 (+ (* 50 ind) 50)))
      (q/text (str "X: " (int x)) 30 400)
      (q/text (str "Y: " (int y)) 30 450)
      (q/text (str "VX: " (int (:speed-x state))) 30 550)
      (q/text (str "VY: " (int (:speed-y state))) 30 600)
      (draw-arrow x y (* 10 (:speed-x state)) (* 10 (:speed-y state)))))

(q/defsketch draw-01
  :title "You spin my circle right round"
  :size [size-x size-y]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features []; :keep-on-top
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
