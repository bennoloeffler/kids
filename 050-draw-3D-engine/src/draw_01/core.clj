(ns draw-01.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; Draws sphere at point [0 0 0] and 6 cubes around it.
; You can fly around this objects using navigation-3d.
; This draw function is fun-mode compatible (it takes state),
; though it's not used here, but we need fun-mode for navigation-3d.
(defn draw [state]
  (q/background 255)
  (q/lights)
  (q/fill 150 100 150)
  (q/sphere 75)
  (doseq [pos [[150 0 0] [-150 0 0]
               [0 150 0] [0 -150 0]
               [0 0 150] [0 0 -150]]]
    (q/with-translation pos
      (q/box 75))))

(q/defsketch my-sketch
  :draw draw
  :size [2000 2000]
  :renderer :p3d
  ; Enable navigation-3d.
  ; Note: it should be used together with fun-mode.
  :middleware [m/fun-mode m/navigation-3d])
