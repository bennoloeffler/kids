(ns draw-01.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [bel.vec :as v :refer [v]]
            [clojure.string :as str]))

; http://quil.info/api

(def size-x 1400)
(def size-y 1400)

; defining an atom like that collides with middleware m/navigation-2d
#_(def state (atom {:schritte []}))
             
  ; haus vom Nicolaus           
  #_ {:schritte [     [:staerke 0]
                      [:gehe 100] 
                      [:drehe 90]
                      [:gehe-s 300 700 200]                              
                      [:drehe-s -115 -75 400]

                      [:staerke 13]
                      [:gehe-s 190 210 45]
                      [:drehe-z -90 -95]
                      [:gehe-s 190 210 60]
                      [:drehe-s -85 -95 99]
                      [:gehe-s 190 210 34]
                      [:drehe 135]
                      [:gehe-z 135 145]
                      [:drehe-s 85 95 90]
                      [:gehe-s 135 145 180]
                      [:drehe 90]
                      [:gehe-s 270 290 40]
                      [:drehe 135]
                      [:gehe-s 180 220 80]
                      [:drehe 135]
                      [:gehe 280]

                      [:staerke 0]
                      [:drehe 135]
                      [:gehe 200]
                      [:drehe 180]
                      [:gehe-s 250 290 40]
                      [:drehe-s -5 15 20]
                      [:wdh 22 3]]}

; blume
(def state                              
            (atom {:schritte [[:staerke 0]
                              [:gehe 500] 
                              [:drehe 90]
                              [:gehe 700]
                              [:staerke 1]
                              [:drehe 190] 
                              [:gehe 600]; 320
                              [:wdh 2 50]
                              [:drehe -110]

                              [:farbe 199 67 43]
                              [:gehe-s 95 100 20]
                              [:drehe 190]
                              [:wdh 2 25]
                              [:drehe-s 79.8 80 300]
                              [:gehe 38]
                              [:wdh 5 20]
                              [:farbe 0 196 243]
                              [:wdh 12 15]                              
                              [:farbe 0 0 255]
                              [:staerke 10]]}))
                              ;[:drehe -6]
                              ;[:gehe 13]
                              ;[:wdh 2 60]
                              ;[:gehe 50]
                              ;[:drehe 45]
                              ;[:gehe 50]]}))


(defn schritt+ [schritt]
  (let [schritte (conj (:schritte @state) schritt)]
   (reset! state (assoc @state :schritte schritte))))

(defn d [winkel]
  (schritt+ [:drehe winkel]))

(defn dz [winkel-von winkel-bis]
  (schritt+ [:drehe-z winkel-von winkel-bis]))

(defn ds [winkel-von winkel-bis anzahl-bilder]
  (schritt+ [:drehe-s winkel-von winkel-bis anzahl-bilder]))

(defn g [weite]
  (schritt+ [:gehe weite]))

(defn gz [weite-von weite-bis]
  (schritt+ [:gehe-z weite-von weite-bis]))

(defn gs [weite-von weite-bis anzahl-bilder]
  (schritt+ [:gehe-s weite-von weite-bis anzahl-bilder]))

(defn s [staerke]
  (schritt+ [:staerke staerke]))

(defn f [r g b]
  (schritt+ [:farbe r g b]))

(defn w [welche wie-oft]
  (schritt+ [:wdh welche wie-oft]))

(defn u []
   (reset! state (update-in @state [:schritte] (comp vec drop-last))))

(defn r []
  (reset! state (update-in @state [:schritte] {})))

(defn draw-arrow [x y dx dy]
  (let [v         (v x y (+ x dx) (+ y dy))
        l         (v/len v)
        a         (v/pi-angle v)
        arr-len   45
        arr-width 25]
    (q/stroke-weight 8)
    (q/translate x y)
    (q/rotate (+ (- a) v/pi-half))
    (q/fill (q/current-stroke))
    (q/triangle l             0 
                (- l arr-len) (-> arr-width (/ 2) -)
                (- l arr-len) (-> arr-width (/ 2)))
    (q/line 0 0 l 0)))


(defn _setup []
  (q/frame-rate 30)
  (q/background 240)
  
  ; Pendel
  {:schritte [[:staerke 0]
              [:gehe 700]

              [:staerke 20]
              [:farbe 0 0 200]
              [:drehe-s 70 110 160]
              [:gehe-s 400 700 200]
              
              [:farbe 200 0 0]
              [:drehe-s -135 135 77]
              [:drehe-s -135 135 130]
              [:gehe-s 200 600 333]
              
              [:farbe 0 170 0]
              [:drehe-s -170 170 39]
              [:gehe-s 100 800 166]
              
              [:drehe 90]
              [:staerke 10]
              [:farbe 40 165 239]
              [:gehe 20]
              [:drehe -20]              
              [:wdh 2 15]]})


(defn setup []
  (q/frame-rate 30)
  (q/background 240)
  #_@state

  ; Blume
  {:schritte[
             [:staerke 0]
             [:gehe 500]
             [:drehe 90]
             [:gehe 700]
             [:staerke 1]
             [:drehe 190]
             [:gehe 600]; 320
             [:wdh 2 50]
             [:drehe -110]
             
             [:farbe 199 67 43]
             [:gehe-s 95 100 20]
             [:drehe 190]
             [:wdh 2 25]
             [:drehe-s 79.8 80 300]
             [:gehe 38]
             [:wdh 5 20]
             [:farbe 0 196 243]
             [:wdh 12 15]
             [:farbe 0 0 255]
             [:staerke 10]]}
  
  ; haus vom Nicolaus           
  #_{:schritte []   
              [:staerke 0]
              [:gehe 100] 
              [:drehe 90]
              [:gehe-s 300 700 200]                              
              [:drehe-s -115 -75 400]
              
              [:staerke 13]
              [:gehe-s 190 210 45]
              [:drehe-s -90 -95 300]
              [:gehe-s 190 210 60]
              [:drehe-s -85 -95 99]
              [:gehe-s 190 210 34]
              [:drehe 135]
              [:gehe-s 135 155 30]
              [:drehe-s 85 95 90]
              [:gehe-s 135 145 180]
              [:drehe 90]
              [:gehe-s 270 290 40]
              [:drehe 135]
              [:gehe-s 180 220 80]
              [:drehe 135]
              [:gehe 280]
              
              [:staerke 0]
              [:drehe 135]
              [:gehe 200]
              [:drehe 180]
              [:gehe-s 250 290 40]
              [:drehe-s -5 15 20]
              [:wdh 22 3]})
            

(defn update-state [q-state]
  #_@state
  ;; TODO add the elements from user here
  q-state)

    
(defn schwinge [von bis frames-2pi current-frame]
  ;(println "schwinge" von bis frames-2pi current-frame)
  (let [x (/ (* 2 Math/PI current-frame) frames-2pi)
        sin-val+1 (+ 1 (q/sin x))
        diff (- bis von)]
    (+ (/ (* sin-val+1 diff) 2) von)))    
    

(defn test-schwinge []
  (let [von -20 
        bis 30
        zyklus 20]
     (for [f (range 100)]
        (schwinge von bis zyklus f))))

;(test-schwinge)


(defn execute [[c p1 p2 p3 :as schritt]]
  ;(println schritt)
  (let [s #(schwinge p1 p2 p3 (q/frame-count))
        r #(+ p1 (rand-int (- p2 p1)))]
    (case c
      :drehe (q/rotate (v/grad-to-pi p1))
      :drehe-z (q/rotate (v/grad-to-pi (r)))
      :drehe-s (q/rotate (v/grad-to-pi (s)))
      :gehe (do (q/line 0 0 p1 0) 
                (q/translate p1 0))
      :gehe-z (do (q/line 0 0 (r) 0)
                  (q/translate (r) 0))
      :gehe-s (do (q/line 0 0 (s) 0)
                  (q/translate (s) 0))
      :staerke (q/stroke-weight p1)
      :farbe (q/stroke p1 p2 p3)
      (throw (Exception. (str "FEHLER: Schritt unbekannt: " schritt))))))


(defn execute-sequence [todos]
  ;(println todos)
  (loop [remaining-todos todos
         done-todos      []
         loop-todos      todos
         loop-counter    0
         stack           '()
         instr-counter   0]
    ;(println "========================================")
    ;(println "remain:  " remaining-todos)
    ;(println "done:    " done-todos)
    ;(println "l-count: " loop-counter " -> " loop-todos)
    ;(println "stack:   " stack)
    (let [todo            (first remaining-todos)
          remaining-todos (rest remaining-todos)
          rep?            (= :wdh (first todo))
          indent          (str/join (repeat (count stack) "   "))]
      (if (seq todo)
        (if rep?
          (let [num-loops  (get todo 2)
                num-instr  (get todo 1)
                loop-instr (take-last num-instr done-todos)]
            ;(println indent "START-LOOP: " todo " " (take-last rep-instr done-todos))
            (recur loop-instr
                   [] 
                   loop-instr 
                   (dec num-loops) 
                   (conj stack [remaining-todos (conj done-todos todo) loop-todos loop-counter])
                   instr-counter))
          (do ;(println indent "*** DO #" (inc instr-counter) " : " todo)
            (execute todo)
            (recur remaining-todos 
                   (conj done-todos todo) 
                   loop-todos 
                   loop-counter 
                   stack
                   (inc instr-counter))))
        (if (zero? loop-counter)
          (when (seq stack)
            (let [[s-todos s-done loop-todos loop-counter] (first stack)]                   
              ;(println indent "END-LOOP")
              (recur s-todos 
                     s-done 
                     loop-todos 
                     loop-counter 
                     (drop 1 stack)
                     instr-counter)))
            ;(println indent "END #" instr-counter))
          (do ;(println indent "LOOP-AGAIN: " loop-counter)
            (recur loop-todos 
                   [] 
                   loop-todos 
                   (dec loop-counter) 
                   stack
                   instr-counter)))))))

(comment
  (execute-sequence nil)
  (execute-sequence [])
  (execute-sequence [[:staerke 1]])
  (execute-sequence [[:staerke 1] [:staerke 2]])
  (execute-sequence [[:staerke 2] [:wdh 1 3]])
  (execute-sequence [[:staerke 1] [:staerke 2] [:wdh 2 3]])
  (execute-sequence [[:staerke 1] [:staerke 2] [:wdh 2 3] [:staerke 3]])
  (execute-sequence [[:staerke 1] [:staerke 2] [:wdh 2 1] [:staerke 3] [:wdh 1 5]])
  (execute-sequence [[:staerke 1] [:staerke 2] [:wdh 1 1] [:staerke 3] [:wdh 1 2] [:staerke 4]])
  (execute-sequence [[:1] [:2] [:wdh 1 2] [:3] [:wdh 4 1] [:4]])
  (execute-sequence [[:a] [:wdh 1 1] [:wdh 2 4] [:wdh 3 9] [:o]]) ; 101
  (execute-sequence [[:a] [:wdh 1 1] [:wdh 2 4] [:wdh 3 9] [:wdh 2 3][:o]])) ; empty loop :wdh 2 3


(defn draw-state [state]
 (q/background 256)
 (q/stroke 0)
 (execute-sequence (:schritte state))
 (draw-arrow 0 0 100 0))
  

(q/defsketch draw-01
  :title "(s 1) (g 300) (d 90) (g 300) (f 12 76 255) (s 3) (g 100) (d 200) (w 100 2)"
  :size [size-x size-y]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]; 
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode m/navigation-2d])
