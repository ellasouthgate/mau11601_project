#lang htdp/isl+
;; TODO:
;; - restart after game over (e.g. press Enter to go back to the home screen)
;; - normal apples can spawn on top of the snake
;; - the rainbow apple can spawn on top of the snake or the normal apple
;; - two arrow keys pressed between moves can turn the snake back into itself
;; - high score
(require 2htdp/image)
(require 2htdp/universe)

(define CELLS 20)
(define SIZE 20)
(define WIDTH (* CELLS SIZE))
(define BG (empty-scene WIDTH WIDTH))
(define FOOD-SQ (square SIZE "solid" "red"))

;; Time is measured in ticks. big-bang's default clock runs at 28 ticks per second,
;; so every time below is a number of ticks, not seconds.
(define TICKS-PER-SECOND 28)
(define RAINBOW-CHANCE (* 10 TICKS-PER-SECOND)) ; 1 in 280 each tick, about once every 10 seconds
(define RAINBOW-LIFE (* 5 TICKS-PER-SECOND))    ; 140 ticks = 5 seconds

(define FOOD-POINTS 1)
(define RAINBOW-POINTS 5)

;; A World is one of:
;; - (make-home level skin)   the home screen
;; - (make-game snake dir food rainbow rainbow-ticks level skin score ticks)
;;                            a game in progress
;; level is "easy", "medium" or "hard"; skin is a colour name (never "red").
;; rainbow is a posn when the rainbow apple is on screen, otherwise #false.
;; rainbow-ticks is how many ticks the rainbow apple has left.
;; ticks counts every clock tick since the game started.
(define-struct home (level skin))
(define-struct game (snake dir food rainbow rainbow-ticks level skin score ticks))

(define START (make-home "medium" "green"))
(define START-SNAKE (list (make-posn 5 10) (make-posn 4 10) (make-posn 3 10)))
(define START-FOOD (make-posn 12 10))

;; ---------- Home screen ----------

;; (case isn't available in ISL+, so these "case" helpers use cond instead)
(define (next-skin s)
  (cond
    [(string=? s "green") "blue"]
    [(string=? s "blue") "purple"]
    [(string=? s "purple") "orange"]
    [else "green"]))

(check-expect (next-skin "green") "blue")
(check-expect (next-skin "blue") "purple")
(check-expect (next-skin "purple") "orange")
(check-expect (next-skin "orange") "green")

(define (start-game h)
  (make-game START-SNAKE "right" START-FOOD #false 0 (home-level h) (home-skin h) 0 0))

(check-expect (start-game (make-home "hard" "blue"))
              (make-game START-SNAKE "right" START-FOOD #false 0 "hard" "blue" 0 0))

(define (home-key h k)
  (cond
    [(key=? k "e") (make-home "easy" (home-skin h))]
    [(key=? k "m") (make-home "medium" (home-skin h))]
    [(key=? k "h") (make-home "hard" (home-skin h))]
    [(key=? k "c") (make-home (home-level h) (next-skin (home-skin h)))]
    [(key=? k "\r") (start-game h)]
    [else h]))

(check-expect (home-key START "e") (make-home "easy" "green"))
(check-expect (home-key START "h") (make-home "hard" "green"))
(check-expect (home-key (make-home "easy" "green") "m") (make-home "medium" "green"))
(check-expect (home-key START "c") (make-home "medium" "blue"))
(check-expect (home-key START "\r") (start-game START))
(check-expect (home-key START "x") START)
(check-expect (home-key START "up") START)

(define (draw-home h)
  (place-image
   (text "SNAKE" 48 "black") (/ WIDTH 2) 90
   (place-image
    (text (string-append "Difficulty: " (home-level h) "  (E / M / H)") 16 "black")
    (/ WIDTH 2) 190
    (place-image
     (text (string-append "Colour: " (home-skin h) "  (C to change)") 16 "black")
     185 230
     (place-image
      (square 16 "solid" (home-skin h)) 305 230
      (place-image
       (text "Press Enter to start" 20 "black") (/ WIDTH 2) 300
       BG))))))

;; ---------- Speed ----------

(define (ticks-per-move level)
  (cond
    [(string=? level "easy") 6]
    [(string=? level "medium") 4]
    [else 2])) ; "hard"

(check-expect (ticks-per-move "easy") 6)
(check-expect (ticks-per-move "medium") 4)
(check-expect (ticks-per-move "hard") 2)

(define (move-tick? g)
  (zero? (modulo (game-ticks g) (ticks-per-move (game-level g)))))

(check-expect (move-tick? (make-game START-SNAKE "right" START-FOOD #false 0 "medium" "green" 0 8)) #true)
(check-expect (move-tick? (make-game START-SNAKE "right" START-FOOD #false 0 "medium" "green" 0 9)) #false)
(check-expect (move-tick? (make-game START-SNAKE "right" START-FOOD #false 0 "easy" "green" 0 12)) #true)

(define (add-tick g)
  (make-game (game-snake g) (game-dir g) (game-food g) (game-rainbow g) (game-rainbow-ticks g)
             (game-level g) (game-skin g) (game-score g) (+ 1 (game-ticks g))))

(check-expect (add-tick (make-game START-SNAKE "right" START-FOOD #false 0 "easy" "green" 3 7))
              (make-game START-SNAKE "right" START-FOOD #false 0 "easy" "green" 3 8))

;; ---------- Moving the snake ----------

(define (grid->pixel n)
  (+ (* n SIZE) (/ SIZE 2)))

(check-expect (grid->pixel 0) 10)
(check-expect (grid->pixel 3) 70)

(define (next-head p dir)
  (cond
    [(equal? dir "up") (make-posn (posn-x p) (- (posn-y p) 1))]
    [(equal? dir "down") (make-posn (posn-x p) (+ (posn-y p) 1))]
    [(equal? dir "left") (make-posn (- (posn-x p) 1) (posn-y p))]
    [else (make-posn (+ (posn-x p) 1) (posn-y p))]))

(check-expect (next-head (make-posn 5 5) "up") (make-posn 5 4))
(check-expect (next-head (make-posn 5 5) "down") (make-posn 5 6))
(check-expect (next-head (make-posn 5 5) "left") (make-posn 4 5))
(check-expect (next-head (make-posn 5 5) "right") (make-posn 6 5))

(define (drop-last ps)
  (reverse (rest (reverse ps))))

(check-expect (drop-last (list 1 2 3)) (list 1 2))
(check-expect (drop-last (list 1)) empty)

(define (move-snake ps dir)
  (cons (next-head (first ps) dir) (drop-last ps)))

(check-expect (move-snake (list (make-posn 2 0) (make-posn 1 0)) "right")
              (list (make-posn 3 0) (make-posn 2 0)))

(define (grow-snake ps dir)
  (cons (next-head (first ps) dir) ps))

(check-expect (grow-snake (list (make-posn 2 0) (make-posn 1 0)) "right")
              (list (make-posn 3 0) (make-posn 2 0) (make-posn 1 0)))

(define (game-next-head g)
  (next-head (first (game-snake g)) (game-dir g)))

(check-expect (game-next-head (make-game START-SNAKE "right" START-FOOD #false 0 "easy" "green" 0 0))
              (make-posn 6 10))

(define (eats-food? g)
  (equal? (game-next-head g) (game-food g)))

(check-expect (eats-food? (make-game (list (make-posn 1 1)) "right" (make-posn 2 1) #false 0 "easy" "green" 0 0)) #true)
(check-expect (eats-food? (make-game (list (make-posn 1 1)) "up" (make-posn 2 1) #false 0 "easy" "green" 0 0)) #false)

(define (eats-rainbow? g)
  (and (posn? (game-rainbow g))
       (equal? (game-next-head g) (game-rainbow g))))

(check-expect (eats-rainbow? (make-game (list (make-posn 1 1)) "right" START-FOOD (make-posn 2 1) 50 "easy" "green" 0 0)) #true)
(check-expect (eats-rainbow? (make-game (list (make-posn 1 1)) "up" START-FOOD (make-posn 2 1) 50 "easy" "green" 0 0)) #false)
(check-expect (eats-rainbow? (make-game (list (make-posn 1 1)) "right" START-FOOD #false 0 "easy" "green" 0 0)) #false)

(define (random-cell n)
  (make-posn (random n) (random n)))

;; the snake grows, the score goes up 1 and the apple moves to new-food
(define (eat-food g new-food)
  (make-game (grow-snake (game-snake g) (game-dir g)) (game-dir g) new-food
             (game-rainbow g) (game-rainbow-ticks g) (game-level g) (game-skin g)
             (+ (game-score g) FOOD-POINTS) (game-ticks g)))

(check-expect (eat-food (make-game (list (make-posn 1 1)) "right" (make-posn 2 1) #false 0 "easy" "green" 3 6)
                        (make-posn 9 9))
              (make-game (list (make-posn 2 1) (make-posn 1 1)) "right" (make-posn 9 9) #false 0 "easy" "green" 4 6))

;; the snake grows, the score goes up 5 and the rainbow apple disappears
(define (eat-rainbow g)
  (make-game (grow-snake (game-snake g) (game-dir g)) (game-dir g) (game-food g)
             #false 0 (game-level g) (game-skin g)
             (+ (game-score g) RAINBOW-POINTS) (game-ticks g)))

(check-expect (eat-rainbow (make-game (list (make-posn 1 1)) "right" START-FOOD (make-posn 2 1) 50 "easy" "green" 3 6))
              (make-game (list (make-posn 2 1) (make-posn 1 1)) "right" START-FOOD #false 0 "easy" "green" 8 6))

(define (slide g)
  (make-game (move-snake (game-snake g) (game-dir g)) (game-dir g) (game-food g)
             (game-rainbow g) (game-rainbow-ticks g) (game-level g) (game-skin g)
             (game-score g) (game-ticks g)))

(check-expect (slide (make-game (list (make-posn 1 1)) "down" (make-posn 9 9) #false 0 "easy" "green" 0 6))
              (make-game (list (make-posn 1 2)) "down" (make-posn 9 9) #false 0 "easy" "green" 0 6))

(define (move g)
  (cond
    [(eats-food? g) (eat-food g (random-cell CELLS))]
    [(eats-rainbow? g) (eat-rainbow g)]
    [else (slide g)]))

(check-expect (move (make-game (list (make-posn 1 1)) "right" START-FOOD (make-posn 2 1) 50 "easy" "green" 0 6))
              (make-game (list (make-posn 2 1) (make-posn 1 1)) "right" START-FOOD #false 0 "easy" "green" 5 6))

;; only move on ticks that line up with the level's speed
(define (step g)
  (if (move-tick? g) (move g) g))

(check-expect (step (make-game (list (make-posn 1 1)) "down" (make-posn 9 9) #false 0 "easy" "green" 0 6))
              (make-game (list (make-posn 1 2)) "down" (make-posn 9 9) #false 0 "easy" "green" 0 6))
(check-expect (step (make-game (list (make-posn 1 1)) "down" (make-posn 9 9) #false 0 "easy" "green" 0 7))
              (make-game (list (make-posn 1 1)) "down" (make-posn 9 9) #false 0 "easy" "green" 0 7))

;; ---------- Rainbow apple ----------

(define (rainbow-colour t)
  (cond
    [(equal? (modulo t 6) 0) "red"]
    [(equal? (modulo t 6) 1) "orange"]
    [(equal? (modulo t 6) 2) "yellow"]
    [(equal? (modulo t 6) 3) "green"]
    [(equal? (modulo t 6) 4) "blue"]
    [else "purple"]))

(check-expect (rainbow-colour 0) "red")
(check-expect (rainbow-colour 1) "orange")
(check-expect (rainbow-colour 2) "yellow")
(check-expect (rainbow-colour 3) "green")
(check-expect (rainbow-colour 4) "blue")
(check-expect (rainbow-colour 5) "purple")
(check-expect (rainbow-colour 13) "orange")

(define (place-rainbow g p)
  (make-game (game-snake g) (game-dir g) (game-food g) p RAINBOW-LIFE
             (game-level g) (game-skin g) (game-score g) (game-ticks g)))

(check-expect (place-rainbow (make-game START-SNAKE "right" START-FOOD #false 0 "easy" "green" 0 6) (make-posn 3 3))
              (make-game START-SNAKE "right" START-FOOD (make-posn 3 3) 140 "easy" "green" 0 6))

;; one tick less for the rainbow apple; it disappears when the count hits 0
(define (countdown-rainbow g)
  (make-game (game-snake g) (game-dir g) (game-food g)
             (if (zero? (- (game-rainbow-ticks g) 1)) #false (game-rainbow g))
             (- (game-rainbow-ticks g) 1)
             (game-level g) (game-skin g) (game-score g) (game-ticks g)))

(check-expect (countdown-rainbow (make-game START-SNAKE "right" START-FOOD (make-posn 3 3) 50 "easy" "green" 0 6))
              (make-game START-SNAKE "right" START-FOOD (make-posn 3 3) 49 "easy" "green" 0 6))
(check-expect (countdown-rainbow (make-game START-SNAKE "right" START-FOOD (make-posn 3 3) 1 "easy" "green" 0 6))
              (make-game START-SNAKE "right" START-FOOD #false 0 "easy" "green" 0 6))

(define (maybe-spawn-rainbow g)
  (if (zero? (random RAINBOW-CHANCE))
      (place-rainbow g (random-cell CELLS))
      g))

;; runs every tick (not just move ticks) so the rainbow apple lasts 5 real seconds
(define (update-rainbow g)
  (if (posn? (game-rainbow g))
      (countdown-rainbow g)
      (maybe-spawn-rainbow g)))

(check-expect (update-rainbow (make-game START-SNAKE "right" START-FOOD (make-posn 3 3) 50 "easy" "green" 0 6))
              (make-game START-SNAKE "right" START-FOOD (make-posn 3 3) 49 "easy" "green" 0 6))

;; ---------- Keys ----------

(define (arrow? k)
  (or (key=? k "up") (key=? k "down") (key=? k "left") (key=? k "right")))

(check-expect (arrow? "up") #true)
(check-expect (arrow? "a") #false)

(define (opposite? d1 d2)
  (or (and (equal? d1 "up") (equal? d2 "down"))
      (and (equal? d1 "down") (equal? d2 "up"))
      (and (equal? d1 "left") (equal? d2 "right"))
      (and (equal? d1 "right") (equal? d2 "left"))))

(check-expect (opposite? "up" "down") #true)
(check-expect (opposite? "left" "right") #true)
(check-expect (opposite? "up" "left") #false)

(define (change-dir g k)
  (if (and (arrow? k) (not (opposite? k (game-dir g))))
      (make-game (game-snake g) k (game-food g) (game-rainbow g) (game-rainbow-ticks g)
                 (game-level g) (game-skin g) (game-score g) (game-ticks g))
      g))

(check-expect (change-dir (start-game START) "up")
              (make-game START-SNAKE "up" START-FOOD #false 0 "medium" "green" 0 0))
(check-expect (change-dir (start-game START) "left") (start-game START))
(check-expect (change-dir (start-game START) "x") (start-game START))

;; ---------- Game over ----------

(define (in-grid? p)
  (and (>= (posn-x p) 0) (< (posn-x p) CELLS)
       (>= (posn-y p) 0) (< (posn-y p) CELLS)))

(check-expect (in-grid? (make-posn 0 0)) #true)
(check-expect (in-grid? (make-posn 19 19)) #true)
(check-expect (in-grid? (make-posn 20 5)) #false)
(check-expect (in-grid? (make-posn 5 -1)) #false)

(define (hits-wall? ps)
  (not (in-grid? (first ps))))

(check-expect (hits-wall? (list (make-posn -1 3))) #true)
(check-expect (hits-wall? (list (make-posn 1 3))) #false)

(define (hits-self? ps)
  (ormap (lambda (q) (equal? q (first ps))) (rest ps)))

(check-expect (hits-self? (list (make-posn 1 1) (make-posn 2 1) (make-posn 1 1))) #true)
(check-expect (hits-self? (list (make-posn 1 1) (make-posn 2 1))) #false)

(define (game-over? g)
  (or (hits-wall? (game-snake g)) (hits-self? (game-snake g))))

(check-expect (game-over? (start-game START)) #false)
(check-expect (game-over? (make-game (list (make-posn 20 0)) "right" START-FOOD #false 0 "easy" "green" 0 0)) #true)

;; ---------- Drawing the game ----------

(define (draw-cell img p scene)
  (place-image img (grid->pixel (posn-x p)) (grid->pixel (posn-y p)) scene))

(define (draw-snake g scene)
  (foldl (lambda (p s) (draw-cell (square SIZE "solid" (game-skin g)) p s))
         scene
         (game-snake g)))

(define (draw-rainbow g scene)
  (if (posn? (game-rainbow g))
      (draw-cell (circle (/ SIZE 2) "solid" (rainbow-colour (game-ticks g)))
                 (game-rainbow g)
                 scene)
      scene))

(define (draw-score g scene)
  (place-image (text (string-append "Score: " (number->string (game-score g))) 16 "black")
               50 12
               scene))

(define (draw-game g)
  (draw-score g (draw-snake g (draw-rainbow g (draw-cell FOOD-SQ (game-food g) BG)))))

;; ---------- big-bang handlers ----------

(define (draw w)
  (cond
    [(home? w) (draw-home w)]
    [(game? w) (draw-game w)]))

(define (tick w)
  (cond
    [(home? w) w]
    [(game? w) (update-rainbow (step (add-tick w)))]))

(check-expect (tick START) START)

(define (key w k)
  (cond
    [(home? w) (home-key w k)]
    [(game? w) (change-dir w k)]))

(check-expect (key START "c") (make-home "medium" "blue"))
(check-expect (key (start-game START) "up") (change-dir (start-game START) "up"))

(define (stop? w)
  (cond
    [(home? w) #false]
    [(game? w) (game-over? w)]))

(check-expect (stop? START) #false)
(check-expect (stop? (start-game START)) #false)

(big-bang START (to-draw draw) (on-tick tick) (on-key key) (stop-when stop?))
