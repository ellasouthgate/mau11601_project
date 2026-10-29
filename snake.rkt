#lang htdp/isl+
;; TODO:
;; - restart after game over (e.g. press Enter to go back to the home screen)
;; - normal apples can spawn on top of the snake
;; - the rainbow apple can spawn on top of the snake or the normal apple
;; - two arrow keys pressed between moves can turn the snake back into itself
;; - high score
;; - a "GAME OVER" screen (the last frame just freezes, with the head on the border)
;; - the HUD text is centred on fixed points, so "SCORE 125" grows both ways
;;   instead of staying lined up on the left
;; - the checkerboard is brighter than intended: "darkslategray" is the darkest
;;   grey/green in the colour list
(require 2htdp/image)
(require 2htdp/universe)

(define CELLS 20)
(define SIZE 20)
(define HUD-H 40)                          ; height of the score strip at the top
(define SCENE-W (* (+ CELLS 2) SIZE))      ; the grid plus a one-cell border each side
(define SCENE-H (+ HUD-H SCENE-W))
(define PLAY-X SIZE)                       ; pixel where grid cell 0 starts
(define PLAY-Y (+ HUD-H SIZE))

;; ---------- Palette ----------
;; every colour is a name from the 2htdp/image colour list

(define bg-colour "black")
(define grid-colour "darkslategray")       ; the other checkerboard colour
(define border-colour "forestgreen")
(define border-edge-colour "darkgreen")
(define hud-colour "darkgreen")
(define hud-line-colour "lime")
(define hud-text-colour "gold")
(define title-colour "lime")
(define text-colour "gold")
(define eye-colour "black")
(define apple-colour "red")
(define stalk-colour "limegreen")
(define clear-colour "transparent")

;; Time is measured in ticks. big-bang's default clock runs at 28 ticks per second,
;; so every time below is a number of ticks, not seconds.
(define TICKS-PER-SECOND 28)
(define RAINBOW-CHANCE (* 10 TICKS-PER-SECOND)) ; 1 in 280 each tick, about once every 10 seconds
(define RAINBOW-LIFE (* 5 TICKS-PER-SECOND))    ; 140 ticks = 5 seconds

(define FOOD-POINTS 1)
(define RAINBOW-POINTS 5)

;; A World is one of:
;; - (make-home level skin ticks)
;;                            the home screen; ticks counts clock ticks, for blinking text
;; - (make-game snake dir food rainbow rainbow-ticks level skin score ticks)
;;                            a game in progress
;; level is "easy", "medium" or "hard"; skin is a colour name (never "red").
;; rainbow is a posn when the rainbow apple is on screen, otherwise #false.
;; rainbow-ticks is how many ticks the rainbow apple has left.
;; ticks counts every clock tick since the game started.
(define-struct home (level skin ticks))
(define-struct game (snake dir food rainbow rainbow-ticks level skin score ticks))

(define START (make-home "medium" "green" 0))
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

(check-expect (start-game (make-home "hard" "blue" 0))
              (make-game START-SNAKE "right" START-FOOD #false 0 "hard" "blue" 0 0))

(define (home-key h k)
  (cond
    [(key=? k "e") (make-home "easy" (home-skin h) (home-ticks h))]
    [(key=? k "m") (make-home "medium" (home-skin h) (home-ticks h))]
    [(key=? k "h") (make-home "hard" (home-skin h) (home-ticks h))]
    [(key=? k "c") (make-home (home-level h) (next-skin (home-skin h)) (home-ticks h))]
    [(key=? k "\r") (start-game h)]
    [else h]))

(check-expect (home-key START "e") (make-home "easy" "green" 0))
(check-expect (home-key START "h") (make-home "hard" "green" 0))
(check-expect (home-key (make-home "easy" "green" 9) "m") (make-home "medium" "green" 9))
(check-expect (home-key START "c") (make-home "medium" "blue" 0))
(check-expect (home-key START "\r") (start-game START))
(check-expect (home-key START "x") START)
(check-expect (home-key START "up") START)

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

;; ---------- Drawing: background ----------
;; drawn once into constants, like the fixed background in the eclipse and dial examples

;; places img in the middle of grid cell p (the border cells are -1 and CELLS)
(define (draw-cell img p scene)
  (place-image img
               (+ PLAY-X (grid->pixel (posn-x p)))
               (+ PLAY-Y (grid->pixel (posn-y p)))
               scene))

(define (grid-cell-colour x y)
  (if (even? (+ x y)) bg-colour grid-colour))

(check-expect (grid-cell-colour 0 0) bg-colour)
(check-expect (grid-cell-colour 1 0) grid-colour)
(check-expect (grid-cell-colour 3 5) bg-colour)

(define (draw-grid-row y scene)
  (foldl (lambda (x s) (draw-cell (square SIZE "solid" (grid-cell-colour x y)) (make-posn x y) s))
         scene
         (range 0 CELLS 1)))

(define (draw-grid scene)
  (foldl draw-grid-row scene (range 0 CELLS 1)))

(define BRICK (overlay (square (- SIZE 4) "solid" border-colour)
                       (square SIZE "solid" border-edge-colour)))

;; one brick on each of the four edges, i cells along
(define (draw-border-at i scene)
  (draw-cell BRICK (make-posn i -1)
             (draw-cell BRICK (make-posn i CELLS)
                        (draw-cell BRICK (make-posn -1 i)
                                   (draw-cell BRICK (make-posn CELLS i) scene)))))

(define (draw-border scene)
  (foldl draw-border-at scene (range -1 (+ CELLS 1) 1)))

;; black scene with the border, shared by the home screen and the game
(define FRAME (draw-border (empty-scene SCENE-W SCENE-H bg-colour)))

(define (draw-hud-strip scene)
  (place-image (line SCENE-W 0 hud-line-colour) (/ SCENE-W 2) (- HUD-H 2)
               (place-image (rectangle SCENE-W HUD-H "solid" hud-colour)
                            (/ SCENE-W 2) (/ HUD-H 2)
                            scene)))

(define BACKGROUND (draw-hud-strip (draw-grid FRAME)))

;; ---------- Drawing the game ----------

;; the head is a darker shade of the skin
(define (head-colour skin)
  (cond
    [(string=? skin "green") "darkgreen"]
    [(string=? skin "blue") "darkblue"]
    [(string=? skin "purple") "darkpurple"]
    [else "orangered"])) ; "orange"

(check-expect (head-colour "green") "darkgreen")
(check-expect (head-colour "blue") "darkblue")
(check-expect (head-colour "purple") "darkpurple")
(check-expect (head-colour "orange") "orangered")

;; a block of skin on a darker outline; the outline is smaller than the cell,
;; so a thin gap shows between segments
(define (draw-segment skin p scene)
  (draw-cell (overlay (square (- SIZE 6) "solid" skin)
                      (square (- SIZE 2) "solid" (head-colour skin)))
             p
             scene))

;; where the eyes go on the 18x18 head, at the front for each direction
(define (eye-posns dir)
  (cond
    [(string=? dir "up") (list (make-posn 5 5) (make-posn 13 5))]
    [(string=? dir "down") (list (make-posn 5 13) (make-posn 13 13))]
    [(string=? dir "left") (list (make-posn 5 5) (make-posn 5 13))]
    [else (list (make-posn 13 5) (make-posn 13 13))])) ; "right"

(check-expect (eye-posns "up") (list (make-posn 5 5) (make-posn 13 5)))
(check-expect (eye-posns "down") (list (make-posn 5 13) (make-posn 13 13)))
(check-expect (eye-posns "left") (list (make-posn 5 5) (make-posn 5 13)))
(check-expect (eye-posns "right") (list (make-posn 13 5) (make-posn 13 13)))

(define (draw-head skin dir p scene)
  (draw-cell (foldl (lambda (e img) (place-image (square 4 "solid" eye-colour) (posn-x e) (posn-y e) img))
                    (overlay (square (- SIZE 4) "solid" (head-colour skin))
                             (square (- SIZE 2) "solid" skin))
                    (eye-posns dir))
             p
             scene))

(define (draw-snake g scene)
  (draw-head (game-skin g) (game-dir g) (first (game-snake g))
             (foldl (lambda (p s) (draw-segment (game-skin g) p s))
                    scene
                    (rest (game-snake g)))))

;; a red apple with a stalk, on a see-through cell so the stalk can poke out the top
(define (draw-apple p scene)
  (draw-cell (place-image (rectangle 2 5 "solid" stalk-colour) 10 3
                          (place-image (circle 7 "solid" apple-colour) 10 12
                                       (square SIZE "solid" clear-colour)))
             p
             scene))

;; the rainbow apple is a star, so it looks like a power-up
(define (draw-rainbow-apple colour p scene)
  (draw-cell (star 12 "solid" colour) p scene))

(define (draw-rainbow g scene)
  (if (posn? (game-rainbow g))
      (draw-rainbow-apple (rainbow-colour (game-ticks g)) (game-rainbow g) scene)
      scene))

(define (level-name level)
  (cond
    [(string=? level "easy") "EASY"]
    [(string=? level "medium") "MEDIUM"]
    [else "HARD"]))

(check-expect (level-name "easy") "EASY")
(check-expect (level-name "medium") "MEDIUM")
(check-expect (level-name "hard") "HARD")

;; score on the left of the strip, difficulty on the right
(define (draw-hud g scene)
  (place-image (text (string-append "SCORE " (number->string (game-score g))) 18 hud-text-colour)
               70 (/ HUD-H 2)
               (place-image (text (level-name (game-level g)) 18 hud-text-colour)
                            (- SCENE-W 60) (/ HUD-H 2)
                            scene)))

(define (draw-game g)
  (draw-hud g (draw-snake g (draw-rainbow g (draw-apple (game-food g) BACKGROUND)))))

;; ---------- Drawing the home screen ----------

(define (draw-centred-text str size colour y scene)
  (place-image (text str size colour) (/ SCENE-W 2) y scene))

;; "> MEDIUM <" when name is the chosen level, otherwise just "EASY"
(define (level-label name level)
  (if (string=? name level)
      (string-append "> " (level-name name) " <")
      (level-name name)))

(check-expect (level-label "medium" "medium") "> MEDIUM <")
(check-expect (level-label "easy" "medium") "EASY")

(define (difficulty-line level)
  (string-append (level-label "easy" level) "  "
                 (level-label "medium" level) "  "
                 (level-label "hard" level)))

(check-expect (difficulty-line "medium") "EASY  > MEDIUM <  HARD")
(check-expect (difficulty-line "easy") "> EASY <  MEDIUM  HARD")

;; on for the first half of every second, off for the second half
(define (blink-on? t)
  (< (modulo t TICKS-PER-SECOND) (/ TICKS-PER-SECOND 2)))

(check-expect (blink-on? 0) #true)
(check-expect (blink-on? 13) #true)
(check-expect (blink-on? 14) #false)
(check-expect (blink-on? 30) #true)

;; a row of little rainbow squares under the title
(define (draw-title-squares scene)
  (foldl (lambda (i s) (place-image (square 12 "solid" (rainbow-colour i))
                                    (+ (/ SCENE-W 2) (* 20 (- i 3))) 165
                                    s))
         scene
         (range 0 7 1)))

;; four segments in the chosen skin, drawn the same way as in the game
(define (draw-preview-snake skin scene)
  (draw-head skin "right" (make-posn 11 11)
             (foldl (lambda (x s) (draw-segment skin (make-posn x 11) s))
                    scene
                    (range 8 11 1))))

(define (draw-press-enter t scene)
  (if (blink-on? t)
      (draw-centred-text "PRESS ENTER" 24 title-colour 400 scene)
      scene))

(define (draw-home h)
  (draw-press-enter (home-ticks h)
   (draw-centred-text "C: CHANGE COLOUR" 14 text-colour 330
    (draw-preview-snake (home-skin h)
     (draw-centred-text "E / M / H" 14 text-colour 250
      (draw-centred-text (difficulty-line (home-level h)) 20 text-colour 222
       (draw-title-squares
        (draw-centred-text "SNAKE" 64 title-colour 115 FRAME))))))))

;; ---------- big-bang handlers ----------

(define (draw w)
  (cond
    [(home? w) (draw-home w)]
    [(game? w) (draw-game w)]))

(define (tick w)
  (cond
    [(home? w) (make-home (home-level w) (home-skin w) (+ 1 (home-ticks w)))]
    [(game? w) (update-rainbow (step (add-tick w)))]))

(check-expect (tick START) (make-home "medium" "green" 1))

(define (key w k)
  (cond
    [(home? w) (home-key w k)]
    [(game? w) (change-dir w k)]))

(check-expect (key START "c") (make-home "medium" "blue" 0))
(check-expect (key (start-game START) "up") (change-dir (start-game START) "up"))

(define (stop? w)
  (cond
    [(home? w) #false]
    [(game? w) (game-over? w)]))

(check-expect (stop? START) #false)
(check-expect (stop? (start-game START)) #false)

(big-bang START (to-draw draw) (on-tick tick) (on-key key) (stop-when stop?))
