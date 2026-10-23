#lang htdp/isl+
(require 2htdp/image)
(require 2htdp/universe)

(define CELLS 20)
(define SIZE 20)
(define WIDTH (* CELLS SIZE))
(define BG (empty-scene WIDTH WIDTH))
(define SNAKE-SQ (square SIZE "solid" "green"))
(define FOOD-SQ (square SIZE "solid" "red"))

(define-struct world (snake dir food))

(define START
  (make-world (list (make-posn 5 10) (make-posn 4 10) (make-posn 3 10))
              "right"
              (make-posn 12 10)))

(define (grid->pixel n)
  (+ (* n SIZE) (/ SIZE 2)))

(check-expect (grid->pixel 0) 10)
(check-expect (grid->pixel 3) 70)

(define (draw-cell img p scene)
  (place-image img (grid->pixel (posn-x p)) (grid->pixel (posn-y p)) scene))

(define (draw w)
  (foldl (lambda (p scene) (draw-cell SNAKE-SQ p scene))
         (draw-cell FOOD-SQ (world-food w) BG)
         (world-snake w)))

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

(define (eats-food? w)
  (equal? (next-head (first (world-snake w)) (world-dir w)) (world-food w)))

(check-expect (eats-food? (make-world (list (make-posn 1 1)) "right" (make-posn 2 1))) #true)
(check-expect (eats-food? (make-world (list (make-posn 1 1)) "up" (make-posn 2 1))) #false)

(define (random-food n)
  (make-posn (random n) (random n)))

(define (tick w)
  (if (eats-food? w)
      (make-world (grow-snake (world-snake w) (world-dir w))
                  (world-dir w)
                  (random-food CELLS))
      (make-world (move-snake (world-snake w) (world-dir w))
                  (world-dir w)
                  (world-food w))))

(check-expect (tick (make-world (list (make-posn 1 1)) "down" (make-posn 9 9)))
              (make-world (list (make-posn 1 2)) "down" (make-posn 9 9)))

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

(define (change-dir w k)
  (if (and (arrow? k) (not (opposite? k (world-dir w))))
      (make-world (world-snake w) k (world-food w))
      w))

(check-expect (change-dir START "up") (make-world (world-snake START) "up" (world-food START)))
(check-expect (change-dir START "left") START)
(check-expect (change-dir START "x") START)

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

(define (game-over? w)
  (or (hits-wall? (world-snake w)) (hits-self? (world-snake w))))

(check-expect (game-over? START) #false)
(check-expect (game-over? (make-world (list (make-posn 20 0)) "right" (make-posn 1 1))) #true)

(big-bang START (to-draw draw) (on-tick tick 0.15) (on-key change-dir) (stop-when game-over?))
