;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define ENDGAME (text "Game Over" 24 "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; ====================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ListOfInvader is one of:
;; - empty
;; (cons invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (list I1))
(define LOI3 (list I1 I2))
(define LOI4 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; ListOfMissile is one of:
;; - empty
;; (cons missile ListOfMissile)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (list M1))
(define LOM3 (list M1 M2))
(define LOM4 (list M1 M2 M3))

#;
(define (fn-for-lom loi)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; ==================
;; Functions

;; Game -> Game
;; start Space Invaders by evaluating (main G0)
(define (main game)
  (big-bang game
    (on-tick next-scene)     ; Game -> Game
    (to-draw render)         ; Game -> Image
    (on-key handle-key)      ; Game KeyEvent -> Game
    (stop-when game-over? end-game))) ; (Game -> Boolean) -> (Game -> Image)

;; Game -> Game
;; produce the next game scene by advancing invaders and missiles by their respective speeds

(define (next-scene game) BACKGROUND) ; stub

;; Game -> Image
;; Render Game state on BACKGROUND

(define (render game) BACKGROUND) ; stub

;; Game KeyEvent -> Game
;; Handle key events:
;;    "left" moves tank left
;;    "right" moves tank right
;;    " " fires missile
(check-expect (handle-key G2 " ") (make-game LOI2 (fire-missile (game-missiles G2) 
                                                                (tank-x (game-tank G2)))
                                             (game-tank G2)))
(check-expect (handle-key G2 "left") (make-game LOI2 (game-missiles G2)
                                                (go-left (game-tank G2))))
(check-expect (handle-key G2 "right") (make-game LOI2 (game-missiles G2)
                                                 (go-right (game-tank G2))))

;(define (handle-key game ke) 0) ; stub

(define (handle-key game ke)
  (cond [(key=? ke " ") (make-game (game-invaders game)
                                   (fire-missile (game-missiles game)
                                                 (tank-x (game-tank game)))
                                   (game-tank game))]
        [(key=? ke "left") (make-game (game-invaders game)
                                      (game-missiles game)
                                      (go-left (game-tank game)))]
        [(key=? ke "right") (make-game (game-invaders game)
                                       (game-missiles game)
                                       (go-right (game-tank game)))]))
                                   

;; Game -> Image
;; Display ENDGAME message

;(define (end-game game) 0) ; stub

(define (end-game game)
  (place-image ENDGAME
               CTR-X
               CTR-Y
               BACKGROUND))

;; Game -> Boolean
;; Return true if invader has landed (invader-x >= HEIGHT)
(check-expect (game-over? G0) false)
(check-expect (game-over? G1) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)

;(define (game-over? game) false) ; stub

; <template from Game>

(define (game-over? game)
  (invaders-landed? (game-invaders game)))

;; =================
;; Tank Helper Functions

;; Tank -> Tank
;; Produce next tank
(check-expect (move-tank T0) (make-tank (+ CTR-X TANK-SPEED) 1))
(check-expect (move-tank T2) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank WIDTH -1))

; (define (move-tank t) t) ; stub

; <use template from Tank>

(define (move-tank t)
  (cond [(offscreen? (tank-x t))
         (make-tank (tank-x t)
                    (change-direction (tank-dir t)))]
        [else
         (make-tank (+ (tank-x t)
                       (* TANK-SPEED (tank-dir t)))
                    (tank-dir t))]))

;; Tank -> Tank
;; Return tank headed left
(check-expect (go-left T0) (make-tank (/ WIDTH 2) -1))
(check-expect (go-left T1) (make-tank 50 -1))
(check-expect (go-left T2) (make-tank 50 -1))

; (define (go-left t) t) ; stub

(define (go-left t)
  (make-tank (tank-x t)
             -1))

;; Tank -> Tank
;; Return tank headed right
(check-expect (go-right T0) (make-tank (/ WIDTH 2) 1))
(check-expect (go-right T1) (make-tank 50 1))
(check-expect (go-right T2) (make-tank 50 1))

; (define (go-right t) t) ; stub

(define (go-right t)
  (make-tank (tank-x t)
             1))

;; =================
;; Invader Helper Functions

;; ListOfInvader -> ListOfInvader
;; Move all invaders in x and y directions
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders LOI3) (list (make-invader (+ 150 (* 12 INVADER-X-SPEED))
                                                       (+ 100 INVADER-Y-SPEED) 12)
                                         (make-invader (- 150 (* 10 INVADER-X-SPEED))
                                                       (+ HEIGHT INVADER-Y-SPEED) -10)))

; (define (move-invaders loi) loi) ; stub

; <template from ListOfInvader>

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; Invader -> Invader
;; Produce next invader moved in x and y directions
(check-expect (move-invader I1) (make-invader (+ 150 (* 12 INVADER-X-SPEED))
                                              (+ 100 INVADER-Y-SPEED) 12))
(check-expect (move-invader I2) (make-invader (- 150 (* 10 INVADER-X-SPEED))
                                              (+ HEIGHT INVADER-Y-SPEED) -10))
(check-expect (move-invader (make-invader 150 100 -12))
              (make-invader (- 150 (* 12 INVADER-X-SPEED))
                            (+ 100 INVADER-Y-SPEED) -12))
(check-expect (move-invader (make-invader WIDTH 100 INVADER-X-SPEED))
              (make-invader WIDTH 100 (* -1 INVADER-X-SPEED)))
(check-expect (move-invader (make-invader 0 100 (* -1 INVADER-X-SPEED)))
              (make-invader 0 100 INVADER-X-SPEED))

;(define (move-invader invader) invader) ; stub

; <use template from Invader>

(define (move-invader i)
  (cond [(offscreen? (invader-x i))
         (make-invader (invader-x i)
                       (invader-y i)
                       (change-direction (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))

;; =================
;; Missile Helper Functions

;; ListOfMissile -> ListOfMissile
;; Return a list of missiles with a new missile added
(check-expect (fire-missile empty CTR-X) (list (make-missile CTR-X (- HEIGHT TANK-HEIGHT/2))))
(check-expect (fire-missile LOM2 10) (cons (make-missile 10 (- HEIGHT TANK-HEIGHT/2)) LOM2))

;(define (fire-missile lom x) LOM2) ; stub

(define (fire-missile lom x)
  (cons (new-missile x) lom))


;; Integer -> Missile
;; Return a new Missile at provided x coordinate & HEIGHT - (TANK-HEIGHT/2)
(check-expect (new-missile CTR-X) (make-missile CTR-X (- HEIGHT TANK-HEIGHT/2)))
(check-expect (new-missile 200) (make-missile 200 (- HEIGHT TANK-HEIGHT/2)))

; (define (new-missile x) M1) ; stub

; <template from missile>

(define (new-missile x)
  (make-missile x (- HEIGHT TANK-HEIGHT/2)))


;; =================
;; Generic Helper Functions

;; Integer -> Boolean
;; Return true if
;;    x <= 0
;;    x >= WIDTH
(check-expect (offscreen? WIDTH) true)
(check-expect (offscreen? 0) true)
(check-expect (offscreen? (invader-x I1)) false)

; (define (offscreen? x) false) ; stub

(define (offscreen? x)
  (or (>= x WIDTH)
      (<= x 0)))

;; Integer -> Integer
;; Return -1 * x-direction provided
(check-expect (change-direction 5) (* -1 5))
(check-expect (change-direction -5) (* -1 -5))
(check-expect (change-direction 0) (* -1 0))

;(define (change-direction dx) 0) ; stub

(define (change-direction dx)
  (* -1 dx))

;; ListOfInvader -> Boolean
;; Return true if any invader has landed
(check-expect (invaders-landed? LOI1) false)
(check-expect (invaders-landed? LOI2) false)
(check-expect (invaders-landed? LOI3) true)
(check-expect (invaders-landed? LOI4) true)

;(define (invaders-landed? loi) false) ; stub

; <template from ListOfInvader>

(define (invaders-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (landed? (invader-y (first loi)))
             true
             (invaders-landed? (rest loi)))]))

;; Integer -> Boolean
;; Return true if y >= HEIGHT
(check-expect (landed? 0) false)
(check-expect (landed? CTR-Y) false)
(check-expect (landed? HEIGHT) true)
(check-expect (landed? (+ 1 HEIGHT)) true)

; (define (landed? y) false) ; stub

(define (landed? y)
  (>= y HEIGHT))