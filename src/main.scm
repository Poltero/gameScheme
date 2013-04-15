;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo with OpenGL

(define-structure tile posx posy width height)
(define-structure camera position state speed)
(define-structure enemy posx posy width height points color)
(define-structure player posx posy width height vstate hstate score)
(define-structure coin posx posy width height points color)
(define-structure world gamestates tiles camera player coins enemies message)


(define world-map '#(#(0 0 0 0 0 0 ++ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 0 0 0 1 0 0 + 0 0 0 0 0 0 0 0 ++ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 +++ 0 0 + 0 0 0 0 0 0 + i i i 0 0 * 0 0 * 0 0 0 0 0 0 0 0 0 0 + 0 0 0 0 0 s 0 0 0 + 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(+ 0 1 0 0 0 0 0 0 0 0 * i i i 0 * * 0 i 0 0 0 0 0 0 +++ 1 1 + 1 i 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 * * * * * * * * 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(+++ + 1 1 * 1 1 ++ ++ + 0 0 + 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 ++ 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1)
                         ))


;Level dimensions
(define level-width 10000.0)
(define level-height 400.0)


(define check-collision-player-with-coin
  (lambda (player coin)
    (let check-collision (
                          (leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (topA (player-posy player))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (coin-posx coin))
                          (rightB (+ (coin-posx coin) (coin-width coin)))
                          (topB (coin-posy coin))
                          (bottomB (+ (coin-posy coin) (coin-height coin))))
      (if (<= bottomA topB)
          #f
          (if (>= topA bottomB)
              #f
              (if (<= rightA leftB)
                  #f
                  (if (>= leftA rightB)
                      #f
                      #t)))))))

(define check-collision-player-with-enemy
  (lambda (player enemy)
    (let check-collision (
                          (leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (topA (player-posy player))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (enemy-posx enemy))
                          (rightB (+ (enemy-posx enemy) (enemy-width enemy)))
                          (topB (enemy-posy enemy))
                          (bottomB (+ (enemy-posy enemy) (enemy-height enemy))))
      (if (<= bottomA topB)
          #f
          (if (>= topA bottomB)
              #f
              (if (<= rightA leftB)
                  #f
                  (if (>= leftA rightB)
                      #f
                      #t)))))))

(define check-collision-player-with-finish
  (lambda (player finish)
    (let check-collision (
                          (leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (topA (player-posy player))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (finish-posx finish))
                          (rightB (+ (finish-posx finish) (finish-width finish)))
                          (topB (finish-posy finish))
                          (bottomB (+ (finish-posy finish) (finish-height finish))))
      (if (<= bottomA topB)
          #f
          (if (>= topA bottomB)
              #f
              (if (<= rightA leftB)
                  #f
                  (if (>= leftA rightB)
                      #f
                      #t)))))))



(define update-player-points-for-take-coin
  (lambda (player coins)
    (let loop ((rest coins))
      (unless (null? rest)
              (if (check-collision-player-with-coin player (car rest))
                  (begin
                    (coin-posx-set! (car rest) -20)
                    (player-score-set! player (+ (player-score player) (coin-points (car rest)))))
                  (loop (cdr rest)))))))

(define check-player-crash-enemy
  (lambda (player enemies)
    (let loop ((rest enemies))
      (unless (null? rest)
              (if (check-collision-player-with-enemy player (car rest))
                  #t
                  (loop (cdr rest)))))))



(define collision-down-tiles
  (lambda (player tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and
               (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) 40) (tile-posx (car rest))))
                   (< (player-posx player) (+ (tile-posx (car rest)) 40))
                   (> (player-posy player) (- (tile-posy (car rest)) 39))
                   (< (player-posy player) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))

(define collision-down-tiles-enemy
  (lambda (enemy tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (enemy-posx enemy) (tile-posx (car rest))) (> (+ (enemy-posx enemy) 40) (tile-posx (car rest))))
                   (< (enemy-posx enemy) (+ (tile-posx (car rest)) 40))
                   (> (enemy-posy enemy) (- (tile-posy (car rest)) 39))
                   (< (enemy-posy enemy) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))

(define collision-right-tiles
  (lambda (player tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) 15) (tile-posx (car rest))))
               (> (+ (player-posx player) 40) (tile-posx (car rest)))
               (> (player-posy player) (tile-posy (car rest)))
               (< (- (player-posy player) 30) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))


(define collision-top-coins
  (lambda (player coinslist)
    (let loop ((rest coinslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (coin-posx (car rest))) (> (+ (player-posx player) 15) (coin-posx (car rest))))
                   (< (player-posx player) (+ (coin-posx (car rest)) 15))
                   (> (player-posy player) (coin-posy (car rest)))
                   (< (- (player-posy player) 40) (coin-posy (car rest))))
              (car rest)
              (loop (cdr rest)))))))

(define collision-down-coins
  (lambda (player coinslist)
    (let loop ((rest coinslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (coin-posx (car rest))) (> (+ (player-posx player) 15) (coin-posx (car rest))))
                   (< (player-posx player) (+ (coin-posx (car rest)) 15))
                   (> (player-posy player) (- (coin-posy (car rest)) 15))
                   (< (player-posy player) (coin-posy (car rest))))
              (car rest)
              (loop (cdr rest)))))))

(define collision-top-tiles
  (lambda (player tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) 15) (tile-posx (car rest))))
               (< (player-posx player)  (+ ( tile-posx (car rest)) 40))
               (> (player-posy player) (tile-posy (car rest)))
               (< (- (player-posy player) 40) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))


(define condition-short
  (lambda (condition values)
    (if condition
        (car values)
        (car (cdr values)))))

(define (create-tiles-map l)
  (let loop ((rest-map world-map) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (let create-plataforms ((element (vector-ref (vector-ref rest-map count-y) count-x)))
            (if (or (eq? element 1) (eq? element '+) (eq? element '*))
                (let create-plataform-normal ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (if (< number 4)
                      (begin
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-normal (+ number 1) (+ posx 39))))))
            (if (or (eq? element 2) (eq? element '+++))
                (let create-plataform-double ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (if (< number 8)
                      (begin
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-double (+ number 1) (+ posx 39))))))
            (if (or (eq? element 'i) (eq? element '++))
                (let create-unique-plataform ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest)))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-coins-map l)
  (let loop ((rest-map world-map) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (case (vector-ref (vector-ref rest-map count-y) count-x)
            ((+)
             (let create-plataform-with-coins ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 4)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-short (< count-y 2) '(98 102)))) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins (+ number 1) (+ posx 40))))))
            ((++)
             (let create-plataform-with-coins-special ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 1)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-short (< count-y 2) '(88 102)) )) 15.0 15.0 50 'green) rest))
                     (create-plataform-with-coins-special (+ number 1) (+ posx 40))))))
            ((+++)
             (let create-plataform-with-coins-doubles ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 8)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-short (< count-y 2) '(98 102)))) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins-doubles (+ number 1) (+ posx 40)))))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-enemies-map l)
  (let loop ((rest-map world-map) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (case (vector-ref (vector-ref rest-map count-y) count-x)
            ((*)
             (let create-enemy-normal ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (set! rest (cons (make-enemy (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) 99)) 40.0 40.0 10 'blue) rest)))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))



(define (main)
  ((fusion:create-simple-gl-cairo '(width: 1280 height: 752))
   (lambda (event world)
     ;;(println (string-append "event: " (object->string event) " ; world: " (object->string world)))
     (let ((type (SDL_Event-type event)))
       (cond
        ((= type SDL_QUIT)
         'exit)
        ((= type SDL_MOUSEBUTTONDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Button down")
         'new-world-modified-by-mouse-button-event)
        ((= type SDL_KEYDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
         (let* ((kevt (SDL_Event-key event))
                (key (SDL_Keysym-sym
                      (SDL_KeyboardEvent-keysym kevt))))
           (cond ((= key SDLK_ESCAPE)
                  'exit)
                 ((= key SDLK_RETURN)
                  (if (or (eq? (world-gamestates world) 'splashscreen) (eq? (world-gamestates world) 'lose) (eq? (world-gamestates world) 'win))
                      (make-world 
                       'gamescreen 
                       (create-tiles-map (world-tiles world))
                       (make-camera
                        0.0
                        'auto
                        0.1)
                       (make-player
                        400.0
                        450.0 
                        30.0
                        30.0
                        'none
                        'down
                        0)
                       (create-coins-map (world-coins world))
                       (create-enemies-map (world-enemies world))
                       "")
                             
                      world))

                 ((= key SDLK_LEFT)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tiles world)
                       (world-camera world)
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        'left 
                        (player-hstate (world-player world))
                        (player-score (world-player world)))
                       (world-coins world)
                       (world-enemies world)
                       (world-message world))

                      world))
                 ((= key SDLK_RIGHT)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tiles world)
                       (world-camera world)
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        'right 
                        (player-hstate (world-player world))
                        (player-score (world-player world)))
                       (world-coins world)
                       (world-enemies world)
                       (world-message world))
                      
                      world))
                 ((= key SDLK_UP)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tiles world)
                       (world-camera world)
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        (player-vstate (world-player world)) 
                        'up
                        (player-score (world-player world)))
                       (world-coins world)
                       (world-enemies world)
                       (world-message world))
                     
                      world))
                 (else
                  ;;(SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))
                  world))))
        
        ((= type SDL_KEYUP)
         (let* ((kevt (SDL_Event-key event))
                (key (SDL_Keysym-sym
                      (SDL_KeyboardEvent-keysym kevt))))
           (cond ((= key SDLK_LEFT)
                  (if (and (eq? (player-vstate (world-player world)) 'left) (eq? (world-gamestates world) 'gamescreen))
                      (make-world
                       (world-gamestates world) 
                       (world-tiles world)
                       (world-camera world)
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        'none 
                        (player-hstate (world-player world))
                        (player-score (world-player world)))
                       (world-coins world)
                       (world-enemies world)
                       (world-message world))
                      
                     
                      world))
                 ((= key SDLK_RIGHT)
                  (if (and (eq? (player-vstate (world-player world)) 'right) (eq? (world-gamestates world) 'gamescreen))
                      (make-world 
                       (world-gamestates world) 
                       (world-tiles world)
                       (world-camera world)
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world))
                        'none
                        (player-hstate (world-player world))
                        (player-score (world-player world)))
                       (world-coins world)
                       (world-enemies world)
                       (world-message world))
                      
                      world))
                 ((= key SDLK_UP)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tiles world)
                       (world-camera world)
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world))
                        (player-vstate (world-player world)) 
                        (player-hstate (world-player world))
                        (player-score (world-player world)))
                       (world-coins world)
                       (world-enemies world)
                       (world-message world))
                     
                      world))
                 (else
                  world))))

        (else
         world))))
   (let ((last-time 0) (delta-time 0) (last-posx 0) (mov #t) (position-y-origin 0))
     (lambda (cr time world)
       (set! delta-time (- time last-time))
       (set! last-time time)
       ;(println (string-append "Position camera " (object->string (world-camera world)) " position-x: " (object->string (world-player world))))
       ;;(SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (object->string (SDL_GL_Extension_Supported "GL_EXT_texture_format_BGRA8888")))                    
       

       (case (world-gamestates world)
         
         ((lose)
          (cairo_set_source_rgba cr 0.0 1.0 1.0 0.01)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)

          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_set_font_size cr 90.0)
          (cairo_move_to cr 150.0 350.0)
          (cairo_show_text cr "GREAT!!, HAS DIED")
          

          ;;Reset lists
          (world-tiles-set! world '())
          (world-coins-set! world '())
          (world-enemies-set! world '()))


         ((win)
          (cairo_set_source_rgba cr 0.0 1.0 1.0 0.01)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)

          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_set_font_size cr 90.0)
          (cairo_move_to cr 250.0 350.0)
          (cairo_show_text cr "YOU WIN")
          

          ;;Reset lists
          (world-tiles-set! world '())
          (world-coins-set! world '())
          (world-enemies-set! world '())
          )

         ((splashscreen)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)

          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 90.0)
          (cairo_move_to cr 380.0 350.0)
          (cairo_show_text cr "THE GAME")

          (cairo_set_font_size cr 20.0)
          (cairo_move_to cr 500.0 450.0)
          (cairo_show_text cr "PRESS ENTER TO START"))
         
         ((gamescreen)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)
          
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 50.0)
          (cairo_move_to cr 260.0 650.0)
          (cairo_show_text cr "GO GO GO GO!!")


          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 20.0)
          (cairo_move_to cr 1000.0 100.0)
          (cairo_show_text cr "Score: ")

          (cairo_move_to cr 1100.0 100.0)
          (cairo_show_text cr (number->string (player-score (world-player world))))

          ;; Debug
          ;; (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          ;; (cairo_set_font_size cr 50.0)
          ;; (cairo_move_to cr 100.0 50.0)
          ;; (cairo_show_text cr (object->string (camera-position (world-camera world))))

          ;;Messages
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 30.0)
          (cairo_move_to cr 400.0 200.0)
          (cairo_show_text cr (world-message world))

          
        
          ;; ;;Calculate points collision down and top
          ;; (let coin-collided ((coin-collided-down (collision-down-coins (world-player world) (world-coins world))) (coin-collided-top (collision-top-coins (world-player world) (world-coins world))))
          ;;   (if coin-collided-down
          ;;       (begin
          ;;         (coin-posx-set! coin-collided-down -20)
          ;;         (player-score-set! (world-player world) (+ (player-score (world-player world)) (coin-points coin-collided-down)))))
          ;;   (if coin-collided-top
          ;;       (begin
          ;;         (coin-posx-set! coin-collided-top -20)
          ;;         (player-score-set! (world-player world) (+ (player-score (world-player world)) (coin-points coin-collided-top))))))


          (update-player-points-for-take-coin (world-player world) (world-coins world))
          
          ;;Drawing and moving all tiles
          
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (let loop ((rest (world-tiles world)))
            (if (not (null? rest))
                (begin
                  (cairo_rectangle cr (exact->inexact (- (tile-posx  (car rest)) (camera-position (world-camera world)))) (exact->inexact (tile-posy (car rest))) (tile-width (car rest)) (tile-height (car rest)))
                  (cairo_fill cr)
                  (loop (cdr rest)))
                '()))

          
          ;;Drawing and moving all coins
          (let loop ((rest (world-coins world)))
            (if (not (null? rest))
                (begin
                  (case (coin-color (car rest))
                    ((yellow)
                     (cairo_set_source_rgba cr 1.0 1.0 0.0 1.0))
                    ((green)
                     (cairo_set_source_rgba cr 0.0 1.0 0.0 1.0)))
                  (cairo_rectangle cr (exact->inexact (- (coin-posx  (car rest)) (camera-position (world-camera world)))) (coin-posy (car rest)) (coin-width (car rest)) (coin-height (car rest)))
                  (cairo_fill cr)
                  (loop (cdr rest)))
                '()))

          ;;Drawing all enemies 
          (cairo_set_source_rgba cr 0.0 0.0 1.0 0.9)
          (let loop ((rest (world-enemies world)))
            (if (not (null? rest))
                (begin
                  (if (not (collision-down-tiles-enemy (car rest) (world-tiles world)))
                      (begin
                        (enemy-posx-set! (car rest) (- (enemy-posx (car rest)) (* 0.1 delta-time)))
                        (enemy-posy-set! (car rest) (+ (enemy-posy (car rest)) (* 0.1 delta-time))))
                      (enemy-posx-set! (car rest) (- (enemy-posx (car rest)) (* 0.1 delta-time))))
                  (cairo_rectangle cr (exact->inexact (- (enemy-posx  (car rest)) (camera-position (world-camera world)))) (exact->inexact (enemy-posy (car rest))) (enemy-width (car rest)) (enemy-height (car rest)))
                  (cairo_fill cr)
                  (loop (cdr rest)))
                '()))
          



          (if (eq? (player-vstate (world-player world)) 'left)
              (let player-left ((camera (world-camera world)) (tiles (world-tiles world)) (player (world-player world)))
               (begin
                 (player-posx-set! player (- (player-posx player) (* 0.3 delta-time)))
                 (if (eq? (camera-state camera) 'on)
                     (camera-position-set! camera (- (camera-position camera) (* 0.3 delta-time)))))))

          (if (eq? (player-vstate (world-player world)) 'right)
             (let player-right ((camera (world-camera world)) (tiles (world-tiles world)) (player (world-player world)))
               (begin
                 (player-posx-set! player (+ (player-posx player) (* 0.3 delta-time)))
                 (if (eq? (camera-state camera) 'on)
                     (camera-position-set! camera (+ (camera-position camera) (* 0.3 delta-time)))))))

          
          (if (eq? (player-hstate (world-player world)) 'up) 
              (if (collision-down-tiles (world-player world) (world-tiles world))
                  (player-hstate-set! (world-player world) 'jump)
                  (player-hstate-set! (world-player world) 'down)))


          (if (eq? (player-hstate (world-player world)) 'jump) 
              (if (not (collision-top-tiles (world-player world) (world-tiles world)))
                  (begin 
                    (if (= position-y-origin (player-posy (world-player world)))
                        (set! position-y-origin (- (player-posy (world-player world)) -50)))
                    (let player-up ((player (world-player world)))
                      (player-posy-set! player (- (player-posy player) (* 0.3 delta-time)))))
                  (player-hstate-set! (world-player world) 'down)))

          (if (and (eq? (player-hstate (world-player world)) 'down) (not (collision-down-tiles (world-player world) (world-tiles world))))
              (let player-down ((player (world-player world)))
                (player-posy-set! player (+ (player-posy player) (* 0.3 delta-time)))))
          

          ;Control limits jump
          (if (> position-y-origin (player-posy (world-player world)))
              (player-hstate-set! (world-player world) 'down))

          
          
          ;;Control limits Y
          (if (or (< (player-posy (world-player world)) 0) (> (+ (player-posy (world-player world)) (player-height (world-player world))) level-height))
              #f)
          

          ;;Set camera
          (if (eq? (camera-state (world-camera world)) 'auto)
              (camera-position-set! (world-camera world) (+ (camera-position (world-camera world)) (* (camera-speed (world-camera world)) delta-time))))

          

          ;;keep the camera in bounds
          (if (< (camera-position (world-camera world)) 0)
              (camera-position-set! (world-camera world) 0))
          (if (> (camera-position (world-camera world)) (- level-width (camera-position (world-camera world))))
              (world-gamestates-set! world 'win))


          ;;Exceder a camara
          (if (> (- (player-posx (world-player world)) (camera-position (world-camera world))) 1280.0)
              (begin
                (world-message-set! world (string-append "Are you sure ? (- " (number->string (player-score (world-player world))) ")"))
                (player-posx-set! (world-player world) (- (player-posx (world-player world)) 200.0))
                (player-score-set! (world-player world) 0)))
          
          ;;Lose player
          (if (> (player-posy (world-player world)) 750)
              (world-gamestates-set! world 'lose))

          ;;Que la camara te coma
          (if (< (- (player-posx (world-player world)) (camera-position (world-camera world))) (* -1 (player-width (world-player world))))
              (world-gamestates-set! world 'lose))

          ;;Que un enemigo se choque con el jugador
          (if (check-player-crash-enemy (world-player world) (world-enemies world))
              (world-gamestates-set! world 'lose))


          ;; Drawing player
          (cairo_set_source_rgba cr 1.0 0.0 0.0 1.0)
          (let drawing-player ((player (world-player world)))
            (cairo_rectangle cr (- ( player-posx player) (camera-position (world-camera world))) (player-posy player) (player-width player) (player-height player)))
          (cairo_fill cr)
          


          ;; Drawing enemy principal (para mapas con camara automatica)
          ;; (cairo_set_source_rgba cr 1.0 0.0 0.0 1.0)
          ;; (let drawing-enemy-principal ((enemy (world-enemy world)))
          ;;   (cairo_rectangle cr (enemy-posx enemy) (enemy-posy enemy) (enemy-width enemy) (enemy-height enemy)))
          ;; (cairo_fill cr)
          
          ))
       
   
      
       
       world))
   (make-world 
    'splashscreen
    '()
    'none
    (make-player 0 0 0 0 'none 'none 0)
    '()
    '()
    "")))