;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo with OpenGL

(define-structure tile posx posy width height)
(define-structure camera position state)
(define-structure enemy posx posy width height points color)
(define-structure player posx posy width height vstate hstate score)
(define-structure coin posx posy width height points color)
(define-structure world gamestates tiles camera player coins enemies message)



(define (filter pred lis)               ; Sleazing with EQ? makes this
  (let recur ((lis lis))  
    (if (null? lis) lis   ; Use NOT-PAIR? to handle dotted lists.
        (let ((head (car lis))
              (tail (cdr lis)))
          (if (pred head)
              (let ((new-tail (recur tail))) ; Replicate the RECUR call so
                (if (eq? tail new-tail) lis
                    (cons head new-tail)))
              (recur tail))))))

(define (remove pred lis) (filter (lambda (x) (not (pred x))) lis))


;; (define map-world '#(#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) ; posicion superior
;;                      #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) ; posicion medio
;;                      #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))) ; posicion suelo

(define new-map-world '#(#(0 0 0 0 0 0 ++ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 0 0 0 1 0 0 + 0 0 0 0 0 0 0 0 ++ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 +++ 0 0 + 0 0 0 0 0 0 + i i i 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 + 0 0 0 0 0 0 0 0 0 + 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(+ 0 1 0 0 0 0 0 0 0 0 * i i i 0 0 0 0 i 0 0 0 0 0 0 +++ 1 1 + 1 i 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(+++ + 1 1 * 1 1 ++ ++ + 0 0 + 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 ++ 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1)
                         ))

;Screen
(define screen-width 300.0)

;Level dimensions
(define level-width 30000.0)
(define level-height 400.0)



(define collision-tiles
  (lambda (player tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) (player-width player)) (tile-posx (car rest))))
                   (< (player-posx player) (+ (tile-posx (car rest)) (tile-width (car rest))))
                   (> (- (player-posy player) (- ( player-height player) 20)) (- (tile-posy (car rest)) (tile-height (car rest))))
                   (< (- (player-posy player) (player-height player)) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))


(define collision-down-tiles
  (lambda (player tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) 15) (tile-posx (car rest))))
                   (< (player-posx player) (+ (tile-posx (car rest)) 40))
                   (> (player-posy player) (- (tile-posy (car rest)) 30))
                   (< (player-posy player) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))

(define collision-down-tiles-enemy
  (lambda (enemy tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (enemy-posx enemy) (tile-posx (car rest))) (> (+ (enemy-posx enemy) 15) (tile-posx (car rest))))
                   (< (enemy-posx enemy) (+ (tile-posx (car rest)) 40))
                   (> (enemy-posy enemy) (- (tile-posy (car rest)) 36))
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

(define update-position-elements
  (lambda (tiles player camera)
    (player-posx-set! player (+ (player-posx player) (camera-position camera)))
    (let loop ((rest tiles))
      (if (not (null? rest))
          (begin
            (tile-posx-set! (car rest) (- (tile-posx  (car rest)) (camera-position camera)))
            (loop (cdr rest)))
          '()))))


(define (create-tiles-map l)
  (let loop ((rest-map new-map-world) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (let create-plataforms ((element (vector-ref (vector-ref rest-map count-y) count-x)))
            (if (or (eq? element 1) (eq? element '+) (eq? element '*))
                (let create-plataform-normal ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (if (< number 4)
                      (begin
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-normal (+ number 1) (+ posx 40))))))
            (if (or (eq? element 2) (eq? element '+++))
                (let create-plataform-double ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (if (< number 8)
                      (begin
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-double (+ number 1) (+ posx 40))))))
            (if (or (eq? element 'i) (eq? element '++))
                (let create-unique-plataform ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest)))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-coins-map l)
  (let loop ((rest-map new-map-world) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (case (vector-ref (vector-ref rest-map count-y) count-x)
            ((+)
             (let create-plataform-with-coins ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 4)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) 102)) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins (+ number 1) (+ posx 40))))))
            ((++)
             (let create-plataform-with-coins-special ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 1)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) 102)) 15.0 15.0 50 'green) rest))
                     (create-plataform-with-coins-special (+ number 1) (+ posx 40))))))
            ((+++)
             (let create-plataform-with-coins-doubles ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 8)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) 102)) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins-doubles (+ number 1) (+ posx 40)))))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-enemies-map l)
  (let loop ((rest-map new-map-world) (rest l) (count-x 0) (count-y 0))
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
                  (if (eq? (world-gamestates world) 'splashscreen)
                      (make-world 
                       'gamescreen 
                       (create-tiles-map (world-tiles world))
                       (make-camera
                        0.0
                        'on)
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
                  (if (eq? (player-vstate (world-player world)) 'left)
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
                  (if (eq? (player-vstate (world-player world)) 'right)
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
       (println (string-append "Position y origin: " (number->string position-y-origin) " position-y: " (object->string (world-player world))))
       ;;(SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (object->string (SDL_GL_Extension_Supported "GL_EXT_texture_format_BGRA8888")))
       
       
       ;; (let loop ((rest (world-tiles world)) (count 1))
       ;;   (if (not (null? rest))
       ;;       (loop (cdr rest) (+ count 1))
       ;;       (println (string-append "Number of tiles: " (number->string count)))))
       

       ;; (if (eq? status-game 'idle)
       ;;     (begin 
       ;;       (let generate-map ((rest-map new-map-world) (count-y 0))
       ;;         (if (< count-y 5)
       ;;             (let create-plataforms ((count-x 0))
       ;;               (if (< count-x 28)
       ;;                   (if (eq? (vector-ref (vector-ref rest-map count-y) count-x) 1)
       ;;                       (let create-plataform-normal ((counter 0))
       ;;                         (if (< counter 4)
       ;;                             (begin
       ;;                               (world-tiles-set! world (cons (make-tile (exact->inexact (* 100 count-x)) (exact->inexact (+ (* 0.7 count-y) 200)) 40.0 40.0) (world-tiles world)))
       ;;                               (create-plataform-normal (+ counter 1)))))
       ;;                       '())
       ;;                   '())
       ;;               (create-plataforms (+ count-x 1))
       ;;               ))
       ;;         ;(generate-map rest-map (+ count-y 1))
       ;;         )
       ;;       (set! status-game 'new)))

       ;(if (eq? status-game 'idle))
       ;; (define metodo 
       ;;   (lambda (world)
       ;;     (begin 
       ;;       (let loop-y ((rest-map new-map-world) (count-y 0))
       ;;         (if (< count-y 5)
       ;;             (begin
       ;;               (let loop-x ((count-x 0))
       ;;                 (if (< count-x 28)
       ;;                     (begin
       ;;                       (if (eq? (vector-ref (vector-ref rest-map count-y) count-x) 1)
       ;;                           (world-tiles-set! world (cons (make-tile (exact->inexact (+ (* 100 count-x) 40)) (exact->inexact (+ (* 0.7 count-y) 200)) 40.0 40.0) (world-tiles world))))
       ;;                       (loop-x (+ count-x 1)))))
       ;;               (loop-y rest-map (+ count-y 1)))))
       ;;       ;(set! status-game 'new)
       ;;       )))



       
       

       (case (world-gamestates world)
         
         ((lose)
          (cairo_set_source_rgba cr 0.0 1.0 1.0 0.01)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)

          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_set_font_size cr 90.0)
          (cairo_move_to cr 150.0 350.0)
          (cairo_show_text cr "GREAT!!, HAS DIED"))

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

          ;;calculate tiles in the world and paint
          
          ;; (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          ;; (let loop ((count-tiles (/ 5000 20)) (posx 0.0))
          ;;   (if (= count-tiles 0)
          ;;       '()
          ;;       (begin (cairo_rectangle cr (- posx (camera-position (world-camera world))) 400.0 20.0 20.0)
          ;;              (cairo_fill cr)
          ;;              (loop (- count-tiles 1) (+ posx 20.0)))))


          
          ;;paint tile test

          ;; (let paint-tile-test ((tile (world-tile world)))
          ;;   (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          ;;   (cairo_rectangle cr (tile-posx tile) (tile-posy tile) (tile-width tile) (tile-height tile))
          ;;   (cairo_fill cr))


          ;; (if (collision-tiles (world-player world) (world-tiles world))
          ;;     (println "Collision!")
          ;;     (println "Nothing!"))
          

          
          ; Drawing all map in the world
          ;; (let loop-map ((rest-map map-world) (count-y 0) (tileslist (world-tiles world)))
          ;;   (if (eq? flag-paint-map #t)
          ;;       (if (= count-y 3)
          ;;           (set! flag-paint-map #f)
          ;;           (begin 
          ;;             (case (vector-ref (vector-ref rest-map count-y) count-x)
          ;;               ((1)
          ;;                (let create-plataform-normal ((pos-y (* (+ count-y 0.7) 100)) (pos-x 1300) (counter 0))
          ;;                  (if (= counter 4)
          ;;                      '()
          ;;                      (begin 
          ;;                        (world-tiles-set! world (cons (make-tile (exact->inexact pos-x) (exact->inexact pos-y) 40.0 40.0) (world-tiles world)))
          ;;                        (create-plataform-normal pos-y (+ pos-x 40) (+ counter 1)))))))
          ;;             (loop-map rest-map (+ count-y 1) tileslist)))))

          ;;Generate all map
          
          

          ;; (if (= position-y-origin 0)
          ;;     (set! position-y-origin (player-posy (world-player world))))
          
        
          ;;Calculate points collision down and top
          (let coin-collided ((coin-collided-down (collision-down-coins (world-player world) (world-coins world))) (coin-collided-top (collision-top-coins (world-player world) (world-coins world))))
            (if coin-collided-down
                (begin
                  (coin-posx-set! coin-collided-down -20)
                  (player-score-set! (world-player world) (+ (player-score (world-player world)) (coin-points coin-collided-down)))))
            (if coin-collided-top
                (begin
                  (coin-posx-set! coin-collided-top -20)
                  (player-score-set! (world-player world) (+ (player-score (world-player world)) (coin-points coin-collided-top))))))
          
          
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

          ;;Control limits X
          ;; (if (or (< (player-posx (world-player world)) 0) (> (+ (player-posx (world-player world)) (player-width (world-player world))) level-width))
          ;;     (player-posx-set! (world-player world) (+ (player-posx (world-player world)) (* 0.3 delta-time))))

          
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
          
          ;;Center the camera over the player
          ;(camera-position-set! (world-camera world) (- (/ (+ (player-posx (world-player world)) (player-width (world-player world))) 2) (/ screen-width 2.0)))

          (if (eq? (camera-state (world-camera world)) 'auto)
              (camera-position-set! (world-camera world) (+ (camera-position (world-camera world)) (* 0.1 delta-time))))

          

          ;;keep the camera in bounds
          (if (< (camera-position (world-camera world)) 0)
              (camera-position-set! (world-camera world) 0))
          (if (> (camera-position (world-camera world)) (- level-width (camera-position (world-camera world))))
              (camera-position-set! (world-camera world) (- level-width (camera-position (world-camera world)))))


          ;; (if (> (player-posx (world-player world)) screen-width)
          ;;    (camera-position-set! (world-camera world) (- screen-width (camera-position (world-camera world)))))
          
          
          

          ;; (if (collision-down-tiles (world-player world) (world-tiles world))
          ;;     (println "Collision Down!"))
          
          
          ;; (if (> (+ (player-posx (world-player world)) (camera-position (world-camera world))) 800)
          ;;     (begin
          ;;       (camera-position-set! (world-camera world) (- (camera-position (world-camera world)) (* 0.5 delta-time)))
          ;;       )
          ;;     (println "Posx: " (object->string (player-posx (world-player world)))))


          ;; (if (and (not(eq? (player-hstate (world-player world)) 'left)) (not (eq? (player-hstate (world-player world)) 'right)))
          ;;     (let decrement-position-player ((player (world-player world)))
          ;;       (player-posx-set! player (- (player-posx player) (* 0.09 delta-time)))))

          

          ;;Exceder a camara
          (if (> (- (player-posx (world-player world)) (camera-position (world-camera world))) 1280.0)
              (begin
                (world-message-set! world (string-append "Are you sure ? (- " (number->string (player-score (world-player world))) ")"))
                (player-posx-set! (world-player world) (- (player-posx (world-player world)) 200.0))
                (player-score-set! (world-player world) 0)))
          
          ;;Lose player
          (if (> (player-posy (world-player world)) 750)
              (world-gamestates-set! world 'lose))


          ;; Drawing player
          (cairo_set_source_rgba cr 1.0 0.0 0.0 1.0)
          (let drawing-player ((player (world-player world)))
            (cairo_rectangle cr (- ( player-posx player) (camera-position (world-camera world))) (player-posy player) (player-width player) (player-height player)))
          
          (cairo_fill cr)
          
          ;; (cairo_rectangle cr (+ 250.0 20) 360.0 10.0 10.0)
          ;; (cairo_fill cr)



          ;; Drawing enemy principal
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
    'none
    '()
    '()
    "")))