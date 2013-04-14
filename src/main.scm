;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo with OpenGL

(define-structure tile posx posy width height)
(define-structure camera position state)
(define-structure enemy posx posy width height)
(define-structure player posx posy width height vstate hstate)
(define-structure coin posx posy width height points)
(define-structure world gamestates tiles camera player coins)

;; (define map-world '#(#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) ; posicion superior
;;                      #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) ; posicion medio
;;                      #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))) ; posicion suelo

(define new-map-world '#(#(0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 0 0 0 + 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 0 0 0 + 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(+ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1)
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
                   (> (player-posy player) (- (tile-posy (car rest)) 40))
                   (< (player-posy player) (tile-posy (car rest))))
              #t
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

(define status-game 'idle)


;; (define collision-down-tile
;;   (lambda (player tilelist)
;;     (let loop ((rest tilelist))
;;       (unless (null? rest)
;;               (if (and (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) (player-width player)) (tile-posx (car rest))))
;;                    (< (player-posx player) (+ (tile-posx (car rest)) (tile-width (car rest))))
;;                    (< (- (player-posy player) (- ( player-height player) 20)) (tile-posy (car rest)))
;;                    )
                  
;;                   #t
;;                   (loop (cdr rest)))))))

(define (metodo l world)
  (let loop-y ((rest-map new-map-world) (count-y 0))
    (if (< count-y 5)
        (begin
          (let loop-x ((count-x 0) (rest l))
            (if (< count-x 28)
                (begin
                  (if (eq? (vector-ref (vector-ref rest-map count-y) count-x) 1)
                      (begin
                        (set! rest (cons (make-tile (exact->inexact (+ (* 100 count-x) 40)) (exact->inexact (+ (* 0.7 count-y) 200)) 40.0 40.0) rest))
                        (loop-x (+ count-x 1) rest))
                      '()))
                '()))
          (loop-y rest-map (+ count-y 1)))
        '())))

(define (puta l)
  (let loop ((rest-map new-map-world) (rest l) (count-x 0) (count-y 0))
    (if (eq? (vector-ref (vector-ref rest-map count-y) count-x) 1)
        (if (< count-y 5)
            (begin (let create-plataform-normal ((counter 0))
                     (if (< counter 4)
                         (cons (make-tile (exact->inexact (+ (* 100 count-x) 40)) (exact->inexact (+ (* 0.7 count-y) 200)) 40.0 40.0) (create-plataform-normal (+ counter 1))))) 
                   
                   (if (< count-x 28)
                       (loop rest-map rest (+ count-x 1) count-y)
                       (loop rest-map rest 0 (+ count-y 1)))))
        
        (if (< count-x 101) 
            (loop rest-map rest (+ count-x 1) count-y)
            '()))))



(define (create-map l)
  (let loop ((rest-map new-map-world) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (if (or (eq? (vector-ref (vector-ref rest-map count-y) count-x) 1) (eq? (vector-ref (vector-ref rest-map count-y) count-x) '+))
              (let create-plataform-normal ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                (if (< number 4)
                    (begin
                      (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                      (create-plataform-normal (+ number 1) (+ posx 40))))))
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
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) 102)) 15.0 15.0 10.0) rest))
                     (create-plataform-with-coins (+ number 1) (+ posx 40)))))))
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
                       (create-map (world-tiles world))
                       (make-camera
                        0.0
                        'auto)
                       (make-player
                        400.0
                        487.0 
                        30.0
                        30.0
                        'none 
                        'none)
                       (create-coins-map (world-coins world)))
                             
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
                        (player-hstate (world-player world)))
                       (world-coins world))

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
                        (player-hstate (world-player world)))
                       (world-coins world))
                      
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
                        'up)
                       (world-coins world))
                     
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
                        (player-hstate (world-player world)))
                       (world-coins world))
                      
                     
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
                        (player-hstate (world-player world)))
                       (world-coins world))
                      
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
                        'down)
                       (world-coins world))
                     
                      world))
                 (else
                  world))))

        (else
         world))))
   (let ((last-time 0) (delta-time 0) (last-posx 0))
     (lambda (cr time world)
       (set! delta-time (- time last-time))
       (set! last-time time)
       (println (string-append "time: " (object->string time) " ; coins: " (object->string  (world-coins world)) " ListaMetodo: N/D" ))
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
         
         ((splashscreen)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)

          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 90.0)
          (cairo_move_to cr 240.0 350.0)
          (cairo_show_text cr "SPLASHSCREEN"))

         ((gamescreen)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)
          
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 50.0)
          (cairo_move_to cr 260.0 500.0)
          (cairo_show_text cr "GAMESCREEN")

          ;; Debug
          ;; (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          ;; (cairo_set_font_size cr 50.0)
          ;; (cairo_move_to cr 100.0 50.0)
          ;; (cairo_show_text cr (object->string (camera-position (world-camera world))))

          

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
          
          

          
          ;;Drawing and moving all tiles
          
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (let loop ((rest (world-tiles world)))
            (if (not (null? rest))
                (begin
                  (cairo_rectangle cr (- (tile-posx  (car rest)) (camera-position (world-camera world))) (tile-posy (car rest)) (tile-width (car rest)) (tile-height (car rest)))
                  (cairo_fill cr)
                  (loop (cdr rest)))
                '()))

          
          ;;Drawing and moving all coins
          (cairo_set_source_rgba cr 1.0 1.0 0.0 1.0)
          (let loop ((rest (world-coins world)))
            (if (not (null? rest))
                (begin
                  (cairo_rectangle cr (- (coin-posx  (car rest)) (camera-position (world-camera world))) (coin-posy (car rest)) (coin-width (car rest)) (coin-height (car rest)))
                  (cairo_fill cr)
                  (loop (cdr rest)))
                '()))
          



          (if (eq? (player-vstate (world-player world)) 'left)
              (let player-left ((camera (world-camera world)) (tiles (world-tiles world)) (player (world-player world)))
               (begin
                 ;(camera-position-set! camera (- (camera-position camera) (* 0.3 delta-time)))
                 (player-posx-set! player (- (player-posx player) (* 0.3 delta-time)))
                 ;(update-position-elements tiles player camera)
                 )
               ))

          (if (eq? (player-vstate (world-player world)) 'right)
             (let player-right ((camera (world-camera world)) (tiles (world-tiles world)) (player (world-player world)))
               (begin
                 (player-posx-set! player (+ (player-posx player) (* 0.3 delta-time)))
                 ;(camera-position-set! camera (+ (camera-position camera) (* 0.3 delta-time)))
                 )))

          ;;Control limits X
          ;; (if (or (< (player-posx (world-player world)) 0) (> (+ (player-posx (world-player world)) (player-width (world-player world))) level-width))
          ;;     (player-posx-set! (world-player world) (+ (player-posx (world-player world)) (* 0.3 delta-time))))

          

          (if (and (eq? (player-hstate (world-player world)) 'up) (not (collision-top-tiles (world-player world) (world-tiles world))))
              (let player-up ((player (world-player world)))
                (player-posy-set! player (- (player-posy player) (* 0.3 delta-time)))))

          (if (and (eq? (player-hstate (world-player world)) 'down) (not (collision-down-tiles (world-player world) (world-tiles world))))
              (let player-down ((player (world-player world)))
                (player-posy-set! player (+ (player-posy player) (* 0.3 delta-time)))))

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
          
          
          ;(println (object->string (- (player-posx (world-player world)) last-posx)))

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
    '())))