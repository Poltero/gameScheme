;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo with OpenGL

(define-structure tile posx posy width height)
(define-structure player posx posy width height vstate hstate)
(define-structure world gamestates tile player)

(define collision-tile
  (lambda (player tile)
    (if (and (or (> (player-posx player) (tile-posx tile)) (> (+ (player-posx player) (player-width player)) (tile-posx tile)))
                     (< (player-posx player) (+ (tile-posx tile) (tile-width tile)))
                     (> (- (player-posy player) (player-height player)) (- (tile-posy tile) (tile-height tile)))
                     (< (- (player-posy player) (player-height player)) (tile-posy tile)))
        #t
        #f)))


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
                      (make-world 'gamescreen (world-tile world) (make-player 250.0 360.0 30.0 30.0 'none 'none))
                      world))
                 ((= key SDLK_LEFT)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tile world) 
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        'left 
                        (player-hstate (world-player world))))

                      world))
                 ((= key SDLK_RIGHT)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tile world) 
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        'right 
                        (player-hstate (world-player world))))
                      
                      world))
                 ((= key SDLK_UP)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tile world) 
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        (player-vstate (world-player world)) 
                        'up))
                     
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
                       (world-tile world) 
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        'none 
                        (player-hstate (world-player world))))
                     
                      world))
                 ((= key SDLK_RIGHT)
                  (if (eq? (player-vstate (world-player world)) 'right)
                      (make-world 
                       (world-gamestates world) 
                       (world-tile world) 
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        'none 
                        (player-hstate (world-player world))))
                      
                      world))
                 ((= key SDLK_UP)
                  (if (eq? (world-gamestates world) 'gamescreen)
                      (make-world 
                       (world-gamestates world) 
                       (world-tile world) 
                       (make-player 
                        (player-posx (world-player world)) 
                        (player-posy (world-player world)) 
                        (player-width (world-player world)) 
                        (player-height (world-player world)) 
                        (player-vstate (world-player world)) 
                        'down))
                     
                      world))
                 (else
                  world))))

        (else
         world))))
   (let ((last-time 0) (delta-time 0))
     (lambda (cr time world)
       (set! delta-time (- time last-time))
       (set! last-time time)
       ;(println (string-append "time: " (object->string time) " ; world: " (object->string world)))
       ;;(SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (object->string (SDL_GL_Extension_Supported "GL_EXT_texture_format_BGRA8888")))

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

          


          ;;calculate tiles in the world and paint
          
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (let loop ((count-tiles (/ 1280 20)) (posx 0.0))
            (if (= count-tiles 0)
                '()
                (begin (cairo_rectangle cr posx 400.0 20.0 20.0)
                       (cairo_fill cr)
                       (loop (- count-tiles 1) (+ posx 20.0)))))

          
          ;;paint tile test

          (let paint-tile-test ((tile (world-tile world)))
            (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
            (cairo_rectangle cr (tile-posx tile) (tile-posy tile) (tile-width tile) (tile-height tile))
            (cairo_fill cr))


          (if (collision-tile (world-player world) (world-tile world))
              (println "Collision!")
              (println "Nothing!"))
          

          


          (if (eq? (player-vstate (world-player world)) 'left)
             (let player-left ((player (world-player world)))
               (player-posx-set! player (- (player-posx player) (* 0.3 delta-time)))))

          (if (eq? (player-vstate (world-player world)) 'right)
             (let player-right ((player (world-player world)))
               (player-posx-set! player (+ (player-posx player) (* 0.3 delta-time)))))
          

          (if (eq? (player-hstate (world-player world)) 'up)
              (let player-up ((player (world-player world)))
                (player-posy-set! player (- (player-posy player) (* 0.3 delta-time)))))

          (if (eq? (player-hstate (world-player world)) 'down)
              (if (and (< (player-posy (world-player world)) 360) (not (collision-tile (world-player world) (world-tile world))))
                 (let player-down ((player (world-player world)))
                   (player-posy-set! player (+ (player-posy player) (* 0.3 delta-time))))
                 ;(player-hstate-set! (world-player world) 'none)
                 ))
          
          

          ;; Drawing player
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (let drawing-player ((player (world-player world)))
            (cairo_rectangle cr (player-posx player) (player-posy player) (player-width player) (player-height player)))
          (cairo_fill cr)


          ))
       
   
      
       
       world))
   (make-world 
    'splashscreen
    (make-tile 200.0 350.0 50.0 50.0)
    'none)))