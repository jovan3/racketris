#lang racket/gui

(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))

(define text "ok")

(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-brush (make-object brush% "BLUE" 'solid))
(define blue-brush (make-object brush% "RED" 'solid))

(define tetris-board (hash
                      '(1 1) "red"
                      '(1 2) "red"
                      '(2 3) "blue"
                      '(4 7) "yellow"))

(define (get-brush name)
  (cond
    [(equal? name "red") red-brush]
    [(equal? name "blue") blue-brush]
    [(equal? name "yellow") yellow-brush]))

(define (draw-square-block canvas square-brush position)
  (let ([start (car position)]
        [end (cadr position)]
        [old-brush (send canvas get-brush)])
    (let ([x0 (car start)]
          [y0 (cadr start)]
          [x1 (car end)]
          [y1 (cadr end)])
      (send canvas set-brush square-brush)
      (send canvas draw-rectangle x0 y0 x1 y1)
      (send canvas set-brush old-brush))))

(define (draw-board board canvas)
  (hash-for-each board (lambda (k v)
                         (let ([x (car k)]
                               [y (cadr k)])
                           (let ([x0 (* x 10)]
                                 [y0 (* y 10)])
                             (draw-square-block canvas
                                                (get-brush v)
                                                (list
                                                 (list x0 y0)
                                                 (list 10 10))))))))

(define keyboard-canvas%
  (class canvas%
    (define/override (on-char key-event)
      (cond
        [(equal? (send key-event get-key-code) 'up) (set! text "up")]
        [(equal? (send key-event get-key-code) 'down) (set! text "down")]))
    (super-new)))

(define can (new keyboard-canvas% [parent frame]
                 [paint-callback
                  (lambda (canvas dc)
                    (send dc set-scale 3 3)
                    (send dc set-text-foreground "blue")
                    (send dc draw-text "Don't Panic!" 0 0))]))

(send frame show #t)
(define ctx (send can get-dc))

(define (loop x y)
  (send ctx clear)
  ;(draw-square-block ctx yellow-brush (list (list 10 10) (list 20 20)))
  ;(draw-square-block ctx red-brush (list (list 40 40) (list 70 70)))
  ;(draw-square-block ctx blue-brush (list (list 80 80) (list 90 90)))
  ;(draw-square-block ctx blue-brush (list (list 40 50) (list 10 10)))
  ;(draw-square-block ctx blue-brush (list (list 50 60) (list 10 10)))
  
  
  ;(send ctx draw-text text x y)
  (draw-board tetris-board ctx)
  
  (sleep/yield 1)
  (loop (+ x 1) (+ y 1)))

(loop 0 0)
