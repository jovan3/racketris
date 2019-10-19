#lang racket/gui

(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))

(define text "ok")
(define board-width 10)
(define board-height 20)

(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-brush (make-object brush% "RED" 'solid))
(define blue-brush (make-object brush% "BLUE" 'solid))

(define (get-brush name)
  (cond
    [(equal? name "red") red-brush]
    [(equal? name "blue") blue-brush]
    [(equal? name "yellow") yellow-brush]))

(define tetris-board (make-hash)) ; mutable hash
(hash-set! tetris-board '(5 10) "blue")
(define shapes (list
                (list '(5 0) '(5 1) '(5 2) '(5 3))
                (list '(5 0) '(5 1) '(5 2) '(4 2))
                (list '(5 0) '(5 1) '(6 0) '(6 1))
                (list '(5 0) '(5 1) '(4 1) '(6 1))))
 

(define (random-shape l)
  (list-ref l (random (length l))))

(define current-shape (map (lambda (point)
                             (list point "red")) (random-shape shapes)))

(define current-shape-2 (apply hash (apply append (map (lambda (point)
                                       (list point "red")) (random-shape shapes)))))

(define (place-on-board board shape)
  (for ([s shape])
    (hash-set! board (car s) (cadr s))))

(define (move-shape shape dx dy)
  (make-hash (hash-map shape (lambda (k v)
                    (let ([x (car k)]
                          [y (cadr k)])
                      (cons (list (+ x dx) (+ y dy)) v))))))

(define (move-shape-down shape)
  (move-shape shape 0 1))

(define (move-shape-left shape)
  (move-shape shape -1 0))

(define (move-shape-right shape)
  (move-shape shape 1 0))

(define (outside-board? x y)
  (cond
    [(< x 0) #t]
    [(> x board-width) #t]
    [(> y board-height) #t]
    [else #f]))


(define (contains? el l)
  (cond
    [(empty? l) #f]
    [(equal? (car l) el) #t]
    [else (contains? el (rest l))]))

(define (shape-collides? coords board)
  (hash-has-key? board coords))

(define (shape-piece-collides? piece board)
  (let ([x (car piece)]
        [y (cadr piece)])
    (if (outside-board? x y)
        #f
        (not (shape-collides? piece board)))))

(define (can-move? shape board dx dy)
  (let ([test-shape (move-shape shape dx dy)])
    (not (contains? #f
                    (hash-map test-shape (lambda (k v)
                                           (shape-piece-collides? k board)))))))

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
  (println "-------")
  (println current-shape-2)
  (draw-board current-shape-2 ctx)

  (set! current-shape-2 (move-shape-down current-shape-2))
  (println current-shape-2)
  (println (can-move? current-shape-2 tetris-board 0 1))
  (sleep/yield 1)
  (loop (+ x 1) (+ y 1)))

(loop 0 0)
