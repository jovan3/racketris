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

(define shapes (list
                (list '(5 0) '(5 1) '(5 2) '(5 3))
                (list '(5 0) '(5 1) '(5 2) '(4 2))
                (list '(5 0) '(5 1) '(6 0) '(6 1))
                (list '(5 0) '(5 1) '(4 1) '(6 1))))
 

(define (random-shape l)
  (list-ref l (random (length l))))

(define (new-shape) (apply hash (apply append (map (lambda (point)
                                       (list point "red")) (random-shape shapes)))))

(define current-shape (new-shape))

(define (place-on-board board shape)
  (println "placing")
  (println shape)
  (hash-for-each shape (lambda (k v)
                         (hash-set! board k v))))

(define (move-shape shape dx dy)
  (make-hash (hash-map shape (lambda (k v)
                    (let ([x (car k)]
                          [y (cadr k)])
                      (cons (list (+ x dx) (+ y dy)) v))))))

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
      (let ([code (send key-event get-key-code)])
      (let ([new-direction (cond
                             [(equal? code 'down) "down"]
                             [(equal? code 'left) "left"]
                             [(equal? code 'right) "right"])])
        (move new-direction current-shape tetris-board))))
    (super-new)))

(define can (new keyboard-canvas% [parent frame]
                 [paint-callback
                  (lambda (canvas dc)
                    (send dc set-scale 3 3)
                    (send dc set-text-foreground "blue")
                    (send dc draw-text "Don't Panic!" 0 0))]))

(send frame show #t)
(define ctx (send can get-dc))

(define (direction->delta direction)
  (cond
    [(equal? "left" direction) '(-1 0)]
    [(equal? "right" direction) '(1 0)]
    [else '(0 1)]))

(define (move direction shape board)
  (let ([deltas (direction->delta direction)])
    (let ([dx (car deltas)]
          [dy (cadr deltas)])
      (println (can-move? shape board dx dy))
      (if (can-move? shape board dx dy)
          (set! current-shape (move-shape current-shape dx dy))
          (begin
            (place-on-board board shape)
            (set! current-shape (new-shape))))))
  (draw))
      
(define (draw)
  (send ctx clear)
  (draw-board tetris-board ctx)
  (draw-board current-shape ctx))

(define (loop x y)
  (draw)
  
  ;(println "-------")
  ;(println current-shape)
  (move "down" current-shape tetris-board)
  
  (sleep/yield 1)
  (loop (+ x 1) (+ y 1)))

(loop 0 0)
