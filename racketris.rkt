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

(define colors (list "red" "blue" "yellow"))

(define (random-element l)
  (list-ref l (random (length l))))

(define (new-shape)
  (let ([color (random-element colors)])
    (apply hash
           (apply append (map (lambda (point)
                                (list point color)) (random-element shapes))))))

(define current-shape (new-shape))

(define (place-on-board board shape)
  (hash-for-each shape (lambda (k v)
                         (hash-set! board k v))))

(define (move-shape shape dx dy)
  (make-hash (hash-map shape (lambda (k v)
                    (let ([x (car k)]
                          [y (cadr k)])
                      (cons (list (+ x dx) (+ y dy)) v))))))

(define (shape-center shape)
  (let ([x-coords (map car (map car (hash->list shape)))]
        [y-coords (map cadr (map car (hash->list shape)))])
    (let ([mean-fn (lambda (l) (ceiling (/ (apply + l) 4)))])
      (list (- (mean-fn x-coords) 1) (mean-fn y-coords)))))

(define (rotate-shape-piece-fn pivot-x pivot-y)
  (lambda (k v)
    (let ([x (car k)]
          [y (cadr k)])
      (cons (list (+ pivot-x pivot-y (- y))
                  (+ pivot-y (- pivot-x) x)) v))))

(define (rotate-shape shape)
  (let ([pivot (shape-center shape)])
    (let ([pivot-x (car pivot)]
          [pivot-y (cadr pivot)])
      (move-shape
       (make-hash (hash-map shape (rotate-shape-piece-fn pivot-x pivot-y))) 0 -1))))

(define (rotate-shape!)
  (let ([rotated-shape (rotate-shape current-shape)])
    (when (not (contains? #t (hash-map rotated-shape (can-move-test tetris-board))))
      (set! current-shape rotated-shape)))
  (draw))

(define (contains? el l)
  (cond
    [(empty? l) #f]
    [(equal? (car l) el) #t]
    [else (contains? el (rest l))]))

(define (shape-piece-collides? piece board)
  (hash-has-key? board piece))

(define (shape-piece-outside? piece)
  (let ([x (car piece)]
        [y (cadr piece)])
    (cond
      [(< x 0) #t]
      [(> x board-width) #t]
      [(> y board-height) #t]
      [else #f])))

(define (shape-piece-hits-bottom? piece)
  (let ([y (cadr piece)])
    (> y board-height)))

(define (can-move-test board)
  (lambda (k v)
    (or
     (shape-piece-outside? k)
     (shape-piece-collides? k board))))

(define (freeze-test board)
  (lambda (k v)
    (or
     (shape-piece-collides? k board)
     (shape-piece-hits-bottom? k))))

(define (can-move? shape board dx dy test-fn)
  (let ([test-shape (move-shape shape dx dy)])
    (not (contains? #t (hash-map test-shape test-fn)))))

(define (should-freeze? shape board dx dy)
  (let ([test-shape (move-shape shape dx dy)])
    (if (equal? dy 1)
        (not (can-move? test-shape board dx dy (freeze-test board)))
        #f)))

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
        (if (equal? code 'up)
            (rotate-shape!)
            (let ([new-direction (cond
                                   [(equal? code 'down) "down"]
                                   [(equal? code 'left) "left"]
                                   [(equal? code 'right) "right"])])
              (move new-direction current-shape tetris-board)))))
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
      (if (can-move? shape board dx dy (can-move-test board))
          (set! current-shape (move-shape current-shape dx dy))
          (when (should-freeze? shape board dx dy)
            (begin
              (place-on-board board shape)
              (set! current-shape (new-shape)))))))
  (draw))
      
(define (draw)
  (send ctx clear)
  (draw-board tetris-board ctx)
  (draw-board current-shape ctx))

(define (loop x y)
  (draw)
  (move "down" current-shape tetris-board)
  (sleep/yield 1)
  (loop (+ x 1) (+ y 1)))

(loop 0 0)
