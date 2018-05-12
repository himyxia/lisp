;; painter
;; wave
;; frame
;;
;; beside(painter1, painter)
;; below(painter1, painter)
;; flip-vert(painter)
;; flip-horiz(painter)

;;;;;;;;;;;;;;;;;;;;;;;;;;vector
(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr (car v)))

(define (add-vect v1 v2) 
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
           
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
           
(define (scale-vect v factor)
  (make-vect (* (xcor-vect v)) 
             (* (ycor-vect v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;vector_end

;;;;;;;;;;;;;;;;;;;;;;;;;;segment_start
(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (car (cdr segment)))
;;;;;;;;;;;;;;;;;;;;;;;;;;segment_end


;;;;;;;;;;;;;;;;;;;;;;;frame_start
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))
;;;;;;;;;;;;;;;;;;;;;;;frame_end

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect y) (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment) (draw-line
                                  ((frame-coord-map frame) (start-segment segment))
                                  ((frame-coord-map frame) (end-segment segment)))) 
                                segment-list)))


(define (outline-frame frame) 
  (let ((origin (origin-frame frame))
        (edge1-offset (edge1-frame frame))
        (edge2-offset (edge2-frame frame))
        (point1 (add-vect origin edge1-offset))
        (point2 (add-vect origin edge2-offset)))
  (segments-painter (list (make-segment origin point1) 
                          (make-segment origin point2) 
                          (make-segment point1 (add-vect point1 edge2-offset))
                          (make-segment point2 (add-vect point2 edge1-offset))))))

(define (x-draw frame)
  (let ((origin (origin-frame frame))
        (edge1-offset (edge1-frame frame))
        (edge2-offset (edge2-frame frame))
        (point1 (add-vect origin edge1-offset))
        (point2 (add-vect origin edge2-offset)))
  (segments-painter (list (make-segment point1 point2)
                          (make-segment origin (add-vect point1 point2))))))
                        
(define (diamon frame)
  (let ((origin (origin-frame frame))
        (edge1-offset (edge1-frame frame))
        (edge2-offset (edge2-frame frame))
        (point1 (add-vect origin edge1-offset))
        (point2 (add-vect origin edge2-offset))
        (point3 (add-vect point1 point2))
        (vec1 (add-vect origin (/ edge1-offset 2)))
        (vec2 (add-vect origin (/ edge2-offset 2)))
        (vec3 (add-vect point1 (/ edge2-offset 2)))
        (vec4 (add-vect point2 (/ edge1-offset 2))))
  (segments-painter (list (make-segment vec1 vec2)
                          (make-segment vec1 vec3)
                          (make-segment vec2 vec4)
                          (make-segment vec3 vec4)))))
  

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

    
(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
           (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
           (below painter (beside up up)))))

(define (split func1 func2)
  (lambda (painter n)
    (if (= n 1)
        painter
        (let ((smaller ((split func1 func2) painter (- n 1))))
          (func1 painter (func2 smaller smaller))))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
           (let ((top-left (beside up up))
                (bottom-right (below right right))
                (corner (corner-split painter (- n 1)))
               (beside (below painter top-left)
                       (below bottom-right corner)))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
       (let ((half (beside (flip-horiz quarter) quarter)))
            (below (flip-vert half) half))))
          
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
       (below painter2 painter2)))
     
(define (square-of-four tl tr bl br)
  (lambad (painter)
          (let ((top (beside (tl painter) (tr painter)))
                (bettom (beside (bl painter) (bl painter))))
              (below bottom top))))
(define (identity x)
  x)

(define (flipped-pairs painter)
  (let (combine (square-of-four identity flip-vert identity flip-vert))
       (combine painter)))
  
(define flipped-pairs (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
       (let ((combine (square-of-four flip-vert 
                                      (lambda (painter) (flip-vert (flip-horiz painter))) 
                                      identity
                                      flip-horiz)))
                                    combine quarter)))
                      
                      
                      
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
         (let ((new-origin (m origin)))
              (painter
                (make-frame new-origin 
                            (sub-vect (m corner1) new-origin)
                            (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter 
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.5)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
                   
(define (beside painter1 painter2)
  (let ((split-point (make-rect 0.5 0.0)))
       (let ((paint-left 
                         (tranform-painter painter1
                                           (make-rect 0.0 0.0)
                                           split-point
                                           (make-rect 0.0 1.0)))
            (paint-right
                          (tranform-painter painter2
                                            split-point
                                            (make-rect 1.0 0.0)
                                            (make-rect 0.5 1.0))))
            (lambda (frame)
              (paint-left frame)
              (paint-right frame)))))

(define (flip-horiz painter)
    (transform-painter painter (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter 
                     (make-vect 1.0 1.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))
                   
(define (rotate270 painter)
  (transform-painter painter 
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (below painter1 painter2)
  (let ((split-point (make-rect 0.0 0.5)))
       (let ((paint-bottom 
                         (tranform-painter painter1
                                           (make-rect 0.0 0.0)
                                           split-point
                                           (make-rect 1.0 0.0)))
            (paint-upper
                          (tranform-painter painter2
                                            split-point
                                            (make-rect 0.0 1.0)
                                            (make-rect 1.0 0.5))))
            (lambda (frame)
              (paint-bottom frame)
              (paint-upper frame)))))
