(define (hilbert-curve n)
  (define (a n)
    (if (<= n 0)
        '()
        (let ((a-next (a (- n 1)))
              (b-next (b (- n 1))))
          (append
            '(left)
            b-next
            '(forward right)
            a-next
            '(forward)
            a-next
            '(right forward)
            b-next
            '(left)))))
  (define (b n)
    (if (<= n 0)
        '()
        (let ((a-next (a (- n 1)))
              (b-next (b (- n 1))))
          (append
            '(right)
            a-next
            '(forward left)
            b-next
            '(forward)
            b-next
            '(left forward)
            a-next
            '(right)))))
  (a n))

(define (draw directives)
  (define (turn orientation directive)
    (cond ((eq? orientation 'left)
           (if (eq? directive 'left)
               'down
               'up))
          ((eq? orientation 'right)
           (if (eq? directive 'left)
               'up
               'down))
          ((eq? orientation 'up)
           (if (eq? directive 'left)
               'left
               'right))
          ((eq? orientation 'down)
           (if (eq? directive 'left)
               'right
               'left))
          (else
           (error "Unknown orientation" orientation))))
  (define (move orientation)
    (cond ((eq? orientation 'left)
           "h -1.25 ")
          ((eq? orientation 'right)
           "h 1.25 ")
          ((eq? orientation 'up)
           "v -1.25 ")
          ((eq? orientation 'down)
           "v 1.25 ")
          (else
           (error "Unknown orientation" orientation))))
  (define (inner orientation directives)
    (if (null? directives)
        ""
        (let* ((d (car directives))
               (s (if (eq? d 'forward)
                      (move orientation)
                      ""))
               (o (if (eq? d 'forward)
                      orientation
                      (turn orientation d))))
          (string-append
            s
            (inner o (cdr directives))))))
  (inner 'left directives))

(define hc (hilbert-curve 6))
(display hc)
(newline)
(display (draw hc))
(newline)
