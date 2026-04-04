(use-modules (srfi srfi-1))
(define pi (* 4 (atan 1)))

(define (make-square n)
  (let* ((side (quotient n 4))
      (top    (map (lambda (i) (make-rectangular (- (* 2 (/ i side)) 1)  1)) (iota side)))
      (right  (map (lambda (i) (make-rectangular  1 (- 1 (* 2 (/ i side))))) (iota side)))
      (bottom (map (lambda (i) (make-rectangular (- 1 (* 2 (/ i side))) -1)) (iota side)))
      (left   (map (lambda (i) (make-rectangular -1 (- (* 2 (/ i side)) 1))) (iota side)))
    )
    (append top right bottom left)
  )
)

(define data (make-square 256))

(define (dft input)
  (define N (length input))
  (map
    (lambda (k)
      (apply +
        (map
          (lambda (n)
            (* (list-ref input n) (exp (/ (* 0-1i 2 pi k n) N)))
          ) 
          (iota N)
        )
      )
    )
    (iota N)
  )
)

(define dft_out (dft data))
(display dft_out)
(newline)

(define (extract_freq_amp_phase input)
  (define N (length input))
  (map
    (lambda (k) (list k (/ (magnitude (list-ref input k)) N) (angle (list-ref input k))))
    (iota N)
  )
)

(define circles (extract_freq_amp_phase dft_out))
(display circles)
(newline)

(define sorted_circles (sort circles (lambda (a b) (> (cadr a) (cadr b)))))
(display sorted_circles)
(newline)

(define (calc_tip circles t)
  (apply +
    (map
      (lambda (circle)
        (let ((freq (car circle))
            (amp  (cadr circle))
            (phase (caddr circle)))
        (make-polar amp (+ phase (* freq 2 pi t))))
      )
      circles
    )
  )
)

(display (calc_tip sorted_circles 0))
(newline)

(define num_t 512)
(define t (iota num_t 0 (/ 1.0 num_t)))
(define result (map (lambda (t) (calc_tip sorted_circles t)) t))

(display result)
(newline)

(define (black_image_grid width height)
  (map
    (lambda (_)
      (map
        (lambda (_) (list 255 255 255))
        (iota height)
      )
    )
    (iota width)
  )
)

(define (to_pixel val size)
  (inexact->exact (round (+ (/ size 2) (* val (/ size 2)))))
)

(define (path_hits? result px py size)
  (any
    (lambda (point)
      (and (= px (to_pixel (real-part point) size))
           (= py (to_pixel (imag-part point) size))))
    result
  )
)

(define (render_path result width height)
  (map
    (lambda (py)
      (map
        (lambda (px)
          (if (path_hits? result px py width)
            (list 0 0 0)
            (list 255 255 255)
          )
        )
        (iota width)
      )
    )
    (iota height)
  )
)

(define (save_ppm_image path width height max_pixel_value data)
  (with-output-to-file path
    (lambda ()
      (display "P3")
      (newline)
      (display width) (display " ") (display height)
      (newline)
      (display max_pixel_value)
      (newline)
      (map 
        (lambda (pixel_value)
          (let ((r (car pixel_value))
            (g  (cadr pixel_value))
            (b (caddr pixel_value)))
            (display r) (display " ") (display g) (display " ") (display b)
            (newline)
          )
        )
        data
      )
    )
  )
)

(define width 512)
(define height 512)
(define pixels (render_path result width height))
(define flat_pixels (apply append pixels))
(save_ppm_image "output.ppm" width height 255 flat_pixels)
