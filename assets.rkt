#lang racket/gui

(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl")
         ffi/cvector ffi/unsafe ffi/unsafe/cvector
         )
(require "math.rkt")
(struct texture (id w h sw sh (x-flip? #:mutable)))
(define (load-image fn)
  (let ([b (make-object bitmap% fn 'unknown/alpha)])
    (if (send b ok?) b (raise (string-append "failed to load image " fn)))))

(define texture-paths (make-hash))
(define (register-image name begin-frame end-frame path #:offset [offset (vec 0 0)] #:scale [scale 1] #:x-flip? [x-flip? #f])
  (define (cv f)
    (cond [(equal? f 'begin) 0]
           [(equal? f 'end) (sub1 (length path))]
           [else f]))
  (hash-set! texture-paths name path)
  (hash-set! texture-xf name x-flip?)
  (hash-set! texture-transforms name (if (symbol? path) (texture-transform path) (lin offset scale 0)))
  (hash-set! texture-frames name (cons (cv begin-frame) (cv end-frame))))

(define texture-store (make-hash)) ; 'image-name -> handle for the image stored on the gfx card memory
(define image-store (make-hash)) ; 'image-name -> raw argb data in RAM

(define (image-by-name name)
  (image-data (hash-ref! image-store name
                         (lambda ()
                           (let ([path (hash-ref texture-paths name)])
                             (if (symbol? path)
                                 (image-by-name path)
                                 (load-image (car path))))))))

(define (texture-by-name name)
  (hash-ref! texture-store name
            (lambda ()
              (let ([path (hash-ref texture-paths name)])
                (if (symbol? path)
                    (let ([tt (struct-copy texture (texture-by-name path))]) (set-texture-x-flip?! tt (hash-ref texture-xf name)) tt)
                    (load-tex path))))))
(define (image-w image-name)
  (texture-w (texture-by-name image-name)))
(define (image-h image-name)
  (texture-h (texture-by-name image-name))); equally as bad as below

(define (get-pixel image-name pos)
  (let* ([tex (texture-by-name image-name)]; inefficient, we force the loading of the tex in the gfx mem
         [w (texture-w tex)] [h (texture-h tex)]
         [i0 (* 4 (+ (vec-x pos) (* w (- h (vec-y pos)))))]
         [b (image-by-name image-name)])
    (if (>= (+ i0 4) (bytes-length b))
        (list 128 128 128 128)
        (for/list ([i (in-range i0 (+ i0 4))])
          (bytes-ref b i)))))

; todo remove all the dirty hashes
(define texture-frames (make-hash))
(define texture-transforms (make-hash))
(define texture-xf (make-hash))
(define (texture-transform name) (hash-ref texture-transforms name))
(define (texture-begin-end name)
  (hash-ref texture-frames name))



(define (texture-x-coord tex frame)
  (modulo frame (round (/ 1 (texture-sw tex)))))
(define (texture-y-coord tex frame)
  (quotient frame (round (/ 1 (texture-sw tex)))))
;from racketgl
(define (argb->rgba! pixels)
  (define r (/ (gl-vector-length pixels) 4))
  (for ((i (in-range r)))
       (let* ((offset (* 4 i))
              (alpha (gl-vector-ref pixels offset))
              (red (gl-vector-ref pixels (+ 1 offset)))
              (green (gl-vector-ref pixels (+ 2 offset)))
              (blue (gl-vector-ref pixels (+ 3 offset))))
         (gl-vector-set! pixels offset (quotient (* alpha red) 255))
         (gl-vector-set! pixels (+ 1 offset) (quotient (* alpha green) 255))
         (gl-vector-set! pixels (+ 2 offset) (quotient (* alpha blue) 255))
         (gl-vector-set! pixels (+ 3 offset) alpha))))
(define (ti n) (bitwise-and #xFFFFFFFF n))
(define (make-tex-argb w h data)
  (let* ([tex (gl-vector-ref (glGenTextures 1) 0)]
        [n (length data)]
        [line-size (inexact->exact (ceiling (sqrt n)))]
        [line-count (ceiling (/ n line-size))])
    (glEnable GL_TEXTURE_2D)
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
    (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (* w line-size) (* h line-count) 0 GL_RGBA GL_UNSIGNED_BYTE (make-cvector _ubyte 0))
    (displayln line-count)
    (printf "WH ~a ~a\n" w h)
    (for ([frame data] [i (in-range 0 n)])
      (glTexSubImage2D GL_TEXTURE_2D 0 (* (modulo i line-size) w) (* (quotient i line-size) h) w h GL_BGRA GL_UNSIGNED_BYTE frame))
    (texture tex w h (/ 1 line-size) (/ 1 line-count) #f)))
    ;k(texture tex w h 1 1)))


; load image data by accessing cario_surface because it's already in RGBA there
(require racket/draw/unsafe/cairo)
(define (image-data bmp)
     (let ([b (cairo_image_surface_get_data (send bmp get-handle))])
  ;k(let* ([size (* (send bmp get-width) (send bmp get-height) 4)]
    ;k    [b (make-bytes size)])
   ;k (send bmp get-argb-pixels 0 0 (send bmp get-width) (send bmp get-height) b #f #t)
;    (printf "bytes ~a ~a\n" (bytes-length b) (bytes->list (subbytes b 0 12)))
  b))

(define (load-tex fns)
  (printf ":loadt ~a\n" fns)
  (let ([images (for/list ([fn fns]) ; build a list of '(w h image-data)
                  (let* ([bmp (load-image fn)]
                         [pixels (image-data bmp)])
                    (list (send bmp get-width) (send bmp get-height) pixels bmp)))])
    
    (let ([w (caar images)] [h (cadar images)])
      (when (not (andmap (lambda img (and (= (caar img) w)) (= (cadar img) h)) images))
        (raise "all images must be of the same size"))
      (make-tex-argb w h (map  (lambda (b) (make-cvector* b _ubyte (bytes-length b))) (map caddr images))))))

(provide (struct-out texture)
         get-pixel texture-begin-end texture-transform register-image texture-by-name load-tex texture-x-coord texture-y-coord image-by-name load-image image-w image-h)
