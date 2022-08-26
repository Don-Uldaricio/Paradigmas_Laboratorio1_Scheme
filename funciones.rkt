#lang racket

;TDA PIXEL--------------------------------------------------------

;Crear pixbit
(define (pixbit-d x y bit depth) (list x y bit depth))

(define (pixbit? pixel)
  (if (and
       (= (length pixel) 4)
       (or (= (list-ref pixel 2) 0) (= (list-ref pixel 2) 1))) #t #f))

;Crear pixrgb
(define (pixrgb-d x y r g b depth) (list x y r g b depth))

(define (pixrgb? pixel)
  (if (and
       (= (length pixel) 6)
       (and (>= (list-ref pixel 2) 0) (<= (list-ref pixel 2) 255))
       (and (>= (list-ref pixel 3) 0) (<= (list-ref pixel 3) 255))
       (and (>= (list-ref pixel 4) 0) (<= (list-ref pixel 4) 255)))#t #f))

;Crear pixhex
(define (pixhex-d x y hex depth) (list x y hex depth))

(define (pixhex? pixel)
  (if (and
       (= (length pixel) 4)
       (string? (list-ref pixel 2))
       (= (string-length (list-ref pixel 2)) 7)) #t #f))

;TDA IMAGE---------------------------------------------------------

;Crear image (pixmap/pixrgb/pixhex)
(define (image width height . pixlist) (list width height pixlist))

;Funciones booleanas (bitmap? / pixmap? / hexmap?)
(define (bitmap? imagen)
  (if (and (pixbit? (list-ref (list-ref imagen 2) 0))
           (= (length (list-ref imagen 2)) (* (list-ref imagen 0) (list-ref imagen 1)))) #t #f))

(define (pixmap? imagen)
  (if (and (pixrgb? (list-ref (list-ref imagen 2) 0))
           (= (length (list-ref imagen 2)) (* (list-ref imagen 0) (list-ref imagen 1)))) #t #f))

(define (hexmap? imagen)
  (if (and (pixhex? (list-ref (list-ref imagen 2) 0))
           (= (length (list-ref imagen 2)) (* (list-ref imagen 0) (list-ref imagen 1)))) #t #f))

;TDA IMAGE ---- MODIFICADORES

;Voltear Horizontalmente
(define (flipH imagen)
  (list (car imagen) (cadr imagen) (flipHpixels (list-ref imagen 2) (- (car imagen) 1))))

(define (flipHpixel pixel width)
  (cons (- width (car pixel)) (cdr pixel)))

(define (flipHpixels pixlist width)
  (if (null? pixlist)
      null
      (cons (flipHpixel (car pixlist) width) (flipHpixels (cdr pixlist) width))))

;Voltear Verticalmente
(define (flipV imagen)
  (list (car imagen) (cadr imagen) (flipVpixels (list-ref imagen 2) (- (cadr imagen) 1))))

(define (flipVpixel pixel height)
  (append (list (car pixel) (- height (cadr pixel))) (cddr pixel)))

(define (flipVpixels pixlist height)
  (if (null? pixlist)
      null
      (cons (flipVpixel (car pixlist) height) (flipVpixels (cdr pixlist) height))))


(provide (all-defined-out))
