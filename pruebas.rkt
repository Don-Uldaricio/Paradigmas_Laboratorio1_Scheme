#lang racket

;TDA PIXEL--------------------------------------------------------

;Crear pixbit
(define (pixbit-d x y bit depth) (list x y bit depth))
;(pixbit-d 0 1 1 2)

(define (pixbit? pixel)
  (if (and
       (= (length pixel) 4)
       (or (= (list-ref pixel 2) 0) (= (list-ref pixel 2) 1))) #t #f))

(define pixb (pixbit-d 0 2 1 25))
pixb
(pixbit? pixb)


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
       (= (length (list-ref pixel 2) 7))) #t #f))

;(pixhex-d 0 1 1 2)

;TDA IMAGE---------------------------------------------------------
;Crear image (pixmap/pixrgb/pixhex)
(define (image width height . pixlist) (list width height pixlist))

#|
;Funciones booleanas (bitmap? / pixmap? / hexmap?)
(define (bitmap? imagen)
  (if (and (pixbit? (list-ref (img-pixlist imagen) 0))
           (= (length (img-pixlist imagen)) (* (img-width imagen) (img-height imagen)))) #t #f))

(define (rgbmap? imagen)
  (if (and (pixrgb? (list-ref (img-pixlist imagen) 0))
           (= (length (img-pixlist imagen)) (* (img-width imagen) (img-height imagen)))) #t #f))

(define (hexmap? imagen)
  (if (and (pixrgb? (list-ref (img-pixlist imagen) 0))
           (= (length (img-pixlist imagen)) (* (img-width imagen) (img-height imagen)))) #t #f))

(define img1 (image 2 2 (pixbit-d 0 0 1 2) (pixbit-d 0 1 1 2) (pixbit-d 1 0 1 2) (pixbit-d 1 1 1 2)))
(bitmap? img1)
(rgbmap? img1)

#|
(define (fua x y . l) (list x y l))
(define jeje (fua 1 2 (list 3 4 5 6) (list "ab" 2)))
(list-ref jeje 0)
(list-ref jeje 1)
(list-ref jeje 2)
(list-ref (list-ref jeje 2) 0)
(list-ref (list-ref jeje 2) 1)
|#
|#