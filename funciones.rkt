#lang racket

;TDA PIXEL--------------------------------------------------------

;Crear pixbit
(define (pixbit-d x y bit depth) (list x y bit depth))

(define (pixbit? pixel)
  (if (and
       (or (= (list-ref pixel 2) 0) (= (list-ref pixel 2) 1))) #t #f))

;Crear pixrgb
(define (pixrgb-d x y r g b depth) (list x y r g b depth))

(define (pixrgb? pixel)
  (if (and
       (and (>= (list-ref pixel 2) 0) (<= (list-ref pixel 2) 255))
       (and (>= (list-ref pixel 3) 0) (<= (list-ref pixel 3) 255))
       (and (>= (list-ref pixel 4) 0) (<= (list-ref pixel 4) 255)))#t #f))

;Crear pixhex
(define (pixhex-d x y hex depth) (list x y hex depth))

(define (pixhex? pixel)
  (if (and
       (string? (list-ref pixel 2))
       (= (string-length (list-ref pixel 2)) 7)) #t #f))

; TDA IMAGE -----------------------------------------------------

; CONSTRUCTORES -------------------------------------------------

; Dominio: Width (int) X Height (int) X [pixbit-d* |  pixrgb-d* | pixhex-d*]
; Recorrido: image
; Descripción: Crea una una imagen de tipo pixmap/bitmap/hexmap
;              poniéndolos en una lista
; Recursión: No aplica
(define (image width height . pixlist)
  (if (and (integer? width) (> width 0) (integer? height) (> height 0))
      (list width height pixlist)
      null))

; MODIFICADORES -------------------------------------

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma horizontal
; Recursión: No aplica
(define (flipH img)
  (list (car img) (cadr img) (flipHpixels (list-ref img 2) (- (car img) 1))))

; Dominio: pixel X int
; Recorrido: pixel
; Descripción: Cambia de posición un pixel volteado horizontalmente
; Recursión: No aplica
(define (flipHpixel pixel width)
  (cons (- width (car pixel)) (cdr pixel)))

; Dominio: pixbit-d* / pixrgb-d* / pixhex-d* X int
; Recorrido: pixbit-d* / pixrgb-d* / pixhex-d*
; Descripción: Cambia de posición una lista de pixeles volteados horizontalmente
; Recursión: No aplica
(define (flipHpixels pixlist width)
  (if (null? pixlist)
      null
      (cons (flipHpixel (car pixlist) width) (flipHpixels (cdr pixlist) width))))

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma vertical
; Recursión: No aplica
(define (flipV img)
  (list (car img) (cadr img) (flipVpixels (list-ref img 2) (- (cadr img) 1))))

; Dominio: pixel X int
; Recorrido: pixel
; Descripción: Cambia de posición un pixel volteado verticalmente
; Recursión: No aplica
(define (flipVpixel pixel height)
  (append (list (car pixel) (- height (cadr pixel))) (cddr pixel)))

; Dominio: pixbit-d* / pixrgb-d* / pixhex-d* X int
; Recorrido: pixbit-d* / pixrgb-d* / pixhex-d*
; Descripción: Cambia de posición una lista de pixeles volteados verticalmente
; Recursión: No aplica
(define (flipVpixels pixlist height)
  (if (null? pixlist)
      null
      (cons (flipVpixel (car pixlist) height) (flipVpixels (cdr pixlist) height))))

; Dominio: pixel (pixrgb-d / pixbit-d / pixhex-d)
; Recorrido: pixel (pixrgb-d / pixbit-d / pixhex-d)
; Descripción: Rota un pixel
; Recursión: No aplica
; APLICAR MATRIZ DE ROTACIÓN PARA ROTAR PIXELES

;TDA IMAGE ---- PERTENENCIA

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen está comprimida
; Recursión: No aplica
(define (compressed? img)
  (if (< (length (list-ref img 2)) (* (car img) (cdar img))) #t #f))

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen es bitmap
; Recursión: No aplica
(define (bitmap? img)
  (if (pixbit? (list-ref (list-ref img 2) 0)) #t #f))

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen es pixmap
; Recursión: No aplica
(define (pixmap? img)
  (if (pixrgb? (list-ref (list-ref img 2) 0)) #t #f))

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen es hexmap
; Recursión: No aplica
(define (hexmap? img)
  (if (pixhex? (list-ref (list-ref img 2) 0)) #t #f))

(provide (all-defined-out))
