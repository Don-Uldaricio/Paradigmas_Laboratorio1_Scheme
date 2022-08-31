#lang racket

; TDA PIXELES -----------------------------------------------------

; CONSTRUCTORES --------------------------------------------------

; Dominio: int X int X int X int
; Recorrido: pixbit-d
; Descripción: Crea un pixbit-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixbit-d x y bit depth) (list x y bit depth))

; Dominio: int X int X int X int X int X int
; Recorrido: pixrgb-d
; Descripción: Crea un pixrgb-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixrgb-d x y r g b depth) (list x y (list r g b) depth))

; Dominio: int X int X int X int
; Recorrido: pixhex-d
; Descripción: Crea un pixhex-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixhex-d x y hex depth) (list x y hex depth))

; PERTENENCIA ---------------------------------------------------

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixbit-d
; Recursión: No aplica
(define (pixbit? pixel)
  (if (or (= (list-ref pixel 2) 0) (= (list-ref pixel 2) 1)) #t #f))

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixrgb-d
; Recursión: No aplica
(define (pixrgb? pixel)
  (if (and
       (and (>= (first (third pixel)) 0) (<= (first (third pixel)) 255))
       (and (>= (second (third pixel)) 0) (<= (second (third pixel)) 255))
       (and (>= (third (third pixel)) 0) (<= (third (third pixel)) 255)))#t #f))

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixhex-d
; Recursión: No aplica
(define (pixhex? pixel)
  (if (and
       (string? (list-ref pixel 2))
       (= (string-length (list-ref pixel 2)) 7)) #t #f))

; SELECTORES ----------------------------------------------------

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la posición x de un pixel de cualquier tipo
; Recursión: No aplica
(define (posX pixel)
  (car pixel))

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la posición y de un pixel de cualquier tipo
; Recursión: No aplica
(define (posY pixel)
  (cadr pixel))

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la profundidad de un pixel de cualquier tipo
; Recursión: No aplica
(define (getDepth pixel)
  (fourth pixel))

; Dominio: pixbit-d
; Recorrido: int
; Descripción: Entrega el valor del bit de un pixbit-d
; Recursión: No aplica
(define (getBit pixel)
  (if (pixbit? pixel) (third pixel) null))

; Dominio: pixrgb-d
; Recorrido: '(Red (int) X Green (int) X Blue (int))
; Descripción: Entrega una lista con los valores RGB de un pixrgb-d
; Recursión: No aplica
(define (rgbChannel pixel)
  (third pixel))

; Dominio: pixrgb-d
; Recorrido: Red (int)
; Descripción: Entrega el canal rojo de un pixrgb-d
; Recursión: No aplica
(define (redChannel pixel)
  (first (rgbChannel pixel)))

; Dominio: pixrgb-d
; Recorrido: Green (int)
; Descripción: Entrega el canal verde de un pixrgb-d
; Recursión: No aplica
(define (greenChannel pixel)
  (second (rgbChannel pixel)))

; Dominio: pixrgb-d
; Recorrido: Blue (int)
; Descripción: Entrega el canal azul de un pixrgb-d
; Recursión: No aplica
(define (blueChannel pixel)
  (third (rgbChannel pixel)))

; TDA IMAGE -----------------------------------------------------

; CONSTRUCTORES -------------------------------------------------

; Dominio: Width (int) X Height (int) X [pixbit-d* |  pixrgb-d* | pixhex-d*]
; Recorrido: image
; Descripción: Crea una una imagen de tipo pixmap/bitmap/hexmap
;              con cada pixel en una lista
; Recursión: No aplica
(define (image width height . pixlist)
  (if (and (integer? width) (> width 0) (integer? height) (> height 0))
      (list width height pixlist)
      null))

; PERTENENCIA ------------------------------------------------------

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

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen está comprimida
; Recursión: No aplica
(define (compressed? img)
  (if (< (length (list-ref img 2)) (* (car img) (cdar img))) #t #f))

; SELECTORES ----------------------------------------

; Dominio: image
; Recorrido: int
; Descripción: Entrega el ancho (width) de una imagen (image)
; Recursión: No aplica
(define (getWidth img)
  (first img))

; Dominio: image
; Recorrido: int
; Descripción: Entrega el alto (height) de una imagen (image)
; Recursión: No aplica
(define (getHeight img)
  (second img))

; Dominio: image
; Recorrido: pixbit-d* | pixrgb-d* | pixhex-d*
; Descripción: Entrega la lista de pixeles de una imagen
; Recursión: No aplica
(define (getPixels img)
  (third img))

; MODIFICADORES -------------------------------------

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma horizontal
; Recursión: No aplica
(define (flipH img)
  (list (getWidth img) (getHeight img) (flipHpixels (getPixels img) (- (getWidth img) 1))))

; Dominio: pixbit-d* / pixrgb-d* / pixhex-d* X int
; Recorrido: pixbit-d* / pixrgb-d* / pixhex-d*
; Descripción: Cambia de posición una lista de pixeles volteados horizontalmente
; Recursión: No aplica
(define (flipHpixels pixlist width)
  (if (null? pixlist)
      null
      (cons (list-set (car pixlist) 0 (- width (posX (car pixlist)))) (flipHpixels (cdr pixlist) width))))

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma vertical
; Recursión: No aplica
(define (flipV img)
  (list (car img) (cadr img) (flipVpixels (list-ref img 2) (- (cadr img) 1))))

; Dominio: pixbit-d* / pixrgb-d* / pixhex-d* X int
; Recorrido: pixbit-d* / pixrgb-d* / pixhex-d*
; Descripción: Cambia de posición una lista de pixeles volteados verticalmente
; Recursión: Recursión de cola
(define (flipVpixels pixlist height)
  (if (null? pixlist)
      null
      (cons (list-set (car pixlist) 1 (- height (posY (car pixlist)))) (flipVpixels (cdr pixlist) height))))

; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
; Descripción: Recorta una imagen a partir de un cuadrante
; Recursión: No aplica
(define (crop img x1 y1 x2 y2)
  (if (and (> x2 x1) (> y2 y1)
           (>= x1 0) (<= x2 (getWidth img))
           (>= y1 0) (<= y2 (getHeight img)))
      (list (+ (- x2 x1) 1) (+ (- y2 y1) 1) (getCropPixels (getPixels img) x1 y1 x2 y2))
      null))

(define (getCropPixels pixlist x1 y1 x2 y2)
  (if (null? pixlist)
      null
      (if (insideCrop? (car pixlist) x1 y1 x2 y2)
          (cons (car pixlist) (getCropPixels (rest pixlist) x1 y1 x2 y2))
          (getCropPixels (rest pixlist) x1 y1 x2 y2))))

(define (insideCrop? pixel x1 y1 x2 y2)
  (if (and (>= (posX pixel) x1) (<= (posX pixel) x2) (>= (posY pixel) y1) (<= (posY pixel) y2)) #t #f))


; Dominio: image
; Recorrido: image
; Descripción: Rota una imagen en 90 grado a la derecha (horario)
; Recursión: No aplica
(define (rotate90 img)
  (define rotatePixel (lambda (pixel)
                      (list (+ (* -1 (posY pixel)) (- (getHeight img) 1)) (posX pixel) (third pixel) (getDepth pixel))))
  (list (getHeight img) (getWidth img) (map rotatePixel (getPixels img))))

(provide (all-defined-out))
