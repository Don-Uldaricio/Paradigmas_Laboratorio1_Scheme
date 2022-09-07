#lang racket

; TDA PIXEL -----------------------------------------------------

; CONSTRUCTORES --------------------------------------------------

; Dominio: int X int X int X int
; Recorrido: pixbit-d
; Descripción: Crea un pixbit-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixbit-d x y bit depth) (list (list x y) bit depth))

; Dominio: int X int X int X int X int X int
; Recorrido: pixrgb-d
; Descripción: Crea un pixrgb-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixrgb-d x y r g b depth) (list (list x y) (list r g b) depth))

; Dominio: int X int X int X int
; Recorrido: pixhex-d
; Descripción: Crea un pixhex-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixhex-d x y hex depth) (list (list x y) hex depth))

; PERTENENCIA ---------------------------------------------------

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixbit-d
; Recursión: No aplica
(define (pixbit? pixel)
  (if (and (integer? (second pixel)) (= (length pixel) 3) (or (= (second pixel) 0) (= (second pixel) 1))) #t #f))

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixrgb-d
; Recursión: No aplica
(define (pixrgb? pixel)
  (if (and (list? (second pixel)) (= (length pixel) 3)
       (and (>= (first (second pixel)) 0) (<= (first (second pixel)) 255))
       (and (>= (second (second pixel)) 0) (<= (second (second pixel)) 255))
       (and (>= (third (second pixel)) 0) (<= (third (second pixel)) 255))) #t #f))

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixhex-d
; Recursión: No aplica
(define (pixhex? pixel)
  (if (and (string? (second pixel)) (= (length pixel) 3) (= (string-length (second pixel)) 7)) #t #f))

; SELECTORES ----------------------------------------------------

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la posición x de un pixel de cualquier tipo
; Recursión: No aplica
(define (pos pixel)
  (car pixel))

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la posición x de un pixel de cualquier tipo
; Recursión: No aplica
(define (posX pixel)
  (caar pixel))

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la posición y de un pixel de cualquier tipo
; Recursión: No aplica
(define (posY pixel)
  (cadar pixel))

; Dominio: pixbit-d
; Recorrido: int
; Descripción: Entrega el valor del bit de un pixbit-d
; Recursión: No aplica
(define (getBit pixel)
  (second pixel))

; Dominio: pixrgb-d
; Recorrido: '(Red (int) X Green (int) X Blue (int))
; Descripción: Entrega una lista con los valores RGB de un pixrgb-d
; Recursión: No aplica
(define (rgbChannel pixel)
  (second pixel))

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

; Dominio: pixhex-d
; Recorrido: hex (string)
; Descripción: Entrega el color en hexadecimal de un pixhex-d
; Recursión: No aplica
(define (getHex pixel)
  (second pixel))

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega el color de algún tipo de pixel (bit/rgb/hex)
; Recursión: No aplica
(define (getColor pixel)
  (cond
    [(pixbit? pixel) (getBit pixel)]
    [(pixrgb? pixel) (rgbChannel pixel)]
    [(pixhex? pixel) (getHex pixel)]))

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la profundidad de un pixel de cualquier tipo
; Recursión: No aplica
(define (getDepth pixel)
  (third pixel))

; MODIFICADORES -------------------------------------------------

; Dominio: pixel X int
; Recorrido: pixel
; Descripción: Cambia el valor de la coordenada X del pixel a un valor entero cualquiera
; Recursión: No aplica
(define (setPosX pixel val)
  (list (list val (posY pixel)) (getColor pixel) (getDepth pixel)))

; Dominio: pixel X int
; Recorrido: pixel
; Descripción: Cambia el valor de la coordenada Y del pixel a un valor entero cualquiera
; Recursión: No aplica
(define (setPosY pixel val)
  (list (list (posX pixel) val) (getColor pixel) (getDepth pixel)))

; TDA IMAGE -----------------------------------------------------

; CONSTRUCTORES -------------------------------------------------

; Dominio: Width (int) X Height (int) X [pixbit-d* |  pixrgb-d* | pixhex-d*]
; Recorrido: image
; Descripción: Crea una una imagen de tipo pixmap/bitmap/hexmap
;              con cada pixel en una lista
; Recursión: No aplica
(define (image width height . pixlist)
  (if (and (integer? width) (> width 0) (integer? height) (> height 0))
      (list width height (list) pixlist (list))
      null))

; PERTENENCIA ------------------------------------------------------

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen es bitmap
; Recursión: No aplica
(define (bitmap? img)
  (if (andmap pixbit? (list-ref img 3)) #t #f))

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen es pixmap
; Recursión: No aplica
(define (pixmap? img)
  (if (andmap pixrgb? (list-ref img 3)) #t #f))

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen es hexmap
; Recursión: No aplica
(define (hexmap? img)
  (if (andmap pixhex? (list-ref img 3)) #t #f))

; Dominio: image
; Recorrido: boolean
; Descripción: Determina si una imagen está comprimida
; Recursión: No aplica
(define (compressed? img)
  (if (empty? (fifth img)) #f #t))

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
; Recorrido: lista con color comprimido
; Descripción: Entrega el color comprimido de una imagen
; Recursión: No aplica
(define (getCompColor img)
  (third img))

; Dominio: image
; Recorrido: pixbit-d* | pixrgb-d* | pixhex-d*
; Descripción: Entrega la lista de pixeles de una imagen
; Recursión: No aplica
(define (getPixels img)
  (fourth img))

; Dominio: image
; Recorrido: pixbit-d | pixrgb-d | pixhex-d
; Descripción: Entrega el primer pixel de una imagen
; Recursión: No aplica
(define (firstPixel img)
  (first (getPixels img)))

; Dominio: image
; Recorrido: pixbit-d* | pixrgb-d* | pixhex-d*
; Descripción: Entrega la lista de pixeles de una imagen, exceptuando el primer término
; Recursión: No aplica
(define (restPixels img)
  (rest (getPixels img)))

; Dominio: image
; Recorrido: lista de pixeles
; Descripción: Entrega la lista de pixeles comprimidos de una imagen con su posición y profundidad
; Recursión: No aplica
(define (getCompPixels img)
  (fifth img))

; MODIFICADORES -------------------------------------

; Dominio: image X fn
; Recorrido: lista de pixeles
; Descripción: Aplica una función a la lista de pixeles de una imagen,
;              retornando la lista de pixeles modificados.
; Recursión: De cola
(define (setPixels img fn)
  (define mapPixels (lambda (pixlist)
    (if (null? pixlist) null (cons (fn (car pixlist)) (mapPixels (cdr pixlist))))))
  (mapPixels (getPixels img)))

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma horizontal
; Recursión: De cola (por la función setPixels)
(define (flipH img)
  (define flipHpixel (lambda (pixel)
                       (setPosX pixel (- (- (getWidth img) 1) (posX pixel)))))
  (list (getWidth img) (getHeight img) (list) (setPixels img flipHpixel) (list)))

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma vertical
; Recursión: De cola (por la función setPixels)
(define (flipV img)
  (define flipVpixel (lambda (pixel)
                       (setPosY pixel (- (- (getHeight img) 1) (posY pixel)))))
  (list (getWidth img) (getHeight img) (list) (setPixels img flipVpixel) (list)))


; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
; Descripción: Recorta una imagen a partir de un cuadrante
; Recursión: No aplica
(define (crop img x1 y1 x2 y2)
  (define moveCropPixel (lambda (pixel)
    (setPosY (setPosX pixel (- (posX pixel) x1)) (- (posY pixel) y1))))
  (if (and (> x2 x1) (> y2 y1) (>= x1 0) (<= x2 (- (getWidth img) 1)) (>= y1 0) (<= y2 (- (getHeight img) 1)))
      (list (+ (- x2 x1) 1) (+ (- y2 y1) 1) (list) (map moveCropPixel (filter (lambda (pixel) (insideCrop? pixel x1 y1 x2 y2)) (getPixels img))) (list))
      null))

; Dominio: pixel X int X int X int X int
; Recorrido: boolean
; Descripción: Verifica si es que un pixel dentro de una imagen está dentro
;              del área de recorte (crop)
; Recursión: No aplica
(define (insideCrop? pixel x1 y1 x2 y2)
  (if (and (>= (posX pixel) x1) (<= (posX pixel) x2) (>= (posY pixel) y1) (<= (posY pixel) y2)) #t #f))

; Dominio: image
; Recorrido: image
; Descripción: Rota una imagen en 90 grado a la derecha (sentido horario)
; Recursión: No aplica
(define (rotate90 img)
  (define rotatePixel (lambda (pixel)
                      (list (list (+ (* -1 (posY pixel)) (- (getHeight img) 1)) (posX pixel)) (getColor pixel) (getDepth pixel))))
  (list (getHeight img) (getWidth img) (list) (setPixels img rotatePixel) (list)))

(define (intStr numero)
  (cond
    [(equal? numero 0) "0"]
    [(equal? numero 1) "1"]
    [(equal? numero 2) "2"]
    [(equal? numero 3) "3"]
    [(equal? numero 4) "4"]
    [(equal? numero 5) "5"]
    [(equal? numero 6) "6"]
    [(equal? numero 7) "7"]
    [(equal? numero 8) "8"]
    [(equal? numero 9) "9"]
    [(equal? numero 10) "A"]
    [(equal? numero 11) "B"]
    [(equal? numero 12) "C"]
    [(equal? numero 13) "D"]
    [(equal? numero 14) "E"]
    [(equal? numero 15) "F"]))

(define (intHex pixel)
  (string-append "#" (intStr (quotient (redChannel pixel) 16)) (intStr (remainder (redChannel pixel) 16))
                     (intStr (quotient (greenChannel pixel) 16)) (intStr (remainder (greenChannel pixel) 16))
                     (intStr (quotient (blueChannel pixel) 16)) (intStr (remainder (blueChannel pixel) 16))))

(define (imgRGB->imgHex img)
  (define rgbToHex (lambda (pixel)
                     (pixhex-d (posX pixel) (posY pixel) (intHex pixel) (getDepth pixel))))
  (list (getWidth img) (getHeight img) (getCompColor img) (map rgbToHex (getPixels img)) (getCompPixels img)))

; -------------------------------------------------------------------------------
; Para poder usar este TDA en los demás archivos, es necesaria la linea de abajo
; -------------------------------------------------------------------------------
(provide (all-defined-out))
