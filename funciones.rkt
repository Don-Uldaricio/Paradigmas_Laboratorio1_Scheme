#lang racket

(require "TDAPixel.rkt")

; ------------------------------------------------- TDA IMAGE -----------------------------------------------------

; ------------------------------------------------ CONSTRUCTORES -------------------------------------------------

; Dominio: Width (int) X Height (int) X [pixbit-d* |  pixrgb-d* | pixhex-d*]
; Recorrido: image
; Descripción: Crea una una imagen de tipo pixmap/bitmap/hexmap
;              con cada pixel en una lista
; Recursión: No aplica
(define (image width height . pixlist)
  (if (and (integer? width) (> width 0) (integer? height) (> height 0))
      (list width height (list) pixlist (list))
      null))

; ---------------------------------------------- FUNCIONES DE PERTENENCIA ------------------------------------------------------

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

; ------------------------------------------- SELECTORES -------------------------------------------

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

; ------------------------------------------- MODIFICADORES ------------------------------------------

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma horizontal
; Recursión: De cola (por la función setPixels)
(define (flipH img)
  (define flipHpixel (lambda (pixel)
                       (setPosX pixel (- (- (getWidth img) 1) (posX pixel)))))
  (list (getWidth img) (getHeight img) (getCompColor img) (setPixels img flipHpixel) (getCompPixels img)))

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma vertical
; Recursión: De cola (por la función setPixels)
(define (flipV img)
  (define flipVpixel (lambda (pixel)
                       (setPosY pixel (- (- (getHeight img) 1) (posY pixel)))))
  (list (getWidth img) (getHeight img) (getCompColor img) (setPixels img flipVpixel) (getCompPixels img)))


; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
; Descripción: Recorta una imagen a partir de un cuadrante
; Recursión: No aplica
(define (crop img x1 y1 x2 y2)
  (define moveCropPixel (lambda (pixel)
    (setPosY (setPosX pixel (- (posX pixel) x1)) (- (posY pixel) y1))))
  (if (and (> x2 x1) (> y2 y1) (>= x1 0) (<= x2 (- (getWidth img) 1)) (>= y1 0) (<= y2 (- (getHeight img) 1)))
      (list (+ (- x2 x1) 1) (+ (- y2 y1) 1) (getCompColor img) (map moveCropPixel (filter (lambda (pixel) (insideCrop? pixel x1 y1 x2 y2)) (getPixels img))) (getCompPixels img))
      null))

; Dominio: image
; Recorrido: histogram
; Descripción: Crea una lista de colores con su frecuencia dentro de la imagen (histograma)
; Recursión: No aplica
(define (histogram img)
  (colorFreq (imgColors (getPixels img)) (getPixels img)))

; Dominio: image
; Recorrido: image
; Descripción: Rota una imagen en 90 grado a la derecha (sentido horario)
; Recursión: No aplica
(define (rotate90 img)
  (define rotatePixel (lambda (pixel)
                      (list (list (+ (* -1 (posY pixel)) (- (getHeight img) 1)) (posX pixel)) (getColor pixel) (getDepth pixel))))
  (list (getHeight img) (getWidth img) (list) (setPixels img rotatePixel) (list)))

; Dominio: image
; Recorrido: image
; Descripción: Comprime una imagen eliminando los pixeles que contienen el color más frecuente
; Recursión: No aplica
;(define (compress img))
(define (compress img)
  (define equalColor? (lambda (x) (if (equal? (mostFreqColor (histogram img)) (getColor x)) #t #f)))
  (list (getWidth img) (getHeight img) (mostFreqColor (histogram img)) (filter-not equalColor? (getPixels img)) (compressPixels (getPixels img) (mostFreqColor (histogram img)))))


(define (compressPixels pixlist color)
  (if (null? pixlist)
      null
      (if (equal? (getColor (car pixlist)) color)
          (cons (list (posX (car pixlist)) (posY (car pixlist)) (getDepth (car pixlist))) (compressPixels (cdr pixlist) color))
          (compressPixels (cdr pixlist) color))))


; Dominio: image
; Recorrido: image
; Descripción: Transforma una imagen tipo pixrgb-d en una pixhex-d
; Recursión: No aplica
(define (imgRGB->imgHex img)
  (define rgbToHex (lambda (pixel)
                     (pixhex-d (posX pixel) (posY pixel) (intHex pixel) (getDepth pixel))))
  (list (getWidth img) (getHeight img) (getCompColor img) (map rgbToHex (getPixels img)) (getCompPixels img)))

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Cambia el valor del bit del pixbit-d al bit opuesto
; Recursión: No aplica
(define (invertColorBit pixel)
  (if (pixbit? pixel)
      (if (equal? (getBit pixel) "0")
      (pixbit-d (posX pixel) (posY pixel) 1 (getDepth pixel))
      (pixbit-d (posX pixel) (posY pixel) 0 (getDepth pixel)))
      null))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Cambia el valor de cada canal RGB al valor simétricamente opuesto
; Recursión: No aplica
(define (invertColorRGB pixel)
  (define invertColor (lambda (x) (- 255 x)))
  (if (pixrgb? pixel)
      (pixrgb-d (posX pixel) (posY pixel) (invertColor (redChannel pixel)) (invertColor (greenChannel pixel)) (invertColor (blueChannel pixel)) (getDepth pixel))
      null))

; Dominio: image X fn
; Recorrido: string
; Descripción: Transforma una imagen a un representación string hexadecimal
; Recursión: De cola, en la función lambda printPixels
(define (image->string img fn)
  (define printPixels (lambda (pixlist)
  (if (null? pixlist)
      null
      (if (= (posX (car pixlist)) (- (getWidth img) 1))
      (cons (string-join (list (getColor (car pixlist)) "\n")) (printPixels (cdr pixlist)))
      (cons (getColor (car pixlist)) (printPixels (cdr pixlist)))))))
  (string-join (printPixels (getPixels (fn img)))))


; ------------------------------------------------------ OTRAS FUNCIONES ------------------------------------------------------------------

; Dominio: image X fn
; Recorrido: lista de pixeles
; Descripción: Aplica una función a la lista de pixeles de una imagen,
;              retornando la lista de pixeles modificados.
; Recursión: De cola
(define (setPixels img fn)
  (define mapPixels (lambda (pixlist)
    (if (null? pixlist) null (cons (fn (car pixlist)) (mapPixels (cdr pixlist))))))
  (mapPixels (getPixels img)))

; Dominio: pixel X int X int X int X int
; Recorrido: boolean
; Descripción: Verifica si es que un pixel dentro de una imagen está dentro
;              del área de recorte (crop)
; Recursión: No aplica
(define (insideCrop? pixel x1 y1 x2 y2)
  (if (and (>= (posX pixel) x1) (<= (posX pixel) x2) (>= (posY pixel) y1) (<= (posY pixel) y2)) #t #f))

; Dominio: histogram
; Recorrido: Par que contiene el color (bit | rgb | hex) y frecuencia del color más repetido en el histograma
; Descripción: Muestra el color más repetido del histograma con frecuencia asociada
; Recursión: No aplica
(define (mostFreqColor hist)
    (define mostFreqElement (lambda (hist)
                              (if (null? (cdr hist))
                                  (car hist)
                                  (greater (car hist) (mostFreqElement (cdr hist))))))
  (car (mostFreqElement hist)))

; Dominio: elemento de histogram
; Recorrido: elemento de histograma
; Descripción: Compara dos elementos tipo histograma (cons Color Frecuencia)
;              y entrega el que tiene mayor frecuencia.
; Recursión: No aplica
(define (greater x y)
  (if (>= (cdr x) (cdr y)) x y))

; Dominio: funcion X image
; Recorrido: image
; Descripción: Función de orden superior que permite aplicar un filtro de manera
;              individual a los pixeles.
; Recursión: No aplica
(define (edit fn img)
  (list (getWidth img) (getHeight img) (getCompColor img) (map fn (getPixels img)) (getCompPixels img)))

; Dominio: image
; Recorrido: image
; Descripción: Retorna
; Recursión: No aplica
(define (pixbit->string img)
  (if (bitmap? img) img null))

; Dominio: image
; Recorrido: image
; Descripción: Transforma el valor de los canales RGB de una imagen pixmap a string hexadecimal
; Recursión: No aplica
(define (pixrgb->string img)
  (if (pixmap? img) (imgRGB->imgHex img) null))

; Dominio: image
; Recorrido: image
; Descripción: Retorna la imagen hexadecimal si es que la imagen es hexmap
; Recursión: No aplica
(define (pixhex->string img)
  (if (hexmap? img) img null))


; -------------------------------------------------------------------------------
; Para poder usar este TDA en los demás archivos, es necesaria la linea de abajo
; -------------------------------------------------------------------------------
(provide (all-defined-out))
