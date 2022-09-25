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
      (sortImage (list width height (list) pixlist (list)))
      null))

; ------------------------------------------- FUNCIONES DE PERTENENCIA ---------------------------------------------

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
                       (setPosX pixel (- (- (getWidth img) 1) (getPosX pixel)))))
  (list (getWidth img) (getHeight img) (getCompColor img) (setPixels img flipHpixel) (getCompPixels img)))

; Dominio: image
; Recorrido: image
; Descripción: Voltea una imagen de forma vertical
; Recursión: De cola (por la función setPixels)
(define (flipV img)
  (define flipVpixel (lambda (pixel)
                       (setPosY pixel (- (- (getHeight img) 1) (getPosY pixel)))))
  (list (getWidth img) (getHeight img) (getCompColor img) (setPixels img flipVpixel) (getCompPixels img)))


; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
; Descripción: Recorta una imagen a partir de un cuadrante
; Recursión: No aplica
(define (crop img x1 y1 x2 y2)
  (define moveCropPixel (lambda (pixel)
    (setPosY (setPosX pixel (- (getPosX pixel) x1)) (- (getPosY pixel) y1))))
  (if (and (>= x2 x1) (>= y2 y1) (>= x1 0) (<= x2 (- (getWidth img) 1)) (>= y1 0) (<= y2 (- (getHeight img) 1)))
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
                      (list (list (+ (* -1 (getPosY pixel)) (- (getHeight img) 1)) (getPosX pixel)) (getColor pixel) (getDepth pixel))))
  (list (getHeight img) (getWidth img) (list) (setPixels img rotatePixel) (list)))

; Dominio: image
; Recorrido: image
; Descripción: Comprime una imagen eliminando los pixeles que contienen el color más frecuente
; Recursión: No aplica
;(define (compress img))
(define (compress img)
  (define equalColor? (lambda (x) (if (equal? (mostFreqColor (histogram img)) (getColor x)) #t #f)))
  (list (getWidth img) (getHeight img) (mostFreqColor (histogram img)) (filter-not equalColor? (getPixels img)) (compressPixels (getPixels img) (mostFreqColor (histogram img)))))

; Dominio: image
; Recorrido: image
; Descripción: Transforma una imagen tipo pixmap-d en una hexmap-d
; Recursión: No aplica
(define (imgRGB->imgHex img)
  (define rgbToHex (lambda (pixel)
                     (pixhex-d (getPosX pixel) (getPosY pixel) (intHex pixel) (getDepth pixel))))
  (list (getWidth img) (getHeight img) (getCompColor img) (map rgbToHex (getPixels img)) (getCompPixels img)))

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Cambia el valor de los pixbit-d pertenecientes al bitmap-d al bit opuesto
; Recursión: No aplica
(define (invertColorBit pixel)
  (if (pixbit? pixel)
      (if (equal? (getBit pixel) "0")
      (pixbit-d (getPosX pixel) (getPosY pixel) 1 (getDepth pixel))
      (pixbit-d (getPosX pixel) (getPosY pixel) 0 (getDepth pixel)))
      null))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Cambia el valor de cada canal RGB al valor simétricamente opuesto
; Recursión: No aplica
(define (invertColorRGB pixel)
  (define invertColor (lambda (x) (- 255 x)))
  (if (pixrgb? pixel)
      (pixrgb-d (getPosX pixel) (getPosY pixel) (invertColor (redChannel pixel)) (invertColor (greenChannel pixel)) (invertColor (blueChannel pixel)) (getDepth pixel))
      null))

; Dominio: image X fn
; Recorrido: string
; Descripción: Muestra una imagen con sus pixeles representados como string
; Recursión: De cola, en la función lambda printPixels
(define (image->string img fn)
  (define printPixels (lambda (pixlist)
  (if (null? pixlist)
      null
      (cond
        [(= (length (getPixels img)) 1) (list "" (getColor (car pixlist)) "\n\n")]
        [(and (= (getPosX (car pixlist)) 0)
              (= (getPosY (car pixlist)) 0)) (cons (string-join (list "" (getColor (car pixlist)))) (printPixels (cdr pixlist)))]
        [(and (= (getPosX (car pixlist)) (- (getWidth img) 1))
              (= (getPosY (car pixlist)) (- (getHeight img) 1))) (cons (string-join (list (getColor (car pixlist)) "\n\n")) (printPixels (cdr pixlist)))]
        [(= (getPosX (car pixlist)) (- (getWidth img) 1)) (cons (string-join (list (getColor (car pixlist)) "\n")) (printPixels (cdr pixlist)))]
        [else (cons (getColor (car pixlist)) (printPixels (cdr pixlist)))]))))
  (string-join (printPixels (getPixels (fn (sortImage (decompress img)))))))

; Dominio: image
; Recorrido: image list
; Descripción: Crea una lista de imagenes donde cada imagen contiene pixeles de la misma profundidad.
;              Los pixeles que no pertenecen a la profundidad son reemplazados con pixeles blancos.
; Recursión: No aplica
(define (depthLayers img)
  (define (depthImage depthlist)
    (define whiteDepthPixel (lambda (pixel)
                              (if (= (getDepth pixel) (car depthlist))
                                  pixel
                                  (cond
                                    [(pixbit? pixel) (pixbit-d (getPosX pixel) (getPosY pixel) 1 (car depthlist))]
                                    [(pixrgb? pixel) (pixrgb-d (getPosX pixel) (getPosY pixel) 255 255 255 (car depthlist))]
                                    [(pixhex? pixel) (pixhex-d (getPosX pixel) (getPosY pixel) "#FFFFFF" (car depthlist))]))))
  (if (null? depthlist)
      null
      (cons (list (getWidth img) (getHeight img) (list) (map whiteDepthPixel (getPixels img)) (list)) (depthImage (cdr depthlist)))))
  (depthImage (imgDepths (getPixels img))))

; Dominio: image
; Recorrido: image
; Descripción: Descomprime una imagen previamente comprimida recuperando
;              los pixeles eliminados.
; Recursión: No aplica
(define (decompress img)
  (define recoverCompPixels (lambda (pixlist color)
  (if (null? pixlist)
      null
      (cons (list (list (first (car pixlist)) (second (car pixlist))) color (third (car pixlist))) (recoverCompPixels (cdr pixlist) color)))))
  (if (compressed? img)
      (sortImage (list (getWidth img) (getHeight img) (list) (append (getPixels img) (recoverCompPixels (getCompPixels img) (getCompColor img))) (list)))
      img))

; Dominio: image
; Recorrido: image
; Descripción: Ordena los pixeles de una imgen para visualizar la imagen correctamente
;              al aplicar la función image->string
; Recursión: No aplica
(define (sortImage img)
  (define sortPixels (lambda (pixlist) (sort (sort pixlist #:key caar <) #:key cadar <)))
  (list (getWidth img) (getHeight img) (getCompColor img) (sortPixels (getPixels img)) (getCompPixels img)))


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
  (if (and (>= (getPosX pixel) x1) (<= (getPosX pixel) x2) (>= (getPosY pixel) y1) (<= (getPosY pixel) y2)) #t #f))

; Dominio: lista de pixeles (pixrgb-d* | pixbit-d* | pixhex-d*)
; Recorrido: Lista de colores
; Descripción: Crea una lista de colores (sin repetir) a partir de una imagen
; Recursión: No aplica
(define (imgColors pixlist)
  (if (null? pixlist)
      null
      (remove-duplicates (cons (getColor (car pixlist)) (imgColors (cdr pixlist))))))

; Dominio: lista de colores (bit | rgb | hex) X lista de pixeles
; Recorrido: Lista de pares -> color X frecuencia (int)
; Descripción: Crea una lista que contiene los colores de una lista de pixeles y su frecuencia
; Recursión: No aplica
(define (colorFreq colorList pixlist)
  (if (null? colorList)
      null
      (cons (cons (car colorList) (count (lambda (px) (if (equal? (getColor px) (car colorList)) #t #f)) pixlist)) (colorFreq (cdr colorList) pixlist))))

; Dominio: histogram
; Recorrido: Par que contiene el color (bit | rgb | hex) y frecuencia del color más repetido en el histograma
; Descripción: Muestra el color más repetido del histograma con frecuencia asociada
; Recursión: No aplica
(define (mostFreqColor hist)
    (define mostFreqElement (lambda (hist)
                              (if (null? (cdr hist))
                                  (car hist)
                                  ((lambda (x y) (if (>= (cdr x) (cdr y)) x y)) (car hist) (mostFreqElement (cdr hist))))))
  (car (mostFreqElement hist)))

; Dominio: lista de pixeles X color (pixbit-d | pixhex-d | pixrgb-d)
; Recorrido: lista de pixeles comprimidos
; Descripción: Retorna una lista con los pixeles comprimidos y la información
;              de su posición y profundidad.
; Recursión: De cola
(define (compressPixels pixlist color)
  (if (null? pixlist)
      null
      (if (equal? (getColor (car pixlist)) color)
          (cons (list (getPosX (car pixlist)) (getPosY (car pixlist)) (getDepth (car pixlist))) (compressPixels (cdr pixlist) color))
          (compressPixels (cdr pixlist) color))))

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

; Dominio: pixel list
; Recorrido: depth list (int*)
; Descripción: Entrega una lista con las profundidades de los pixeles de la imagen, sin repetir.
; Recursión: No aplica
(define (imgDepths pixlist)
  (if (null? pixlist)
      null
      (remove-duplicates (cons (getDepth (car pixlist)) (imgDepths (cdr pixlist))))))

; -------------------------------------------------------------------------------
; Para poder usar este TDA en los demás archivos, es necesaria la linea de abajo
; -------------------------------------------------------------------------------
(provide (all-defined-out))
