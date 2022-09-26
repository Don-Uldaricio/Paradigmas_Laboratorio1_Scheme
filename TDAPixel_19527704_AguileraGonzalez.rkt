#lang racket

; -------------------------------------------------- TDA PIXEL -----------------------------------------------------

; ----------------------------------------------- REPRESENTACIÓN -------------------------------------------------

; A continuación se presenta el TDA Pixel, donde convergen todos los tipos de pixel mostrados en el enunciado de este
; Laboratorio 1 (pixbit-d, pixrgb-d y pixhex-d) los cuales están compuestos por: la posición dentro de la imagen (x e y),
; el color (dependiendo si es bit, rgb o hex tendrá diferentes representaciones mostradas en las funciones Constructoras),
; y profundidad. Todos estos elementos componen una lista mostrada en los Constructores.

; ------------------------------------------------- CONSTRUCTORES --------------------------------------------------

; Dominio: int X int X int X int
; Recorrido: pixbit-d
; Descripción: Crea un pixbit-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixbit-d x y bit depth)
  (cond
    [(equal? bit 0) (list (list x y) "0" depth)]
    [(equal? bit 1) (list (list x y) "1" depth)]))

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


; ----------------------------------------------------- PERTENENCIA ---------------------------------------------------

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixbit-d
; Recursión: No aplica
(define (pixbit? pixel)
  (if (and (string? (second pixel)) (= (length pixel) 3) (or (equal? (second pixel) "0") (equal? (second pixel) "1"))) #t #f))

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

; ------------------------------------------------------- SELECTORES ----------------------------------------------------

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la posición x de un pixel de cualquier tipo
; Recursión: No aplica
(define (getPosX pixel)
  (caar pixel))

; Dominio: pixel
; Recorrido: int
; Descripción: Entrega la posición y de un pixel de cualquier tipo
; Recursión: No aplica
(define (getPosY pixel)
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
  (list (list val (getPosY pixel)) (getColor pixel) (getDepth pixel)))

; Dominio: pixel X int
; Recorrido: pixel
; Descripción: Cambia el valor de la coordenada Y del pixel a un valor entero cualquiera
; Recursión: No aplica
(define (setPosY pixel val)
  (list (list (getPosX pixel) val) (getColor pixel) (getDepth pixel)))

; OTRAS FUNCIONES --------------------------------------------------------------

; Dominio: int
; Recorrido: str
; Descripción: Convierte los números de un canal RGB en interpretación hexadecimal
; Recursión: No aplica
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

; Dominio: pixel (pixbit-d | pixrgb-d | pixhex-d)
; Recorrido: String color hexadecimal (#XXXXXX)
; Descripción: Transforma el canal RGB a su interpretación hexadecimal
; Recursión: No aplica
(define (intHex pixel)
  (string-append "#" (intStr (quotient (redChannel pixel) 16)) (intStr (remainder (redChannel pixel) 16))
                     (intStr (quotient (greenChannel pixel) 16)) (intStr (remainder (greenChannel pixel) 16))
                     (intStr (quotient (blueChannel pixel) 16)) (intStr (remainder (blueChannel pixel) 16))))


; -------------------------------------------------------------------------------
; Para poder usar este TDA en los demás archivos, es necesaria la linea de abajo
; -------------------------------------------------------------------------------
(provide (all-defined-out))