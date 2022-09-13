#lang racket

(require "funciones.rkt")
(require "TDAPixel.rkt")

; TDA HISTOGRAM ----------------------------------------------------

; CONSTRUCTORES ---------------------------------------------------------

; Dominio: image
; Recorrido: histogram
; Descripción: Crea una lista de colores con su frecuencia dentro de la imagen (histograma)
; Recursión: No aplica
(define (histogram img)
  (colorFreq (imgColors (getPixels img)) (getPixels img)))


; SELECTORES ----------------------------------------------------------------------

; Dominio: histogram
; Recorrido: Par que contiene el color (bit | rgb | hex) y frecuencia del color más repetido en el histograma
; Descripción: Muestra el color más repetido del histograma con frecuencia asociada
; Recursión: No aplica
(define (mostFreqColor hist)
  (define greater (lambda (x y) (if (> (cdr x) (cdr y)) x y)))
    (if (null? (cdr hist))
        (car hist)
        (greater (car hist) (mostFreqColor (cdr hist)))))

; OTRAS FUNCIONES ----------------------------------------------------------------



; -------------------------------------------------------------------------------
; Para poder usar este TDA en los demás archivos, es necesaria la linea de abajo
; -------------------------------------------------------------------------------
(provide (all-defined-out))