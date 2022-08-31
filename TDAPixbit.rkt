#lang racket

; TDA PIXBIT-D ------------------------------------

; REPRESENTACIÓN ----------------------------------

; CONSTRUCTORES -----------------------------------

; Dominio: int X int X int X int
; Recorrido: pixbit-d
; Descripción: Crea un pixbit-d mediante una lista de sus parámetros
; Recursión: No aplica
(define (pixbit-d x y bit depth)
  (list x y bit depth))

; FUNCIONES DE PERTENENCIA ------------------------

; Dominio: pixel
; Recorrido: boolean
; Descripción: Verifica si un dato es tipo pixbit-d
; Recursión: No aplica
(define (pixbit? pixel)
  (if (and (= (length pixel) 4) (or (= (list-ref pixel 2) 0) (= (list-ref pixel 2) 1))) #t #f))

; SELECTORES --------------------------------------

; Dominio: pixbit-d
; Recorrido: int
; Descripción: Entrega el valor del bit del pixbit-d
; Recursión: No aplica
(define (getBit pixel)
  (if (pixbit? pixel) (third pixel) null))

