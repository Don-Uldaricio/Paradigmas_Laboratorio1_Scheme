#lang racket

(require "TDAImage.rkt")
(require "TDAPixel.rkt")

;--------------------------------------- SCRIPT DE PRUEBAS --------------------------------------

;Creación de una imagen de 2 x 2 del tipo pixmap
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)))

;Creación de una imagen de 2 x 2 del tipo bitmap
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))

(define img3 (imgRGB->imgHex img1))


;imprimir una representación string de la imagen
(display (image->string img1 pixrgb->string))

;output:
; #FF0000 #0000FF
; #00FF00 #FFFFFF

;imprimir una representación string de la imagen
(display (image->string img2 pixbit->string))

;output:
;0 1
;1 0

(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f

(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f

(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t

(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f

(flipH img1)
(flipH img2)
(flipH img3)

(flipV img1)
(flipV img2)
(flipV img3)

(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen

(histogram img1)
(histogram img2)
(histogram img3)
(histogram img4)
(histogram img5)
(histogram img6)
(histogram img7)

(define img18 (rotate90 img1))
(define img19 (rotate90 img2))
(define img20 (rotate90 img3))
(define img21 (rotate90 img4))
(define img22 (rotate90 img5))
(define img23 (rotate90 img6))
(define img24 (rotate90 img7))

(define img8 (compress img1))
(define img9 (compress img2))
(define img10 (compress img3))
(define img11 (compress img4))
(define img12 (compress img5))
(define img13 (compress img6))
(define img14 (compress img7))

(compressed? img8)  ; la respuesta debería ser #t
(compressed? img9)  ; la respuesta debería ser #t
(compressed? img10)  ; la respuesta debería ser #t
(compressed? img11)  ; la respuesta debería ser #t
(compressed? img12)  ; la respuesta debería ser #t
(compressed? img13)  ; la respuesta debería ser #t
(compressed? img14)  ; la respuesta debería ser #t

(define img15 (edit invertColorBit img2))
(define img16 (edit invertColorRGB img1))

;se asume que las funciones de ajuste de canal están implementadas. 
;Puede cambiarlas por otras en su script de pruebas si así lo prefiere 
;(define img15 (edit (adjustChannel getR setR incCh) img1))
;(define img16 (edit (adjustChannel getG setG incCh) img1)) ADJUST CHANNEL FUNCIÓN NO IMPLEMENTADA
;(define img17 (edit (adjustChannel getB setB incCh) img1))

;imágenes no comprimidas
(display (image->string img1 pixrgb->string))
(display (image->string img2 pixbit->string))
(display (image->string img3 pixhex->string))
(display (image->string img4 pixrgb->string))
(display (image->string img5 pixbit->string))
(display (image->string img6 pixrgb->string))
(display (image->string img7 pixbit->string))

;imagenes comprimidas, podrían internamente descomprimirlas para convertir a string ;(opcional)
(display (image->string img8 pixrgb->string))
(display (image->string img9 pixbit->string))
(display (image->string img10 pixhex->string)) 
(display (image->string img11 pixrgb->string))
(display (image->string img12 pixbit->string))
(display (image->string img13 pixrgb->string))
(display (image->string img14 pixbit->string))

;imágenes no comprimidas
(display (image->string img15 pixbit->string))
(display (image->string img16 pixrgb->string))
;(display (image->string img17 pixrgb->string))
(display (image->string img18 pixrgb->string))
(display (image->string img19 pixbit->string))
(display (image->string img20 pixhex->string))
(display (image->string img21 pixrgb->string))
(display (image->string img22 pixbit->string))
(display (image->string img23 pixrgb->string))
(display (image->string img24 pixbit->string))

(depthLayers img1)
(depthLayers img2)
(depthLayers img3)
(depthLayers img4)
(depthLayers img5)
(depthLayers img6)
(depthLayers img7)

(define img25 (decompress img8))
(define img26 (decompress img9))
(define img27 (decompress img10))
(define img29 (decompress img11))
(define img30 (decompress img12))
(define img31 (decompress img13))
(define img32 (decompress img14))

;las siguientes comparaciones deberían arrojar #t
(equal? img25 img1)
(equal? img26 img2)
(equal? img27 img3)
;(equal? img28 img4)
(equal? img29 img5)
(equal? img30 img6)
(equal? img31 img7)

;las siguientes comparaciones deberían arrojar #f
(equal? img25 img2)
(equal? img26 img1)


; --------------------------------- SCRIPT DE PRUEBA ADICIONAL --------------------------------------
(display "----------------------------------------------------- SCRIPT DE PRUEBA ADICIONAL ------------------------------------------------\n")

;Creamos una imagen bitmap-d de 4x2 con 8 pixeles tipo pixbit-d
(define img40 (image 4 2 (pixbit-d 0 0 1 2) (pixbit-d 1 0 1 4) (pixbit-d 2 0 1 2) (pixbit-d 3 0 0 2)
                        (pixbit-d 0 1 0 4) (pixbit-d 1 1 0 2) (pixbit-d 2 1 0 1) (pixbit-d 3 1 0 0)))

;Creamos una imagen pixmap-d de 4x2 con 8 pixeles tipo pixrgb-d
(define img41 (image 4 2 (pixrgb-d 0 0 15 34 120 3)(pixrgb-d 1 0 15 67 120 2)(pixrgb-d 2 0 15 65 30 2)(pixrgb-d 3 0 15 65 120 3)
                        (pixrgb-d 0 1 15 65 120 1)(pixrgb-d 1 1 15 65 40 1)(pixrgb-d 2 1 15 65 120 1)(pixrgb-d 3 1 50 65 120 2)))

;Creamos una imagen hexmap-d de 2x2 con 4 pixeles tipo pixhex-d
(define img42 (image 2 2 (pixhex-d 0 0 "#AA45CC" 1) (pixhex-d 1 0 "#AAEDCC" 1) (pixhex-d 0 1 "#AABBCC" 0) (pixhex-d 1 1 "#AA45CC" 2)))

(display "Mostramos el bitmap de 4x2 creado:\n")
img40
(display "Mostramos el pixmap de 4x2 creado:\n")
img41
(display "Mostramos el hexmap de 2x2 creado:\n")
img42
(display "Es img40 un bitmap? ")
(bitmap? img40)
(display "Es img41 un pixmap? ")
(pixmap? img41)
(display "Es img42 un hexmap? ")
(hexmap? img42)
(display "\n")

; ---------------------------------- PRUEBA BITMAP ----------------------------------------
(display "----------------------- PRUEBA BITMAP -------------------------\n")
img40
(display "Probamos algunas funciones con el BITMAP IMG40:\n")
(flipH img40)
(flipV img40)
(rotate90 img40)
(display "Aplicamos flipH, flipV y rotate90 a la img40:\n")
(flipH (flipV (rotate90 img40)))
(display "Cortamos la imagen según el cuadrante de pixeles (1,0) x (2,1):\n")
(crop img40 1 0 2 1)
(display "Vemos el histograma del bitmap img40:\n")
(histogram img40)
(display "Comprimimos la imagen:\n")
(compress img40)
(display "Se invierten los canales de sus bits:\n")
(edit invertColorBit img40)
(display "Se muestran la imagen como string:\n")
(display (image->string img40 pixbit->string))
(display "Creamos una lista de imagenes con depthLayers:\n")
(depthLayers img40)
(display "Descomprimimos la imagen comprimida:\n")
(decompress (compress img40))
(display "\n\n")


; ---------------------------------- PRUEBA PIXMAP ----------------------------------------
(display "----------------------- PRUEBA PIXMAP -------------------------\n")
img41
(display "Probamos algunas funciones con el pixmap img41:\n")
(flipH img41)
(flipV img41)
(rotate90 img41)
(display "Aplicamos flipH, flipV y rotate90 a la img41:\n")
(flipH (flipV (rotate90 img41)))
(display "Cambiamos el formato a hexadecimal de la img41:\n")
(imgRGB->imgHex img41)
(display "Cortamos la imagen según el cuadrante de pixeles (1,0) x (2,1):\n")
(crop img41 1 0 2 1)
(display "Vemos el histograma del pixmap img41:\n")
(histogram img41)
(display "Comprimimos la imagen:\n")
(compress img41)
(display "Se invierten los canales de sus pixeles:\n")
(edit invertColorRGB img41)
(display "Se muestran la imagen como string:\n")
(display (image->string img41 pixrgb->string))
(display "Creamos una lista de imagenes con depthLayers:\n")
(depthLayers img41)
(display "Descomprimimos la imagen comprimida:\n")
(decompress (compress img41))
(display "\n\n")

; ---------------------------------- PRUEBA HEXMAP ----------------------------------------
(display "----------------------- PRUEBA HEXMAP -------------------------\n")
img42
(display "Probamos algunas funciones con el hexmap img42:\n")
(flipH img42)
(flipV img42)
(rotate90 img42)
(display "Aplicamos flipH, flipV y rotate90 a la img42:\n")
(flipH (flipV (rotate90 img42)))
(display "Cortamos la imagen según el cuadrante de pixeles (1,0) x (1,1):\n")
(crop img41 1 0 1 1)
(display "Vemos el histograma del pixmap img42:\n")
(histogram img42)
(display "Comprimimos la imagen:\n")
(compress img42)
(display "Está comprimida la imagen? ")
(compressed? img42)
(display "Se muestran la imagen como string:\n")
(display (image->string img42 pixhex->string))
(display "Creamos una lista de imagenes con depthLayers:\n")
(depthLayers img42)
(display "Descomprimimos la imagen comprimida:\n")
(decompress (compress img42))
(display "\n\n")
;|#

