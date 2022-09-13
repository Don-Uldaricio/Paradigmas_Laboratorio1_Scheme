#lang racket

(require "funciones.rkt")
(require "TDAPixel.rkt")
;(require "TDAHistogram.rkt")


;-------------------------------------------------------------

(define img1 (image 4 2
                    (pixbit-d 0 0 1 2)
                    (pixbit-d 1 0 1 2)
                    (pixbit-d 2 0 1 2)
                    (pixbit-d 3 0 0 2)
                    (pixbit-d 0 1 0 2)
                    (pixbit-d 1 1 0 2)
                    (pixbit-d 2 1 0 2) 
                    (pixbit-d 3 1 1 2)))

(define img2 (image 4 2
                    (pixrgb-d 0 0 15 34 120 2)
                    (pixrgb-d 1 0 15 67 120 2)
                    (pixrgb-d 2 0 15 65 30 2)
                    (pixrgb-d 3 0 15 65 120 2)
                    (pixrgb-d 0 1 15 65 120 2)
                    (pixrgb-d 1 1 15 65 40 2)          
                    (pixrgb-d 2 1 15 65 120 2) 
                    (pixrgb-d 3 1 50 65 120 2)))

(define img3 (image 2 2 (pixhex-d 0 0 "#AA45CC" 2) (pixhex-d 1 0 "#AAEDCC" 2) (pixhex-d 0 1 "#AABBCC" 2) (pixhex-d 1 1 "#AA45CC" 2)))

(define img4 (image 4 2
                    (pixbit-d 0 0 1 2)
                    (pixbit-d 2 0 1 2)
                    (pixbit-d 1 0 1 2)
                    (pixbit-d 3 1 1 2)
                    (pixbit-d 3 0 0 2)
                    (pixbit-d 0 1 0 2)
                    (pixbit-d 1 1 0 2)
                    (pixbit-d 2 1 0 2)))

img1
img2
img3
(display "Es img1 un bitmap? ")
(bitmap? img1)
(display "Es img2 un pixmap? ")
(pixmap? img2)
(display "Es img3 un hexmap? ")
(hexmap? img3)

img2
(flipH img2)
(flipV img2)
(rotate90 img2)
(imgRGB->imgHex img2)
(crop img2 1 0 2 1)
(display "-------- CAMBIO ---------------\n")
(display "Creamos una lista con los colores de cada pixel, eliminando los repetidos:\n")


(define pixlist (getPixels img3))
(define finalColors (imgColors pixlist))
finalColors
(histogram img3)
(display "El color más frecuente del histograma es:\n")
(mostFreqColor (histogram img3))

(display "-------- CAMBIO ---------------\n")
(display "Uso de funciones + edit:\nSe muestra el bitmap img1 y luego se invierten los bits de sus pixeles:\n")
img1
(edit invertColorBit img1)

(display "-------- CAMBIO ---------------\n")
(display "Se muestra el pixmap img2 y luego se invierten los canales de sus pixeles:\n")
img2
(edit invertColorRGB img2)

(display "---------CAMBIO---------------\nimage->string:\n")
(image->string img1 pixbit->string)
(image->string img2 pixrgb->string)
(image->string img3 pixhex->string)

(display "---------CAMBIO---------------\ncompress img1:\n")
img1
(compress img1)
(compressed? (compress img1))
(compressed? img1)

(display "---------CAMBIO---------------\ncompress img2:\n")
img2
(compress img2)
(compressed? (compress img2))
(compressed? img2)

(define (sortImage img)
  (define sortPixels (lambda (pixlist) (sort (sort pixlist #:key caar <) #:key cadar <)))
  (list (getWidth img) (getHeight img) (getCompColor img) (sortPixels (getPixels img)) (getCompPixels img)))

(define (recoverCompPixels pixlist color)
  (if (null? pixlist)
      null
      (cons (list (list (first (car pixlist)) (second (car pixlist))) color (third (car pixlist))) (recoverCompPixels (cdr pixlist) color))))

(define (decompress img)
  (sortImage (list (getWidth img) (getHeight img) (list) (append (getPixels img) (recoverCompPixels (getCompPixels img) (getCompColor img))) (list))))

(display "\n---------CAMBIO---------------\ncompress img3:\n")
img3
(define compimg3 (compress img3))
compimg3
(display "Está la (compress img3) comprimida? ")
(compressed? (compress img3))
(display "Está la img3 comprimida? ")
(compressed? img3)
(display "Descomprimimos la img3:\n")
(decompress compimg3)
(display "Son iguales la imagen img3 y la (decompress compimg3)? ")
(equal? (decompress compimg3) img3)



(display "\n---------CAMBIO---------------\nOrdenar pixeles de una imagen\n")
img4
(sortImage img4)



