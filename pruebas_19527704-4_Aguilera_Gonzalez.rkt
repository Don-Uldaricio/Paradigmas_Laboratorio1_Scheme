#lang racket

(require "funciones.rkt")

;-------------------------------------------------------------

(define img1 (image 4 2
                    (pixbit-d 0 0 1 2)
                    (pixbit-d 0 1 1 2)
                    (pixbit-d 1 0 1 2)
                    (pixbit-d 1 1 1 2)
                    (pixbit-d 2 0 1 2)
                    (pixbit-d 2 1 1 2)
                    (pixbit-d 3 0 1 2)
                    (pixbit-d 3 1 1 2)))

(define img2 (image 4 2
                    (pixrgb-d 0 0 15 65 120 2)
                    (pixrgb-d 0 1 15 34 120 2)
                    (pixrgb-d 1 0 15 67 120 2)
                    (pixrgb-d 1 1 15 65 40 2)
                    (pixrgb-d 2 0 15 65 30 2)
                    (pixrgb-d 2 1 15 65 120 2)
                    (pixrgb-d 3 0 15 65 120 2)
                    (pixrgb-d 3 1 50 65 120 2)))

(define img3 (image 2 2 (pixhex-d 0 0 "#AA45CC" 2) (pixhex-d 0 1 "#AAEDCC" 2) (pixhex-d 1 0 "#AABBCC" 2) (pixhex-d 1 1 "#AABBCC" 2)))

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

(define pixlist (getPixels img3))

(define (listaColores pixlist)
  (if (null? pixlist)
      null
      (cons (getColor (car pixlist)) (listaColores (cdr pixlist)))))

(listaColores pixlist)
(display "Eliminamos los colores repetidos obteniendo la siguiente lista de colores:\n")
(define finalColors (remove-duplicates (listaColores pixlist)))
finalColors


(define (colorFreq colorList pixlist)
  (define equalColor? (lambda (color px)
                          (if (= (getColor px) color) #t #f)))
  (if (null? colorList)
      null
      (cons (cons (car colorList) (count (lambda (px) (if (equal? (getColor px) (car colorList)) #t #f)) pixlist)) (colorFreq (cdr colorList) pixlist))))

(colorFreq finalColors pixlist)