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
                    (pixrgb-d 0 1 15 65 120 2)
                    (pixrgb-d 1 0 15 65 120 2)
                    (pixrgb-d 1 1 15 65 120 2)
                    (pixrgb-d 2 0 15 65 120 2)
                    (pixrgb-d 2 1 15 65 120 2)
                    (pixrgb-d 3 0 15 65 120 2)
                    (pixrgb-d 3 1 15 65 120 2)))

(define img3 (image 2 2 (pixhex-d 0 0 "#AABBCC" 2) (pixhex-d 0 1 "#AABBCC" 2) (pixhex-d 1 0 "#AABBCC" 2) (pixhex-d 1 1 "#AABBCC" 2)))

(display "Es img1 un bitmap? ")
(bitmap? img1)
(display "Es img2 un pixmap? ")
(pixmap? img2)
(display "Es img3 un hexmap? ")
(hexmap? img3)

img1
(flipH img1)
(flipV img1)
(rotate90 img1)


