set terminal svg enhanced size 480,256 fsize 10
set output "horner.svg"

plot "./data" using 1:2 with lines title 'horner', \
     "./data" using 1:3 with lines title 'exn\_horner'
