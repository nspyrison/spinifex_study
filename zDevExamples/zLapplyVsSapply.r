my_l <- list( 1, 3, 5:7)
lapply(my_l, FUN = function(vect_i){vect_i +1})
lapply(my_l, FUN = function(vect_i){sum(vect_i) +1})
sapply(my_l, USE.NAMES = TRUE, FUN = function(vect_i){sum(vect_i) +1}) 
##NOTICE CANNOT COLLAPSE TO VECTOR/ MATRIX CAUSE VARYIGN LENGTH
(out <- sapply(my_l, USE.NAMES = TRUE, FUN = function(vect_i){sum(vect_i) +1}))
## A numeric vecto
str(out)
class(out)
