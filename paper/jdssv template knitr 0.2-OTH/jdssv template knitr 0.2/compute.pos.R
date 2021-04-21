compute.pos <- function(X){
  R <- matrix(c(cos(pi/4), -sin(pi/4), sin(pi/4), cos(pi/4)), 2, 2)
  # Compute proper alingment of labels in pos
  # pos:           p1 p2    
  # 1 = below      1 -1  
  # 2 = left      -1 -1
  # 3 = top       -1  1
  # 4 = right      1  1
  pos <- sign(X %*% R)
  pos[pos==0] <- 1      # Avoid problems for the origin
  pos <- cbind(rep(1, nrow(pos)), pos, pos[,1]*pos[,2]) %*% c(2.5, 0, 1.0, 0.5)
  return(pos)
}