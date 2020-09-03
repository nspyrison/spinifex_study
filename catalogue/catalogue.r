if(F)
  file.edit("./catalogue/sim_mclust.r")
root <- "./catalogue/simulations/"
load(file = paste0(root, "sim_eii.rds"))
load(file = paste0(root, "sim_eee.rds"))
load(file = paste0(root, "sim_vvv.rds"))



##### DEPRICATED VISUAL APPROACH ----
if(F){
  ## Setup -----
  require("tibble")
  require("dplyr")
  require("ggplot2")
  require("ggforce") ## For geom_ellipse
  source("./R/sim_pDim_kCl.r") ## Cluster levels stored in attribute "cl_lvl". Try attr(mySim, "cl_lvl")
  set.seed(20200912)
  
  ## Planning table -----
  l <- 6
  description <- 
    tibble(id  = 1:l,
           dim = rep(2, l),
           distribution = c(rep("Spherical", 2),
                            rep("Diagonal", 4)), ## %in% c("Spherical", "Diagonal", "Ellipsoidal")
           Volume       = rep(c("Equal", "Variable"), 
                              length.out = l),   ## %in% c("Equal", "Variable")
           Shape        = c(rep("Equal", 4), 
                            rep("Variable", 2)), ## %in% c("Equal", "Variable")
           Orientation  = c(rep("---", 2), 
                            rep("Coordinate axes", 4)), ## %in% c("---", "Coordinate axes", "Equal", "Variable")
           mclust_name  = c("EII", "VII", "EEI", "VEI", "EVI", "VVI")
    )
  description$name <- paste0(
    substr(description$Volume, 1, 1),
    substr(description$Shape,  1, 1),
    substr(description$Orientation, 1, 1)
  )
  description
  
  ## Make data frame for ggplot2 ------
  #l <- 6 ## l set above
  cl <- 3
  r <- cl * l
  w <- 16/3
  h <- 3
  s <- 1.5 ## Scale coeffient
  df <- 
    data.frame(
      facet = rep(description$name[1:l], each = cl),
      cl = paste0("Cl ", rep(letters[1:cl], length.out = r)),
      x  = rep(c(0, 10, 5), length.out = r),
      y  = rep(c(0, 0, -5), length.out = r),
      ##     EE-,    VE-,    EEC,    VEC,        EVI,    VVC
      a  = c(4,4,4,  4,6,2,  w,w,w,  w,w*s,w/s,  4,w,w,  4,w*s,w/s)/s, 
      b  = c(4,4,4,  4,6,2,  h,h,h,  h,h*s,h/s,  4,h,h,  4,h*s,h/s)/s, 
      ang = rep(0, length.out = r)
    ) %>% 
    mutate(x1    = x - a * cos(ang),
           y1    = y - b * sin(ang),
           xend1 = x + a * cos(ang),
           yend1 = y + b * sin(ang),
           x2    = x - a * sin(ang),
           y2    = y - b * cos(ang),
           xend2 = x + a * sin(ang),
           yend2 = y + b * cos(ang)
    )
  
  ## ggplot2 plots -----
  ggplot(data = df) + 
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b, angle = ang,  color = cl),
                 size = 1.1) + 
    geom_segment(aes(x = x1, y = y1, xend = xend1, yend = yend1, color = cl),
                 linetype = 2, lwd = 1) +
    geom_segment(aes(x = x2, y = y2, xend = xend2, yend = yend2, color = cl),
                 linetype = 2, lwd = 1) +
    facet_wrap(vars(facet)) +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed() +
    labs(x = "PC1", y = "PC2") +
    theme_bw()
  
  ## Save ggplot2 example
  if(F){
    ggsave(filename = "./catalogue/output/mclust_1.6_ggplot2.png")
  }
  
  
  ### rgl -----
  { ## Setup
    library("rgl")
    options(rgl.useNULL = TRUE) ## if FALSE prints to an X11 window. (MacOS needs XQuarts for X11.)
    options(rgl.printRglwidget = T) ## if TRUE sends the scene to the RStuido Viewer pane after every change.
    ## Call rglwidget() to display the scene
    
    nsCloseRGL <- function(n_tries = 5) {
      try_num <- 1
      while (try_num <= n_tries){
        try(rgl.close(), silent = TRUE)
        try_num <- try_num + 1
      }
    }
  }
  # ?rgl::ellipse3d()
  # args(sim_pDim_kCl) ## Added density option; sim_pDim_kCl(method = "d")
  # print("see the roxygen-like example in sim_pDim_kCl.r")
  
  ## Init params
  p <- 3 ## Fix to 3
  mns <- list(c(0,0,0), c(10,0,0), c(5,-5,0))
  covs1 <- rep(list(diag(p)), 3)
  diag_p <- diag(p)
  covs2 <- list(matrix(c(2,0,0, 0,1,0, 0,0,1), p),
                matrix(c(1,0,0, 0,2,0, 0,0,1), p),
                matrix(c(1,0,0, 0,1,0, 0,0,2), p)
  )
  install.packages("mclust")
  library("mclust")
  help(package = "mclust")
  
  num_sims <- 2
  i_s <- 1:num_sims
  ## Create sim#, clas#; sim1, sim2, ..., 
  for(i in i_s){
    cov_obj <- get(paste0("covs",i))
    assign(paste0("sim",i), sim_pDim_kCl(mns, cov_obj, do_shuffle = FALSE))
  }
  ## clas will be the same number and order unless changed.
  clas <- attr(sim1, "cl_lvl")
  pal <- RColorBrewer::brewer.pal(8, "Dark2")
  col <- pal[as.integer(as.factor(clas))]
  
  ## Render rgl
  try(rgl.close(), silent = T)
  open3d(FOV = 0, zoom = 1)
  mfrow3d(1, 2, sharedMouse = TRUE) ## function of num sims
  
  for(i in i_s){
    cov_obj <- get(paste0("covs",i))
    j_s <- 1:length(cov_obj)
    bbox3d(xlen = 0, ylen = 0, zlen = 0, color = "darkgrey", lwd = 1, alpha = .5)
    spheres3d(sim_obj, radius = .15, col = col, alpha = .3)
    for(j in 1:3)
      plot3d(ellipse3d(cov_obj[[j]], centre = mns[[j]], level = .68), 
             col = pal[j], alpha = 0.5, add = TRUE)
    if(i != max(i_s)) rgl::next3d()
  }
  rgl::highlevel()
}
