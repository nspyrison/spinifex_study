library("spinifex")
library("here")      ## Fixing base dir
set.seed(20200927)   ## If tourr starts using seeds

height_px <- 500L
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1, 2, 3, 6, 8)]
bas_p4 <- matrix(c(.5,  .5,
                   -.5, .5,
                   -.5, -.5,
                   .5,  -.5),
                 ncol = 2, nrow = 4, byrow = TRUE)
bas_p6 <- matrix(c(.2887,  .5,
                   -.2887, .5,
                   -.5774, 0,
                   -.2887, -.5,
                   .2887,  -.5,
                   .5774,  0),
                 ncol = 2, nrow = 6, byrow = TRUE)

## Load all sim names, for dev control
sim_nms <- c("EEE_p4_0_1",    "EEE_p4_33_66",    "EEE_p4_50_50",
             "EEV_p4_0_1",    "EEV_p4_33_66",    "EEV_p4_50_50",
             "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
             "EEE_p6_0_1",    "EEE_p6_33_66",    "EEE_p6_50_50",
             "EEV_p6_0_1",    "EEV_p6_33_66",    "EEV_p6_50_50",
             "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
sim_nms <- c(paste0("EEE_p4_0_1_t", 1:3), ## 3 training sets
             as.vector(outer(sim_nms, paste0("_rep", 1:3), FUN = "paste0"))) ## cross product paste
sim_fps <- paste0(root, "/", sim_nms, ".rda")
for(i in 1:length(sim_nms)){
  load(sim_fps[i], envir = globalenv())
}
## Load the few tpaths.
tpath_nms <- paste0("tpath_", c("p4_t", "p4", "p6"))
tpath_fps <- paste0(root, "/", tpath_nms, ".rda")
for(i in 1:length(tpath_nms)){
  load(tpath_fps[i], envir = globalenv())
}


#### EXAMPLE -----
dat_std <- tourr::rescale(tourr::flea[, 1:6])
cluster <- tourr::flea$species
mv      <- manip_var_pca(dat_std)
### Aesthetic options
angle <- .1
fps   <- 6L
max_frames <- 90L
axes_position <- "left"
pt_size <- 3L
###


### save_pca(), for 1 sim ----
save_pca <- function(sim_nm = "EEE_p4_0_1"){
  pc <- data.frame(x = 1:4)
  pc_opts <- merge(pc, pc)[-c(1, 6, 11, 16), ] ## Less trivial combinations
  
  ## Loop over sim_nm
  lapply(sim_nm, function(this_sim_nm){
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    pca_rot <- prcomp(this_sim)$rotations
    
    ## Loop over pc_opts
    lapply(1:nrow(pc_opts), function(row_i){
      this_bas  <- get(paste0("bas_p", p))
      this_clas <- attr(this_sim, "cluster")
      
      ## Save grand tour
      fn <- paste0(this_sim, "__pca_.gif")
      
      ## TODO DO THE PCA HERE
    })
    
    message("Saved a PC images for", this_sim, ".")
  })
}

### save_grand(), for 1 sim ----
save_grand <- function(sim_nm = "EEE_p4_0_1"){
  
  ## Loop over sim_nm,
  lapply(sim_nm, function(this_sim_nm){
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    this_bas <- get(paste0("bas_p", p))
    this_clas <- attr(this_sim, "cluster")
    ## Grand specific
    this_tpath <- get(paste0("tpath_p", p)) ##TODO: this needs to cover training sets as well.
    
    ## Save grand tour
    fn <- paste0(this_sim, "__grand.gif")
    
    play_tour_path(basis = this_bas, data = this_sim, manip_var = this_mv,
                   axes = axes_position, fps = fps, angle = angle,
                   aes_args = list(color = this_clas, shape = this_clas),
                   identity_args = list(size = pt_size),
                   ggproto = list(theme_spinifex(),
                                  theme(legend.position = "none"),
                                  scale_colour_manual(values = pal)
                   ),
                   render_type = render_gganimate,
                   gif_filename = "radialTour_example.gif",
                   gif_path = "./apps/study/www/images"
    )
    message("Saved a grand tour for", this_sim, ".")
  })
}

### save_radial(), for 1 sim ----
save_radial <- function(sim_nm = "EEE_p4_0_1"){
  
  ## Loop over sim_nm,
  lapply(sim_nm, function(this_sim_nm){
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    this_bas <- get(paste0("bas_p", p))
    this_clas <- attr(this_sim, "cluster")
    
    ## Loop over number of columns, setting mv, and save a radial.
    hiden <- lapply(1:p, function(this_mv){
      fn <- paste0(this_sim, "__radial_mv", this_mv, ".gif")
      
      play_manual_tour(basis = this_bas, data = this_sim, manip_var = this_mv,
                       axes = axes_position, fps = fps, angle = angle,
                       aes_args = list(color = this_clas, shape = this_clas),
                       identity_args = list(size = pt_size),
                       ggproto = list(theme_spinifex(),
                                      theme(legend.position = "none"),
                                      scale_colour_manual(values = pal)
                       ),
                       render_type = render_gganimate,
                       gif_filename = "radialTour_example.gif",
                       gif_path = "./apps/study/www/images"
      )
    })
    message("Saved radial tours for each mv of ", this_sim, ".")
  })
}



