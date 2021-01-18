warning("Changed dir from inside ./www/ to ./app_supplementary/.
        May need to fix filepaths.")

library("spinifex")
library("ggplot2")
library("gganimate")
library("gifski")

set.seed(20200927) ## If tourr starts using seeds

### Global parameters
height_in <- 5 ## fixed to 8" x 5" for small screens
height_px <- height_in * 72  ## "screen" is 72 dpi/ppi
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1, 2, 3, 6, 8)]
## bas_p4
.ang <- seq(0, pi, length.out = 5)[-5] ## p + 1
.u_circ_p4 <- as.matrix(data.frame(x = sin(.ang), y = cos(.ang)))
bas_p4 <- tourr::orthonormalise(.u_circ_p4)
# tourr::is_orthonormal(bas_p4)
# spinifex::view_frame(bas_p4)

## bas_p6
.ang <- seq(0, pi, length.out = 7)[-7] ## p + 1
.u_circ_p6 <- as.matrix(data.frame(x = sin(.ang), y = cos(.ang)))
bas_p6 <- tourr::orthonormalise(.u_circ_p6)
# tourr::is_orthonormal(bas_p6)
# spinifex::view_frame(bas_p6)

### Aesthetic options
angle <- .1
fps   <- 5L ## .gif format only handles factors of 100!?
max_frames <- 90L
axes_position <- "left"
pt_size <- 3L
###

#### Load all data script
## load_all_data <- function(){
{
  ## Load all sim names, for dev control
  root <- "./apps/study/www/data"
  ## Can't use here::here(); Filepaths cannot be too long...
  print(normalizePath(root))
  sim_nms <- c("EEE_p4_0_1",    "EEE_p4_33_66",    "EEE_p4_50_50",
               "EEV_p4_0_1",    "EEV_p4_33_66",    "EEV_p4_50_50",
               "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
               "EEE_p6_0_1",    "EEE_p6_33_66",    "EEE_p6_50_50",
               "EEV_p6_0_1",    "EEV_p6_33_66",    "EEV_p6_50_50",
               "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
  sim_nms <- c(paste0("EEE_p4_0_1_t", 1:3), ## 3 training sets
               as.vector(outer(sim_nms, paste0("_rep", 1:3), FUN = "paste0"))) ## Cross product paste
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
}

### save_pca(), for 1 sim
save_pca <- function(sim_nm = "EEE_p4_0_1_rep1"){
  x <- data.frame(x = 1:4)
  y <- data.frame(y = 1:4)
  pc_opts <- merge(x, y, all = TRUE)[-c(1, 6, 11, 16), ] ## Less trivial combinations
  
  ## Loop over sim_nm
  invisible(lapply(sim_nm, function(this_sim_nm){
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    pca_rot <- prcomp(this_sim)$rotations
    
    ## Loop over pc_opts
    invisible(lapply(1:nrow(pc_opts), function(row_i){
      this_bas  <- get(paste0("bas_p", p))
      this_clas <- attr(this_sim, "cluster")
      x_num <- pc_opts[row_i, 1]
      y_num <- pc_opts[row_i, 2]
      x_pc  <- paste0("PC", x_num)
      y_pc  <- paste0("PC", y_num)
      ## Save grand tour
      fn <- paste0(this_sim_nm, "__pca_x", x_num, "y", y_num, ".png")
      
      pca     <- prcomp(this_sim)
      pca_x   <- as.data.frame(pca$x[ , c(x_num, y_num)])
      pca_rot <- data.frame(pca$rotation[ , c(x_num, y_num)])
      pca_rot <- scale_axes(pca_rot, axes_position, pca_x)
      
      angle <- seq(0L, 2L * pi, length = 360L)
      circ  <- scale_axes(data.frame(x = cos(angle), y = sin(angle)),
                          axes_position, pca_x)
      zero  <- scale_axes(data.frame(x = 0L, y = 0L),
                          axes_position, pca_x)
      x_range <- max(pca_x[, 1L], circ[, 1L]) - min(pca_x[, 1L], circ[, 1L])
      y_range <- max(pca_x[, 2L], circ[, 2L]) - min(pca_x[, 2L], circ[, 2L])
      
      ### ggplot2
      gg <- ggplot() +
        ## Themes and aesthetics
        theme_void() +
        scale_colour_manual(values = pal) +
        scale_fill_manual(values = pal) +
        theme(legend.position = "none", ## no legend
              aspect.ratio = y_range / x_range) +
        ## Data points
        geom_point(pca_x,
                   mapping = aes(x = get(x_pc), y = get(y_pc),
                                 color = this_clas,
                                 fill  = this_clas,
                                 shape = this_clas),
                   size = 3L) +
        ## Axis segments
        geom_segment(pca_rot,
                     mapping = aes(x = get(x_pc), xend = zero[, 1L],
                                   y = get(y_pc), yend = zero[, 2L]),
                     size = 1L, colour = "grey50") +
        ## Axis label text
        geom_text(pca_rot,
                  mapping = aes(x = get(x_pc),
                                y = get(y_pc),
                                label = colnames(this_sim)),
                  size = 5L, colour = "grey50", fontface = "bold",
                  vjust = "outward", hjust = "outward") +
        ## Circle path
        geom_path(circ, mapping = aes(x = x, y = y),
                  color = "grey80", size = 1L, inherit.aes = F)
      
      
      suppressMessages(
        ggsave(fn, gg, device = "png", path = "./apps/study/www/images",
               height = height_in, units = "in", dpi = "screen")
      )
    }))
    
    message("Saved the 12x png images for PC permutations of ", this_sim_nm, ".")
  }))
}


### save_grand(), for 1 sim
## !ERR from tourr::interpolate(), within play_tour_path();
## !ERR, Error in dim(x) <- length(x) : attempt to set an attribute on NULL
#### did we see this before? i think the examples still work.
save_grand <- function(sim_nm = "EEE_p4_0_1_rep1"){
  ## Loop over sim_nm,
  lapply(sim_nm, function(this_sim_nm){
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    this_bas <- get(paste0("bas_p", p))
    this_clas <- attr(this_sim, "cluster")
    ## Grand specific
    this_tpath <- get(paste0("tpath_p", p)) ##TODO: this needs to cover training sets as well.
    
    ## Save grand tour
    fn <- paste0(this_sim_nm, "__grand.gif")
    
    ## manually wade through play_tour_path, odd behavior occuring
    tour_path <- tourr::interpolate(basis_set = this_tpath, angle = angle)
    attr(tour_path, "class") <- "array"
    tour_df <- array2df(array = tour_path, data = this_sim)
    
    gg <- render_(frames = tour_df,
                  axes = axes_position,
                  line_size = 1L,
                  aes_args = list(color = this_clas, shape = this_clas),
                  identity_args = list(size = pt_size),
                  ggproto = list(theme_spinifex(),
                                 theme(legend.position = "none"),
                                 scale_colour_manual(values = pal))
    )
    gga <- gg + gganimate::transition_states(frame, transition_length = 0L)
    anim <- gganimate::animate(gga, fps = 5L, height = height_px, renderer = gifski_renderer())
    ## Save.
    gganimate::anim_save(filename = fn,
                         animation = anim,
                         path = "./apps/study/www/images")
    
    message("Saved a grand tour gif of ", this_sim_nm, ".")
  })
}



### save_radial(), for 1 sim
save_radial <- function(sim_nm = "EEE_p4_0_1_rep1"){
  ## Loop over sim_nm,
  invisible(lapply(sim_nm, function(this_sim_nm){
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    this_bas <- get(paste0("bas_p", p))
    this_clas <- attr(this_sim, "cluster")
    
    ## Loop over number of columns, setting mv, and save a radial.
    invisible(lapply(1:p, function(this_mv){
      fn <- paste0(this_sim_nm, "__radial_mv", this_mv, ".gif")
      
      tour_hist <- manual_tour(basis = this_bas, manip_var = this_mv)
      tour_df <- array2df(array = tour_hist, data = this_sim)
      gg <- render_(frames = tour_df,
                    axes = axes_position,
                    aes_args = list(color = this_clas, shape = this_clas),
                    identity_args = list(size = pt_size),
                    ggproto = list(theme_spinifex(),
                                   theme(legend.position = "none"),
                                   scale_colour_manual(values = pal))
      )
      gga <- gg + gganimate::transition_states(frame, transition_length = 0L)
      anim <- gganimate::animate(gga, fps = 5L, height = height_px, renderer = gifski_renderer())
      ## Save.
      gganimate::anim_save(filename = fn,
                           animation = anim,
                           path = "./apps/study/www/images")
    }))
    
    message("Saved all ", p, " radial tours for each mv of ", this_sim_nm, ".")
  }))
}
#' @examples 
#' save_radial("EEE_p4_0_1_t1")


save_all_static <- function(){
  require(tictoc); require(beepr)
  tic("outside loop")
  invisible(lapply(1:length(sim_nms), function(i){
    # tic("pca")
    # save_pca(sim_nms[i])
    # toc()
    # tic("grand")
    # save_grand(sim_nms[i])
    # toc()
    tic("radial")
    save_radial(sim_nms[i])
    toc()
  }))
  toc("outside loop")
  beepr::beep(4); message("DONE. =========== end of save_all_static")
}
#' @examples 
#' save_all_static()
#' 