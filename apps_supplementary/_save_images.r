##### Initialize and setup -----
require("spinifex")
require("ggplot2")
require("gganimate")
require("gifski")

set.seed(20200927L) ## If tourr starts using seeds

### Global parameters
height_in <- 5L ## fixed to 8" x 5" for small screens
height_px <- height_in * 72L  ## "screen" is 72 dpi/ppi by default
pal <- RColorBrewer::brewer.pal(8L, "Dark2")[c(1L, 2L, 3L, 6L, 8L)]
## bas_p4
.ang <- seq(0L, pi, length.out = 5L)[-5L] ## p + 1
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
fps   <- 5L     ## fps of .gif. ##.gif format only handles factors of 100!?
duration <- 15L ## seconds duration of the gif
max_frames <- fps * duration ## ~75, the frame of the grand tour to cut down to.
axes_position <- "left"
pt_size <- 3L
###

#### Load all data script ----
## load_all_data <- function(){
{
  ## Vector of sim_nms
  sim_nms <- c("EEE_p4_0_1",    "EEE_p4_33_66",    "EEE_p4_50_50",
               "EEV_p4_0_1",    "EEV_p4_33_66",    "EEV_p4_50_50",
               "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
               "EEE_p6_0_1",    "EEE_p6_33_66",    "EEE_p6_50_50",
               "EEV_p6_0_1",    "EEV_p6_33_66",    "EEV_p6_50_50",
               "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
  sim_nms <- c(paste0("EEE_p4_0_1_t", 1L:3L), ## 3 training sets
               as.vector(outer(sim_nms, paste0("_rep", 1L:3L), FUN = "paste0"))) ## Cross product paste
  root <- "./apps_supplementary/data"   ## Can't use here::here(); Filepaths cannot be too long...
  sim_fps <- paste0(root, "/", sim_nms, ".rda")
  ## Load all simulations
  for(i in 1L:length(sim_nms)){
    load(sim_fps[i], envir = globalenv())
  }
  
  ## Load the few tpaths.
  tpath_nms <- paste0("tpath_", c("p4_t", "p4", "p6"))
  tpath_fps <- paste0(root, "/", tpath_nms, ".rda")
  for(i in 1L:length(tpath_nms)){
    load(tpath_fps[i], envir = globalenv())
  }
}

### save_pca(), for 1 simulation
save_pca <- function(sim_nm = "EEE_p4_0_1_rep1"){
  x <- data.frame(x = 1L:4L)
  y <- data.frame(y = 1L:4L)
  pc_opts <- merge(x, y, all = TRUE)[-c(1L, 6L, 11L, 16L), ] ## Remove trivial combinations like PC1 by PC1/
  
  ## Loop over sim_nm
  invisible(lapply(sim_nm, function(this_sim_nm){
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    pca_rot <- prcomp(this_sim)$rotations
    
    ## Loop over pc_opts
    invisible(lapply(1L:nrow(pc_opts), function(row_i){
      fn <- paste0(this_sim_nm, "__pca_x", x_num, "y", y_num, ".png")
      this_bas  <- get(paste0("bas_p", p))
      this_clas <- attr(this_sim, "cluster")
      x_num <- pc_opts[row_i, 1L]
      y_num <- pc_opts[row_i, 2L]
      x_pc  <- paste0("PC", x_num)
      y_pc  <- paste0("PC", y_num)
      
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
      
      ### Visualize
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
                  color = "grey80", size = 1L, inherit.aes = FALSE)
      
      ## Save
      suppressMessages(
        ggsave(fn, gg, device = "png", path = "./apps/spinifex_study/www/images",
               height = height_in, units = "in", dpi = "screen")
      )
    }))
    message("Saved the 12x png images for PC permutations of ", this_sim_nm, ".")
  }))
}


### save_grand(), for 1 simulation
save_grand <- function(sim_nm = "EEE_p4_0_1_rep1"){
  ## Loop over sim_nm,
  lapply(sim_nm, function(this_sim_nm){
    fn <- paste0(this_sim_nm, "__grand.gif")
    this_sim <- get(this_sim_nm)
    p <- ncol(this_sim)
    this_bas <- get(paste0("bas_p", p))
    this_clas <- attr(this_sim, "cluster")
    ## Grand specific, tour paths 
    this_tpath <- get(paste0("tpath_p", p))
    tour_path <- tourr::interpolate(basis_set = this_tpath, 
                                    angle = angle)[, , 1L:max_frames]
    attr(tour_path, "class") <- "array"
    tour_df <- array2df(array = tour_path, data = this_sim)
    ## Visualize
    gg <- render_(frames = tour_df,
                  axes = axes_position,
                  line_size = 1L,
                  aes_args = list(color = this_clas, shape = this_clas),
                  identity_args = list(size = pt_size),
                  ggproto = list(theme_spinifex(),
                                 theme(legend.position = "none"),
                                 scale_colour_manual(values = pal))
    )
    ## Animate
    gga <- gg + gganimate::transition_states(frame, transition_length = 0L)
    anim <- gganimate::animate(gga, fps = fps, height = height_px, 
                               renderer = gifski_renderer())
    ## Save
    gganimate::anim_save(filename = fn,
                         animation = anim,
                         path = "./apps/spinifex_study/www/images")
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
    invisible(lapply(1L:p, function(this_mv){
      fn <- paste0(this_sim_nm, "__radial_mv", this_mv, ".gif")
      tour_hist <- manual_tour(basis = this_bas, manip_var = this_mv)
      tour_df <- array2df(array = tour_hist, data = this_sim)
      ## Visualize
      gg <- render_(frames = tour_df,
                    axes = axes_position,
                    aes_args = list(color = this_clas, shape = this_clas),
                    identity_args = list(size = pt_size),
                    ggproto = list(theme_spinifex(),
                                   theme(legend.position = "none"),
                                   scale_colour_manual(values = pal))
      )
      ## Animate
      gga <- gg + gganimate::transition_states(frame, transition_length = 0L)
      anim <- gganimate::animate(gga, fps = fps, height = height_px, renderer = gifski_renderer())
      ## Save
      gganimate::anim_save(filename = fn,
                           animation = anim,
                           path = "./apps/spinifex_study/www/images")
    }))
    
    message("Saved all ", p, " radial tours for each mv of ", this_sim_nm, ".")
  }))
}
#' @examples 
#' save_radial("EEE_p4_0_1_t1")


## save all/subset of factors looping over all simulations in `sim_nms`
#' @example
#' sim_nms <- paste0("EEE_p4_0_1_t", 1L:3L) ## FOR TEST SUBSET
#' save_all_static()
save_all_static <- function(){
  require(tictoc)
  tic("outside loop")
  invisible(lapply(1L:length(sim_nms), function(i){
    # tic("pca")
    # save_pca(sim_nms[i])
    # toc()
    # tic("grand")
    # save_grand(sim_nms[i]) ## Give error (external code) at end, but all .gifs are saved.
    # toc()
    tic("radial")
    save_radial(sim_nms[i])
    toc()
  }))
  toc("outside loop")
}
#' @examples 
#' save_all_static()
#' 