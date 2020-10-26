source("~/R/spinifex_study/apps/study/factor_block_permutation")
input_sims <- add_participant(100)$perm_RAND$this_sim_nms
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1, 2, 3, 6, 8)]

#### Single ggplot primitives
## _for pca:
plot_single_pca <- function(sim_nm, x_num, y_num){
  ## Initialize
  dat_std <- get(sim_nm)
  cluster <- attr(dat_std, "cluster")
  axes_position <- "left"
  x_pc <- paste0("PC", x_num)
  y_pc <- paste0("PC", y_num)
  
  ## PCA
  pca     <- prcomp(dat_std)
  pca_x   <- as.data.frame(pca$x)
  pca_rot <- data.frame(pca$rotation[, c(x_num, y_num)]) 
  pca_rot <- scale_axes(pca_rot, axes_position, pca_x)
  
  ## Basis unit circle
  angle   <- seq(0L, 2L * pi, length = 360L)
  circ    <- scale_axes(data.frame(x = cos(angle), y = sin(angle)),
                        axes_position, pca_x)
  zero    <- scale_axes(data.frame(x = 0L, y = 0L),
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
                             color = cluster,
                             fill  = cluster,
                             shape = cluster),
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
                            label = colnames(dat_std)),
              size = 5L, colour = "grey50", fontface = "bold",
              vjust = "outward", hjust = "outward") +
    ## Circle path
    geom_path(circ, mapping = aes(x = x, y = y),
              color = "grey80", size = 1L, inherit.aes = F)
  
  return(gg)
}

## _for grand tour:
plot_single_grand <- function(sim_nm = "EEE_p4_0_1"){
  ## Initialize
  dat_std <- get(sim_nm)
  cluster <- attr(dat_std, "cluster")
  .srt_char <- regexpr("p", sim_nm)[1]
  tpath <- get(paste0("tpath_", substr(sim_nm,
                                   start = .srt_char,
                                   stop = .srt_char + 1))) ## Will need _t at end if trainig.
  
  ##
  angle <- .1
  fps   <- 6L
  max_frames <- 90L ## 90 frame for 15 sec @ fps = 6
  axes_position <- "left"
  ##
  cluster     <- rep(cluster, max_frames)
  tour_array  <- tourr::interpolate(basis_set = tpath, angle = angle)
  attr(tour_array, "class") <- "array"
  
  gg_ls <- list()
  for(i in 1L:90L){
    gg_ls[[i]] <- 
      view_frame(basis = tour_array[,, i], data = dat_std,
                 axes = "left",
                 aes_args = list(color = cluster, shape = cluster),
                 identity_args = list(size = 3L), 
                 ggproto = list(theme_spinifex(), 
                                scale_colour_manual(values = pal)
                 ))
  }
  
  return(gg_ls)
}

## _for radial tour:
plot_single_radial <- function(sim_nm, mvar){
  ## Initialize
  dat_std <- get(sim_nm)
  cluster <- attr(dat_std, "cluster")
  bas <- basis_pca(dat_std)

  fps <- 6L
  axes_position <- "left"
  ## Needed only for setting scales and aspect ratio
  angle <- seq(0L, 2L * pi, length = 360L)
  circ  <- scale_axes(data.frame(x = cos(angle), y = sin(angle)),
                      axes_position, dat_std)
  x_range <- c(min(dat_std[, 1L], circ[, 1L]), max(dat_std[, 1L], circ[, 1L]))
  y_range <- c(min(dat_std[, 2L], circ[, 2L]), max(dat_std[, 2L], circ[, 2L]))
  ##
  tour_array <- manual_tour(basis = bas, manip_var = mvar)
  num_bas <- dim(tour_array)[3L]
  
  gg_ls <- list()
  for(i in 1L:num_bas){
    gg_ls[[i]] <- 
      view_frame(basis = tour_array[,, i], data = dat_std, 
                 manip_var = mvar, axes = "left",
                 aes_args =  list(color = cluster, shape = cluster),
                 identity_args = list(size = 3L),
                 ggproto = list(theme_spinifex(), 
                                scale_colour_manual(values = pal)
                 ))
  }
  
  return(gg_ls)
}


## Bringing it all together
preload_ggplots <- function(sim_nms = c("EEE_p4_0_1", "EEE_p4_33_66")){
  ## Load simulations
  root <- ("~/R/spinifex_study/apps/data") # here("apps/data/") ## Filepaths cannot be too long....
  sim_fps <- paste0(root, "/", sim_nms, ".rda")
  for(i in 1:length(sim_nms)){
    load(sim_fps[i])
  }
  ## Load the few tpaths
  tpath_nms <- paste0("tpath_", c("p4_t", "p4", "p6"))
  tpath_fps <- paste0(root, "/", tpath_nms, ".rda")
  for(i in 1:length(tpath_nms)){
    load(tpath_fps[i])
  }
  
  #### pca_ls ------
  ## Initialize axes permutations
  pc_opts <- 1:4
  axis_perms <- data.frame(x = pc_opts,
                        y = pc_opts) %>%
    tidyr::expand(x, y)
  axis_perms <- axis_perms[-c(1, 6, 11, 16), ] ## Remove duplicates.
  n_perms <- nrow(df_opts)
  ## Pre-loop initialization 
  l_sim_nms <- length(sim_nms)
  pca_ls <- list()
  for(i in 1:l_sim_nms){ ## LOOP OVER SIMS
    this_sim_nm <- sim_nms[i]
    
    for(j in 1:n_perms){ ## LOOP OVER axes permutations
      .axes_nums <- axis_perms[j, ]
      
      pca_ls[[(i - 1) * n_perms + j]] <- 
        plot_single_pca(sim_nm = this_sim_nm, 
                        x_num = .axes_nums$x, 
                        y_num = .axes_nums$y)
    }
  }
  names(pca_ls) <- paste(rep(sim_nms, each = n_perms),
                       paste0("_pca_x", 
                              rep(axis_perms$x, l_sim_nms),
                              "y", 
                              rep(axis_perms$y, l_sim_nms)),
                       sep = "_")
  ## Parent envr assign
  pca_ls <<- pca_ls
  
  
  #### grand_ls ------
  ## Initialize axes permutations
 
  ## Pre-loop initialization 
  l_sim_nms <- length(sim_nms)
  grand_ls <- list()
  for(i in 1:l_sim_nms){ ## LOOP OVER SIMS
    this_sim_nm <- sim_nms[i]
    grand_ls[[i]] <- 
        plot_single_grand(sim_nm = this_sim_nm)
    }
  }
  names(grand_ls) <- paste0(sim_nms, "_grand")
  length(grand_ls)
  ## Parent envr assign
  pca_ls <<- pca_ls
  
  }
  