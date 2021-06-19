## All utility functions ------
## Aggregated utility / local functions to abstract away coding complexity.

### Originally clean_participant_data.r ------
require("dplyr")

#' @example  
#' ss_id <- "1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU" ## Hash or name of the google sheet
#' googlesheets4::gs4_auth(
#'   cache = ".secrets", email = "nicholas.spyrison@monash.edu")
#' 
#' raw <- googlesheets4::read_sheet(ss_id, sheet = 1L, range = "A:AG")
#' raw <- raw %>% filter(!is.na(plot_active)) # Remove dummy rows with application notes
#' ## Doesn't work in mutate:
#' raw$full_perm_num = unlist(as.integer(raw$full_perm_num))
#' raw$prolific_id = unlist(as.character(raw$prolific_id))
#' str(raw)
#' ## 
#' saveRDS(raw, "./apps_supplementary/PATH_TO_data/raw_SUFFIX.rds")


#' @example
#' ans_tbl <- readRDS(file = "./apps/spinifex_study/www/ans_tbl.rds")
#' tgt <- ans_tbl %>% dplyr::filter(sim_nm == "EEV_p6_33_66_rep2")
#' dat_longer <- pivot_longer_resp_ans_tbl(tgt)
#' 
#' 
#' ## Raw data
#' raw <- readRDS("./raw_nick_pilot.rds")
#' ## Filter to only task data
#' dat_active <- raw[raw$plot_active == TRUE, ]
#' ## Read and join ans_tbl, clean up 0 to NA on v5/v6.
#' ans_tbl   <- readRDS(file = "../../apps/spinifex_study/www/ans_tbl.rds")
#' resp_ans  <- dplyr::left_join(x = dat_active, y =  ans_tbl, by = "sim_nm") %>%
#'     mutate(v5_resp = ifelse(p_dim == "p4", NA_integer_, v5_resp),
#'            v6_resp = ifelse(p_dim == "p4", NA_integer_, v6_resp))
#' pivot_longer_resp_ans_tbl(resp_ans)

## Pivot variables longer and join (cbind)
pivot_longer_resp_ans_tbl <- function(dat){
  ## Fix col if dat was only from ans_tbl needed
  if(all(c("key", "v1_resp", "v1_marks") %in% colnames(dat)) == FALSE){
    message("dat was only from ans_tbl, fixing columns")
    dat <- dat %>%
      dplyr::mutate(.keep = "all",
                    key = sim_nm,
                    participant_num = 0L,
                    full_perm_num = 0L,
                    bar         = bar,
                    write_dt    = "NA",
                    factor      = NA,
                    p_dim       = NA,
                    prolific_id = NA,
                    period      = NA,
                    vc          = NA,
                    location    = NA,
                    eval        = NA,
                    v1_resp     = 0L,
                    v2_resp     = 0L,
                    v3_resp     = 0L,
                    v4_resp     = 0L,
                    v5_resp     = 0L,
                    v6_resp     = 0L,
                    v1_marks    = 0L,
                    v2_marks    = 0L,
                    v3_marks    = 0L,
                    v4_marks    = 0L,
                    v5_marks    = 0L,
                    v6_marks    = 0L,
                    sec_to_resp = 0L,
                    sec_on_pg   = 0L,
                    input_inter = NA,
                    resp_inter  = NA)
  }
  
  ## Pivot each var longer and cbind  back together at end.
  resp_longer <- dat %>%
    dplyr::select(c(key, participant_num, full_perm_num, prolific_id, sim_nm,
                    period, eval, factor, vc, p_dim, location,
                    input_inter, resp_inter, sec_to_resp, sec_on_pg, bar, write_dt ,
                    v1_resp:v6_resp)) %>%
    tidyr::pivot_longer(cols = v1_resp:v6_resp,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "resp",
                        values_drop_na = TRUE)
  
  marks_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_marks:v6_marks)) %>%
    tidyr::pivot_longer(cols = v1_marks:v6_marks,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "marks",
                        values_drop_na = TRUE)
  signal_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_signal:v6_signal)) %>%
    tidyr::pivot_longer(cols = v1_signal:v6_signal,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "signal",
                        values_drop_na = TRUE)
  diff_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_diff:v6_diff)) %>%
    tidyr::pivot_longer(cols = v1_diff:v6_diff,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "diff",
                        values_drop_na = TRUE)
  weight_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_weight:v6_weight)) %>%
    tidyr::pivot_longer(cols = v1_weight:v6_weight,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "weight",
                        values_drop_na = TRUE)
  if(all.equal(
    nrow(resp_longer), nrow(marks_longer),
    nrow(signal_longer), nrow(diff_longer), nrow(weight_longer)
  )){
    ## cbind(), left_join not working.
    ret <- cbind(
      resp_longer, marks_longer[, 4L], signal_longer[, 4L],
      diff_longer[, 4L], weight_longer[, 4L]
    )
  }else{error("!!!all nrow() not equal!!!")}
  ret <- ret %>%
    dplyr::mutate(key = as.factor(key),
                  participant_num = factor(participant_num,
                                           levels = 1L:max(participant_num)),
                  full_perm_num = factor(full_perm_num, 
                                         levels = 1L:max(full_perm_num)),
                  prolific_id = as.factor(prolific_id),
                  sim_nm = as.factor(sim_nm),
                  factor = factor(factor, levels = c("pca", "grand", "radial")),
                  period = as.factor(period),
                  eval = factor(eval, levels = 
                                  c("t1", 1L:2L, "t2", 3L:4L, "t3", 5L:6L)),
                  is_training = ifelse(substr(eval, 1L, 1L) == "t", TRUE, FALSE),
                  vc = factor(vc, c("EEE", "EEV", "banana")),
                  p_dim = factor(as.integer(substr(p_dim, 2L, 2L)),
                                 levels = c(4L, 6L)),
                  location = as.factor(location),
                  var_num = as.factor(substr(var_num, 2L, 2L)),
                  write_dt = lubridate::as_datetime(write_dt)
    ) %>% tibble::as_tibble()
  return(ret)
}



#' @example
#' ans_tbl <- readRDS(file = "./apps/spinifex_study/www/ans_tbl.rds")
#' tgt <- ans_tbl %>% dplyr::filter(sim_nm == "EEV_p6_33_66_rep2")
#' dat_longer <- pivot_longer_resp_ans_tbl(tgt)
#' dat_longer_agg <- aggregate_task_vars(dat_longer)
#' 
## Aggregate to the task grain.
aggregate_task_vars <- function(df_long){
  df_long %>%
    dplyr::group_by(key, participant_num, full_perm_num, prolific_id, sim_nm,
                    factor, period, eval, is_training, vc, p_dim, location) %>%
    dplyr::summarise(write_dt = first(write_dt),
                     task_input_inter = first(input_inter),
                     task_resp_inter = first(resp_inter),
                     sec_to_resp = first(sec_to_resp),
                     sec_on_pg = first(sec_on_pg),
                     cnt_resp = sum(resp),
                     task_marks = sum(marks),
                     z_sum_weight_check = sum(weight),
                     zz_sum_sq_weight_check = sum(weight^2)
    ) %>% dplyr::ungroup() %>%
    tibble::as_tibble()
}



### Originally pivot_longer_resp_ans_tbl.r -----
require("magrittr")

#' @example
#' ans_tbl <- readRDS(file = "./apps/spinifex_study/www/ans_tbl.rds")
#' tgt <- ans_tbl %>% dplyr::filter(sim_nm == "EEV_p6_33_66_rep2")
#' dat_longer <- pivot_longer_resp_ans_tbl(tgt)
#' 
#' 
#' ## Raw data
#' raw <- readRDS("./raw_nick_pilot.rds")
#' ## Filter to only task data
#' dat_active <- raw[raw$plot_active == TRUE, ]
#' ## Read and join ans_tbl, clean up 0 to NA on v5/v6.
#' ans_tbl   <- readRDS(file = "../../apps/spinifex_study/www/ans_tbl.rds")
#' resp_ans  <- dplyr::left_join(x = dat_active, y =  ans_tbl, by = "sim_nm") %>%
#'     mutate(v5_resp = ifelse(p_dim == "p4", NA_integer_, v5_resp),
#'            v6_resp = ifelse(p_dim == "p4", NA_integer_, v6_resp))
#' pivot_longer_resp_ans_tbl(resp_ans)

## Pivot variables longer and join (cbind)
pivot_longer_resp_ans_tbl <- function(dat){
  ## Fix col if dat was only from ans_tbl needed
  if(all(c("key", "v1_resp", "v1_marks") %in% colnames(dat)) == FALSE){
    message("dat was only from ans_tbl, fixing columns")
    dat <- dat %>%
      dplyr::mutate(.keep = "all",
                    key = sim_nm,
                    participant_num = 0L,
                    full_perm_num = 0L,
                    bar         = bar,
                    factor      = NA,
                    p_dim       = NA,
                    prolific_id = NA,
                    period      = NA,
                    vc          = NA,
                    location    = NA,
                    eval        = NA,
                    v1_resp     = 0L,
                    v2_resp     = 0L,
                    v3_resp     = 0L,
                    v4_resp     = 0L,
                    v5_resp     = 0L,
                    v6_resp     = 0L,
                    v1_marks    = 0L,
                    v2_marks    = 0L,
                    v3_marks    = 0L,
                    v4_marks    = 0L,
                    v5_marks    = 0L,
                    v6_marks    = 0L,
                    sec_to_resp = 0L,
                    sec_on_pg   = 0L,
                    input_inter = NA,
                    resp_inter  = NA)
  }
  
  ## Pivot each var longer and cbind  back together at end.
  resp_longer <- dat %>%
    dplyr::select(c(key, participant_num, full_perm_num, prolific_id, sim_nm,
                    period, eval, factor, vc, p_dim, location,
                    input_inter, resp_inter, sec_to_resp, sec_on_pg, bar,
                    v1_resp:v6_resp)) %>%
    tidyr::pivot_longer(cols = v1_resp:v6_resp,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "resp",
                        values_drop_na = TRUE)
  
  marks_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_marks:v6_marks)) %>%
    tidyr::pivot_longer(cols = v1_marks:v6_marks,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "marks",
                        values_drop_na = TRUE)
  signal_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_signal:v6_signal)) %>%
    tidyr::pivot_longer(cols = v1_signal:v6_signal,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "signal",
                        values_drop_na = TRUE)
  diff_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_diff:v6_diff)) %>%
    tidyr::pivot_longer(cols = v1_diff:v6_diff,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "diff",
                        values_drop_na = TRUE)
  weight_longer <- dat %>%
    dplyr::select(c(key, sim_nm, v1_weight:v6_weight)) %>%
    tidyr::pivot_longer(cols = v1_weight:v6_weight,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "weight",
                        values_drop_na = TRUE)
  if(all.equal(
    nrow(resp_longer), nrow(marks_longer),
    nrow(signal_longer), nrow(diff_longer), nrow(weight_longer)
  )){
    ## cbind(), left_join not working.
    ret <- cbind(
      resp_longer, marks_longer[, 4L], signal_longer[, 4L],
      diff_longer[, 4L], weight_longer[, 4L]
    )
  }else{error("!!!all nrow() not equal!!!")}
  ret <- ret %>%
    dplyr::mutate(key = as.factor(key),
                  participant_num = factor(participant_num, 
                                           levels = 1L:max(participant_num)),
                  full_perm_num = factor(full_perm_num, 
                                         levels = 1L:max(full_perm_num)),
                  prolific_id = as.factor(prolific_id),
                  sim_nm = as.factor(sim_nm),
                  factor = as.factor(factor),
                  period = as.factor(period),
                  eval = factor(eval, levels = 
                                  c("t1", 1L:2L, "t2", 3L:4L, "t3", 5L:6L)),
                  is_training = ifelse(substr(eval, 1L, 1L) == "t", TRUE, FALSE),
                  vc = factor(vc, c("EEE", "EEV", "banana")),
                  p_dim = factor(as.integer(substr(p_dim, 2L, 2L)),
                                 levels = c(4L, 6L)),
                  location = as.factor(location),
                  var_num = as.factor(substr(var_num, 2L, 2L)),
    ) %>% tibble::as_tibble()
  return(ret)
}

#' @example
#' ans_tbl <- readRDS(file = "./apps/spinifex_study/www/ans_tbl.rds")
#' tgt <- ans_tbl %>% dplyr::filter(sim_nm == "EEV_p6_33_66_rep2")
#' dat_longer <- pivot_longer_resp_ans_tbl(tgt)
#' dat_longer_agg <- aggregate_task_vars(dat_longer)
#' 
## Aggregate to the task grain.
aggregate_task_vars <- function(df_long){
  df_long %>%
    dplyr::group_by(key, participant_num, full_perm_num, prolific_id, sim_nm, 
                    factor, period, eval, is_training, vc, p_dim, location) %>%
    dplyr::summarise(task_input_inter = mean(input_inter),
                     task_resp_inter = mean(resp_inter),
                     max_sec_to_resp = max(sec_to_resp),
                     max_sec_on_pg = max(sec_on_pg),
                     cnt_resp = sum(resp),
                     task_marks = sum(marks),
                     z_weight_check = sum(weight)
    ) %>% dplyr::ungroup() %>%
    tibble::as_tibble()
}



### Originally ggproto_pca_biplot.r -----
require("ggplot2")
#' @example
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea[, 7]
#' ggplot() + theme_void() +
#'   ggproto_pca_biplot(dat, aes_clas = clas)
ggproto_pca_biplot <- function(
  dat,
  x_pc_num = 1L,
  y_pc_num = 2L,
  aes_clas,
  brewer_pal = RColorBrewer::brewer.pal(8L, "Dark2")[c(1L, 2L, 3L, 6L, 8L)],
  text_size = 5L
){
  axes_position <- "left"
  pca_obj <- prcomp(as.matrix(dat))
  proj <- as.data.frame(pca_obj$x[, c(x_pc_num, y_pc_num)])
  bas <- data.frame(pca_obj$rotation[, c(x_pc_num, y_pc_num)])
  bas <- scale_axes(bas, axes_position, proj)
  colnames(proj) <- c("x", "y")
  proj$aes_clas  <- aes_clas
  colnames(bas)  <- c("x", "y")
  
  angle <- seq(0L, 2L * pi, length = 360L)
  circ  <- spinifex::scale_axes(data.frame(x = cos(angle), y = sin(angle)),
                                axes_position, proj)
  zero  <- spinifex::scale_axes(data.frame(x = 0L, y = 0L),
                                axes_position, proj)
  
  asp_r <- 
    diff(range(c(proj[, 2L], circ[, 2L]))) /
    diff(range(c(proj[, 1L], circ[, 1L])))
  point_aes <- ggplot2::aes(x, y) ## Initialize
  if(missing(aes_clas) == FALSE){
    point_aes <- ggplot2::aes(
      x = x, y = y,
      color = aes_clas,
      fill  = aes_clas,
      shape = aes_clas)
  }
  
  
  ### ggproto
  list(
    ggplot2::scale_colour_manual(values = brewer_pal),
    ggplot2::scale_fill_manual(values = brewer_pal),
    ## Themes and aesthetics
    ggplot2::theme(legend.position = "none", ## no legend
                   aspect.ratio = asp_r),
    ## Data points
    ggplot2::geom_point(mapping = point_aes,
                        proj,
                        size = 1),
    ## Circle path
    ggplot2::geom_path(ggplot2::aes(x = x, y = y),
                       circ,
                       color = "grey80", size = 1L, inherit.aes = FALSE),
    ## Axis segments
    ggplot2::geom_segment(ggplot2::aes(x = x, xend = zero[, 1L],
                                       y = y, yend = zero[, 2L]),
                          bas,
                          size = 1L, colour = "grey50"),
    ## Axis label text
    ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = colnames(dat)),
                       bas,
                       size = text_size, colour = "grey50", #fontface = "bold",
                       vjust = "outward", hjust = "outward")
  )
}


### Originally ggproto_ans_plot.r -----
require("ggplot2")

#' @example
#' str(dat)
#' ggplot(dat) +
#'   ggproto_ans_plot(dat) +
#'   facet_wrap(vars(sim_nm)) + theme_minimal()

## ggproto for the ans_plot
ggproto_ans_plot <- function(
  resp_ans_longer,
  brewer_pal = RColorBrewer::brewer.pal(8L, "Dark2")){
  ## List of ggproto objects
  lab_fill <- "Varaiable cluster seperation"
  ret <- list(
    ## Boxplot, signal
    ggplot2::geom_bar(ggplot2::aes(x = var_num, y = signal, fill = lab_fill),
                      resp_ans_longer, position = "dodge", stat = "identity",
                      width = .8),
    
    ## Titles and colors
    ggplot2::labs(x = "Variable number", y = "Value"),
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = "horizontal"),
    ggplot2::scale_fill_manual(
      values = c(brewer_pal[1L], "grey80", "lightblue"), name = "",
      labels = c("Varaiable cluster seperation", "selected", "not selected")),
    ggplot2::scale_colour_manual(values = c("green", "red"),
                                 name = "", labels = c("marks (+)", "marks (-)"))
  )
  
  ## Add in the bar and weight
  p <- 6L #<- length(unique(resp_ans_longer$var_num))
  mark_col <- dplyr::if_else(sign(resp_ans_longer$diff) == 1L, "green", "red")
  ret <- c(ret,
           list(
             ## Marks vertical line segment
             ggplot2::geom_segment(ggplot2::aes(x = var_num, xend = var_num,
                                                y = 0, yend = weight),
                                   resp_ans_longer, colour = mark_col, size = 1.5),
             ## 0 line
             ggplot2::geom_hline(yintercept = 0L, size = 1L),
             ## Uniform bar
             ggplot2::geom_hline(ggplot2::aes(yintercept = bar),
                                 resp_ans_longer,
                                 size = 1L, linetype = 2L),
             ## Uniform bar text
             ggplot2::geom_text(ggplot2::aes(x = p + 1L, y = bar + .07,
                                             label = paste0("1/p = ", round(bar, 2))),
                                resp_ans_longer,
                                size = 4L, hjust = 1L)
           )
  )
  
  return(ret)
}
