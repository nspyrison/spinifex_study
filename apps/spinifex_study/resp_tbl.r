require("tibble")
require("dplyr")


### Stand alone example, typically initialized in global.r:
#' @examples
#' #### IF RUN OUTSIDE OF APP, NEED THIS INITIALIZATION ####
#' ## needs participant#, permutation#, and experimental design.
#' ## Initialize participant and perm number.
#' participant_num <- 1
#' log_file <- "initialize"
#' if(do_log == TRUE){
#'   full_perm_num <- 1 + participant_num %% 36
#'   log_file <- paste0("log_participant_", participant_num, ".json")
#'   ## TODO: Need to get participant_num, from google docs.
#'   while (file.exists(log_file)){ ## Find an unused log number
#'     participant_num <- participant_num + 1
#'     full_perm_num <- 1 + participant_num %% 36
#'     log_file <- paste0("log_participant_", participant_num, ".json")
#'   }
#'   set_logfile(log_file)
#' }else{ ## When do_log == F
#'   participant_num <- sample(1:999, 1)
#'   full_perm_num <- 1 + participant_num %% 36
#'   log_file <- paste0("log_participant_", participant_num,
#'                      "Logging is off! Log and responses not being recorded.")
#' }
#' full_perm_num <- 1 + participant_num %% 36
#' cat("do_log, log_file: ", do_log, log_file, " /n")
#' 
#' #### Select factor and block permutations
#' ## The permutation number
#' this_factor_perm   <- 1 + (full_perm_num - 1) %% 6 ## %% is mod
#' this_location_perm <- 1 #1 + floor((full_perm_num - 1) / 3) %% 3
#' this_vc_perm       <- 1 #1 + floor((full_perm_num - 1) / 9) %% 6
#' ## The permutations
#' factor_perms   <- rbind(c(1, 1,  2, 2,  3, 3),
#'                         c(1, 1,  3, 3,  2, 2),
#'                         c(2, 2,  3, 3,  1, 1),
#'                         c(2, 2,  1, 1,  3, 3),
#'                         c(3, 3,  1, 1,  2, 2),
#'                         c(3, 3,  2, 2,  1, 1))
#' location_perms <- rbind(c(1, 1,  2, 2,  3, 3),
#'                         c(1, 1,  3, 3,  2, 2),
#'                         c(2, 2,  3, 3,  1, 1),
#'                         c(2, 2,  1, 1,  3, 3),
#'                         c(3, 3,  1, 1,  2, 2),
#'                         c(3, 3,  2, 2,  1, 1))
#' vc_perms       <- rbind(c(1, 1,  2, 2,  3, 3))
#' ## set factor and block names
#' factor_nms   <- c("pca", "grand", "radial")
#' location_nms <- c("0_1", "33_66", "50_50")
#' vc_nms       <- c("EEE", "EEV", "banana")
#' p_dim_nms <- this_p_dim_nm_ord <- c("p4", "p6")
#' ## The decoded names
#' this_factor_nm_ord <-
#'   factor_nms[factor_perms[this_factor_perm, ]]
#' this_vc_nm_ord <-
#'   vc_nms[vc_perms[this_vc_perm, ]]
#' this_location_nm_ord <-
#'   location_nms[location_perms[this_location_perm, ]]
#' survey_questions <- c("What are your preferred pronouns?",
#'   "Which age group do you belong to?",
#'   "What is your highest completed education?",
#'   "I understand the how to perform the task.",
#'   "I am experienced with data visualization",
#'   "I am experienced with data analysis.",
#'   rep(c("I was already familiar with this method.",
#'         "I found this visualization easy to use.",
#'         "I felt confident in my answers with this method.",
#'         "I liked using this method."), 3L)
#' )
make_resp_tbl <- function(participant_num = sample(1L:n_perms, 1L),
                          n_perms = 36L){
  full_perm_num <- 1L + (participant_num - 1L) %% n_perms
  resp_tbl <- tibble::tibble(
    key = paste(sep = "_", participant_num, full_perm_num, 1L:15L),
    participant_num = as.integer(participant_num),
    full_perm_num   = as.integer(full_perm_num),
    prolific_id = NA_character_,
    pg         = as.integer(1L:15L),
    section_nm = c(rep("intro", 2L),
                   rep("period1", 4L),
                   rep("period2", 4L),
                   rep("period3", 4L),
                   "survey"),
    period = as.integer(c(rep(0L, 3L),
                          rep(1L, 4L),
                          rep(2L, 4L),
                          rep(3L, 3L),
                          4L)),
    plot_active = c(rep(FALSE, 3L),
                    rep(TRUE,  3L), FALSE,
                    rep(TRUE,  3L), FALSE,
                    rep(TRUE,  3L),
                    FALSE),
    eval   = c("study structure", "video",
               "intermission1", "t1", 1L, 2L,
               "intermission2", "t2", 3L, 4L,
               "intermission3", "t3", 5L, 6L,
               "study questions"),
    factor = c(rep(NA, 3L),
               rep(this_factor_nm_ord[1L], 3L), NA,
               rep(this_factor_nm_ord[3L], 3L), NA,
               rep(this_factor_nm_ord[5L], 3L), NA),
    vc     = c(rep(NA, 3L),
               "EEE", rep(this_vc_nm_ord[1L], 2L), NA,
               "EEE", rep(this_vc_nm_ord[3L], 2L), NA,
               "EEE", rep(this_vc_nm_ord[5L], 2L), NA),
    p_dim  = c(rep(NA, 3L),
               rep(c("p4", "p4", "p6", NA), 3L)),
    location = c(rep(NA, 3L),
                 "0_1", rep(this_location_nm_ord[1L], 2L), NA,
                 "0_1", rep(this_location_nm_ord[3L], 2L), NA,
                 "0_1", rep(this_location_nm_ord[5L], 2L), NA),
    sim_nm = case_when(
      is.na(vc) ~ NA_character_, ## wow, have to use NA_char_
      substr(eval, 1L, 1L) == "t" ~ paste(sep = "_", vc, p_dim, location,
                                        paste0("t", period)),
      TRUE ~ paste(sep = "_", vc, p_dim, location,
                   paste0("rep", period))
    ),
    input_inter = NA_integer_,
    resp_inter  = NA_integer_,
    sec_to_resp = NA_integer_,
    task_marks  = NA_real_,
    sec_on_pg   = NA_integer_,
    write_dt    = NA_character_,
    v1_resp     = NA_integer_, ## input as logical, stored as integer.
    v2_resp     = NA_integer_,
    v3_resp     = NA_integer_,
    v4_resp     = NA_integer_,
    v5_resp     = NA_integer_,
    v6_resp     = NA_integer_,
    v1_marks    = NA_real_,
    v2_marks    = NA_real_,
    v3_marks    = NA_real_,
    v4_marks    = NA_real_,
    v5_marks    = NA_real_,
    v6_marks    = NA_real_,
  )
  message(paste0("Made resp_tbl."))
  return(resp_tbl)
}
#' @examples
#' (resp_tbl <- make_resp_tbl(participant_num))
#' View(resp_tbl)


make_survey_tbl <- function(participant_num = sample(1L:n_perms, 1L),
                            n_perms = 36L){
  full_perm_num <- 1L + (participant_num - 1L) %% n_perms
  
  survey_tbl <- tibble::tibble(
    key = paste(sep = "_", participant_num, full_perm_num, 1L:18L),
    participant_num = as.integer(participant_num),
    full_perm_num   = as.integer(full_perm_num),
    prolific_id     = "<default, blank>",
    survey_num      = 1L:18L,
    scope           = c(rep("demographic", 6L), 
                        rep(this_factor_nm_ord[1L], 4L),
                        rep(this_factor_nm_ord[3L], 4L),
                        rep(this_factor_nm_ord[5L], 4L)),
    question        = paste0(scope, ": ", survey_questions),
    response        = c(rep("decline to answer <default, blank, no change>", 3L),
                        rep("3 <default, no change>", 15L)),
    sec_to_resp     = NA_integer_,
    write_dt        = NA_character_,
  )
  message(paste0("Made survey_tbl."))
  return(survey_tbl)
}
#' @examples
#' (survey_tbl <- make_survey_tbl(participant_num))
#' View(survey_tbl)

#### Creates the ans_tbl relative to the project dir, run outside of app. 
## NOTE:  MANUALLY RUN ONCE, NOT IN APP. Apply with load_join_ans_tbl() inside app.
make_save_ans_tbl <- function(){
  tictoc::tic("make_save_ans_tbl()")
  ## Vector of sim_nms
  sim_nms <- c("EEE_p4_0_1",    "EEE_p4_33_66",    "EEE_p4_50_50",
               "EEV_p4_0_1",    "EEV_p4_33_66",    "EEV_p4_50_50",
               "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
               "EEE_p6_0_1",    "EEE_p6_33_66",    "EEE_p6_50_50",
               "EEV_p6_0_1",    "EEV_p6_33_66",    "EEV_p6_50_50",
               "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
  sim_nms <- c(paste0("EEE_p4_0_1_t", 1L:3L), ## 3 training sets
               as.vector(outer(sim_nms, paste0("_rep", 1L:3L), FUN = "paste0"))) ## Cross product paste
  root <- "./apps_supplementary/data" ## Can't use here::here(); File paths cannot be too long...
  sim_fps <- paste0(root, "/", sim_nms, ".rda")
  
  ## Initialize before looping
  n_sims <- length(sim_nms)
  var_signal_mat <- var_diff_mat <- var_weight_mat <-
    matrix(NA, nrow = n_sims, ncol = 6L)
  bar_vect <- rep(NA, n_sims)
  ## Load and extracting measures, loop
  for(i in 1L:n_sims){
    ## Load sims, with attached attributes, including signal
    load(sim_fps[i]) ## private, not in global env
    sim_signal <- attr(get(sim_nms[i]), "var_mean_diff_ab")
    ## Normalize to fraction of total signal.
    sim_signal <- sim_signal / sum(sim_signal)
    var_ind <- 1L:length(sim_signal) ## For sims of 4 dim
    
    ## var_signal, vector, the difference of the means of clusters A & B in given dim.
    var_signal_mat[i, var_ind] <- sim_signal
    ## bar, scalar, the average size of the signal per dim.
    ##TODO VALIDATE BAR, / p? / (p-1)? / (p_noise)? z_check_weights all negative.
    bar_vect[i] <- 
      sum(var_signal_mat[i, var_ind]) / length(sim_signal)
    ## var_diff, vector, magnitude above bar of the signal in each dim
    var_diff_mat[i, var_ind] <- var_signal_mat[i, var_ind] - bar_vect[i]
    ## var_weight, vector, the weight assigned to each var if selected in response
    var_weight_mat[i, var_ind] <-
      sign(var_diff_mat[i, var_ind]) * sqrt(abs(var_diff_mat[i, var_ind]))
  }
  ## Make ans_tbl
  ans_tbl <- tibble::tibble(
    data.frame(
      sim_nms,
      bar_vect,
      var_signal_mat,
      var_diff_mat,
      var_weight_mat
    )
  )
  ## Rename columns
  colnames(ans_tbl) <- c("sim_nm", "bar",
                         paste0("v", 1L:6L, "_signal"),
                         paste0("v", 1L:6L, "_diff"),
                         paste0("v", 1L:6L, "_weight")
  )
  
  ## Save ans_tbl and wrap up
  path <- "./apps/spinifex_study/www/ans_tbl.rds"
  saveRDS(object = ans_tbl, file = path)
  message(paste0("Successfully saved ans_tbl to ", path))
  tictoc::toc()
  return(NULL)
}
#' @example 
#' make_save_ans_tbl()


#### Read ans_tbl obj made in make_save_ans_tbl()
## This is called within app.
read_join_ans_tbl <- function(resp_tbl){
  if(missing(resp_tbl)) stop("resp_tbl not passed to read_join_ans_tbl.")

  ## Read
  ans_tbl <- readRDS(file = "./www/ans_tbl.rds") ## Note runs in app, fp relative to "apps/study".
  ## If run from project, outside app, wants:
  #' @example 
  #' ans_tbl <- readRDS(file = "./apps/spinifex_study/www/ans_tbl.rds")
  
  ## Left join, base merge() changes rows, col order, and coerces to data.frame.
  resp_ans_tbl <- dplyr::left_join(x = resp_tbl, y =  ans_tbl, by = "sim_nm")
  
  message(paste0("Joined ans_tbl to resp_tbl."))
  return(resp_ans_tbl)
}