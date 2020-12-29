require("tibble")
require("dplyr")


### Stand alone example, typically initialized in global.r:
#' @examples
#' #### IF RUN OUTSIDE OF APP ####
#' ## needs participant#, permutation#, and experimental design.
#' ## Initialize participant and perm number.
#' participant_num <- 1
#' log_file <- "initalize"
#' if(do_log == TRUE){
#'   full_perm_num <- 1 + participant_num %% 56
#'   log_file <- paste0("log_participant_", participant_num, ".json")
#'   ## TODO: Need to get participant_num, from google docs.
#'   while (file.exists(log_file)){ ## Find an unused log number
#'     participant_num <- participant_num + 1
#'     full_perm_num <- 1 + participant_num %% 56
#'     log_file <- paste0("log_participant_", participant_num, ".json")
#'   }
#'   set_logfile(log_file)
#' }else{ ## When do_log == F
#'   participant_num <- sample(1:999, 1)
#'   full_perm_num <- 1 + participant_num %% 56
#'   log_file <- paste0("log_participant_", participant_num,
#'                      "Logging is off! Log and responses not being recorded.")
#' }
#' full_perm_num <- 1 + participant_num %% 56
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


make_resp_tbl <- function(participant_num = sample(1L:1000L, 1L)){
  full_perm_num <- 1 + participant_num %% 56L
  
  message(paste0("Participant_num: ", participant_num,
                 ", full_perm_num: ", full_perm_num))
  
  resp_tbl <- tibble::tibble(
    key = paste(sep = "_", participant_num, full_perm_num, 1L:15L),
    participant_num = as.integer(participant_num),
    full_perm_num   = as.integer(full_perm_num),
    prolific_id = NA_character_,
    pg         = as.integer(1L:15L),
    section_pg = as.integer(c(1L:3L, 1L:4L, 1L:4L, 1L:3L, 1L)),
    section_nm = c(rep("intro", 3L),
                   rep("period1", 4L),
                   rep("period2", 4L),
                   rep("period3", 3L),
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
    eval   = c("study structure", "video", "intermission",
               "t1", 1L, 2L, "intermission", 
               "t2", 3L, 4L, "intermission",
               "t3", 5L, 6L, "study questions"),
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
    ttr         = NA_integer_,
    marks       = NA_real_,
    write_dt    = NA_character_,
    v1_resp     = NA_integer_,
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
  return(resp_tbl)
}
#' @examples
#' (resp_tbl <- make_resp_tbl(participant_num))
#' View(resp_tbl)




make_survey_tbl <- function(participant_num = sample(1L:1000L, 1L)){
  full_perm_num <- 1L + participant_num %% 56L
  
  message(paste0("Participant_num: ", participant_num,
                 ", full_perm_num: ", full_perm_num))
  
  survey_tbl <- tibble::tibble(
    key = paste(sep = "_", participant_num, full_perm_num, 1L:18L),
    participant_num = as.integer(participant_num),
    full_perm_num   = as.integer(full_perm_num),
    prolific_id     = NA_character_,
    survey_num      = 1L:18L,
    scope           = c(rep("demographic", 6L), 
                        rep(this_factor_nm_ord[1L], 4L),
                        rep(this_factor_nm_ord[3L], 4L),
                        rep(this_factor_nm_ord[5L], 4L) ),
    question        = NA_character_,
    response        = NA_character_,
    seconds_on_page = NA_integer_,
    write_dt        = NA_character_,
  )
  return(survey_tbl)
}

#' @examples
#' (survey_tbl <- make_survey_tbl(participant_num))
#' View(survey_tbl)