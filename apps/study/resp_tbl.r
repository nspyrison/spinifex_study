require("tibble")
require("dplyr")

## Initialize participant and perm number.
participant_num <- 1
log_file <- "initalize"
if(do_log == TRUE){
  full_perm_num <- 1 + participant_num %% 56
  log_file <- paste0("log_participant_", participant_num, ".json")
  ## TODO: Need to get participant_num, from google docs.
  while (file.exists(log_file)){ ## Find an unused log number
    participant_num <- participant_num + 1
    full_perm_num <- 1 + participant_num %% 56
    log_file <- paste0("log_participant_", participant_num, ".json")
  }
  set_logfile(log_file)
}else{ ## When do_log == F
  participant_num <- sample(1:999, 1)
  full_perm_num <- 1 + participant_num %% 56
  log_file <- paste0("log_participant_", participant_num,
                     "Logging is off! Log and responses not being recorded.")
}
full_perm_num <- 1 + participant_num %% 56
cat("do_log, log_file: ", do_log, log_file, " /n")

#### Select factor and block permutations
## The permutation number
this_factor_perm   <- 1 + (full_perm_num - 1) %% 6 ## %% is mod
this_location_perm <- 1 #1 + floor((full_perm_num - 1) / 3) %% 3
this_vc_perm       <- 1 #1 + floor((full_perm_num - 1) / 9) %% 6
## The permutations
factor_perms   <- rbind(c(1, 1,  2, 2,  3, 3),
                        c(1, 1,  3, 3,  2, 2),
                        c(2, 2,  3, 3,  1, 1),
                        c(2, 2,  1, 1,  3, 3),
                        c(3, 3,  1, 1,  2, 2),
                        c(3, 3,  2, 2,  1, 1))
location_perms <- rbind(c(1, 1,  2, 2,  3, 3),
                        c(1, 1,  3, 3,  2, 2),
                        c(2, 2,  3, 3,  1, 1),
                        c(2, 2,  1, 1,  3, 3),
                        c(3, 3,  1, 1,  2, 2),
                        c(3, 3,  2, 2,  1, 1))
vc_perms       <- rbind(c(1, 1,  2, 2,  3, 3))
## set factor and block names
factor_nms   <- c("pca", "grand", "radial")
location_nms <- c("0_1", "33_66", "50_50")
vc_nms       <- c("EEE", "EEV", "banana")
p_dim_nms <- this_p_dim_nm_ord <- c("p4", "p6")
## The decoded names
this_factor_nm_ord <-
  factor_nms[factor_perms[this_factor_perm, ]]
this_vc_nm_ord <-
  vc_nms[vc_perms[this_vc_perm, ]]
this_location_nm_ord <-
  location_nms[location_perms[this_location_perm, ]]


## Response table ----
make_resp_tbl <- function(participant_num = sample(1:1000, 1)){
  full_perm_num <- 1 + participant_num %% 56
  
  message(paste0("Participant_num: ", participant_num,
                 ", full_perm_num: ", full_perm_num))
  
  resp_tbl <- tibble(
    key = paste(sep = "_", participant_num, full_perm_num, 1:15),
    participant_num = participant_num,
    full_perm_num   = full_perm_num,
    pg         = 1:15,
    section_pg = c(1:3,1:4,1:4,1:3, 1),
    section_nm = c(rep("intro", 3),
                   rep("period1", 4),
                   rep("period2", 4),
                   rep("period3", 3),
                   "survey"),
    period = c(rep(0, 3),
               rep(1, 4),
               rep(2, 4),
               rep(3, 3),
               4),
    plot_active = c(rep(FALSE, 3),
                    rep(TRUE, 3), FALSE,
                    rep(TRUE, 3), FALSE,
                    rep(TRUE, 3), 
                    FALSE),
    eval   = c("study structure", "video", "intermission",
               "t1", 1, 2, "intermission", 
               "t2", 3, 4, "intermission",
               "t3", 5, 6, "study questions"),
    factor = c(rep(NA, 3),
               rep(this_factor_nm_ord[1], 3), NA,
               rep(this_factor_nm_ord[3], 3), NA,
               rep(this_factor_nm_ord[5], 3), NA),
    vc     = c(rep(NA, 3),
               rep(this_vc_nm_ord[1], 3), NA,
               rep(this_vc_nm_ord[3], 3), NA,
               rep(this_vc_nm_ord[5], 3), NA),
    p_dim  = c(rep(NA, 3),
               rep(c("p4", "p4", "p6", NA), 3)),
    location = c(rep(NA, 3),
                 rep(this_location_nm_ord[1], 3), NA,
                 rep(this_location_nm_ord[3], 3), NA,
                 rep(this_location_nm_ord[5], 3), NA),
    sim_nm = case_when(
      is.na(vc) ~ NA_character_, ## wow, have to use NA_char_
      substr(eval, 1, 1) == "t" ~  paste(sep = "_", vc, p_dim, location,
                                 paste0("_t", period)),
      TRUE ~ paste(sep = "_", vc, p_dim, location,
                   paste0("_rep", period))
    ),
    ctrl_inter = NA,
    resp_inter = NA,
    ttr = NA,
    resp = NA,
    ans = NA,
    marks = NA
  )
resp_tbl
}
#' @examples
#' (resp_tbl <- make_resp_tbl())
#' View(resp_tbl)
#' resp_tbl[,1]