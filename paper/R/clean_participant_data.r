### local function for cleaning spinifex study data, as stored on google sheets.
require(dplyr)

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



