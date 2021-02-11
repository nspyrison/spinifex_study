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
#'
#'
## Pivot variables longer and join (cbind)
pivot_longer_resp_ans_tbl <- function(dat){
  ## Fix col if dat was only from ans_tbl needed
  if(all(c("key", "v1_resp", "v1_marks") %in% colnames(dat)) == FALSE){
    message("dat was only from ans_tbl, fixing columns")
    dat <- dat %>%  dplyr::mutate(.keep = "all",
                                key = sim_nm,
                                v1_resp = 0L,
                                v2_resp = 0L,
                                v3_resp = 0L,
                                v4_resp = 0L,
                                v5_resp = 0L,
                                v6_resp = 0L,
                                v1_marks = 0L,
                                v2_marks = 0L,
                                v3_marks = 0L,
                                v4_marks = 0L,
                                v5_marks = 0L,
                                v6_marks = 0L)
  }
  
  ## Pivot each var longer and cbind  back together at end.
  resp_longer <- dat %>%
    dplyr::select(c(key, prolific_id, sim_nm,
                    period, eval, factor, vc, p_dim, location,
                    input_inter, resp_inter, sec_on_pg,
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
                  prolific_id = as.factor(prolific_id),
                  sim_nm = as.factor(sim_nm),
                  factor = as.factor(factor),
                  period = as.factor(period),
                  eval = factor(eval, levels = c("t1", 1:2, "t2", 3:4, "t3", 5:6)),
                  is_training = ifelse(substr(eval, 1, 1) == "t", TRUE, FALSE),
                  vc = factor(vc, c("EEE", "EEV", "banana")),
                  p_dim = factor(substr(p_dim, 2L, 2L), levels = c("4", "6")),
                  location = as.factor(location),
                  var_num = as.factor(substr(var_num, 2L, 2L)),
                  .keep = "unused"
    ) %>% tibble::as_tibble()
  return(ret)
}


## Aggregate to the task grain.
aggregate_task_vars <- function(df_long){
  df_long %>% 
    dplyr::group_by(key, prolific_id, sim_nm, factor, period, eval, is_training, vc, p_dim, location) %>%
    dplyr::summarise(task_input_inter = mean(input_inter),
                     task_resp_inter = mean(resp_inter),
                     max_sec_on_pg = max(sec_on_pg),
                     cnt_resp = sum(resp),
                     task_marks = sum(marks),
                     z_weight_check = sum(weight)
    ) %>% dplyr::ungroup() %>% 
    tibble::as_tibble()
}
