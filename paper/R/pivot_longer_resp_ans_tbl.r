#' @example
#' ans_tbl <- readRDS(file = "./apps/spinifex_study/www/ans_tbl.rds")
#' tgt <- ans_tbl %>% dplyr::filter(sim_nm == "EEV_p6_33_66_rep2")
#' df_longer <- pivot_longer_resp_ans_tbl(tgt)
#' 
#' 
#' ## Raw data
#' raw <- readRDS("./raw_nick_pilot.rds")
#' ## Filter to only task data
#' df_active <- raw[raw$plot_active == TRUE, ]
#' ## Read and join ans_tbl, clean up 0 to NA on v5/v6.
#' ans_tbl   <- readRDS(file = "../../apps/spinifex_study/www/ans_tbl.rds")
#' resp_ans  <- dplyr::left_join(x = df_active, y =  ans_tbl, by = "sim_nm") %>%
#'     mutate(v5_resp = ifelse(p_dim == "p4", NA_integer_, v5_resp),
#'            v6_resp = ifelse(p_dim == "p4", NA_integer_, v6_resp))
#' pivot_longer_resp_ans_tbl(resp_ans)
#'
#'
## Pivot variables longer and join (cbind)
pivot_longer_resp_ans_tbl <- function(df){
  ## Fix col if df was only from ans_tbl needed
  if(all(c("key", "v1_resp", "v1_marks") %in% colnames(df)) == FALSE){
    message("df was only from ans_tbl, fixing columns")
    df <- df %>%  dplyr::mutate(.keep = "all",
                                key = sim_nm,
                                v1_resp = 0,
                                v2_resp = 0,
                                v3_resp = 0,
                                v4_resp = 0,
                                v5_resp = 0,
                                v6_resp = 0,
                                v1_marks = 0,
                                v2_marks = 0,
                                v3_marks = 0,
                                v4_marks = 0,
                                v5_marks = 0,
                                v6_marks = 0)
  }
  
  
  ## Pivot each var longer and cbind  back together at end.
  resp_longer <- df %>%
    dplyr::select(c(key, sim_nm, bar, v1_resp:v6_resp)) %>%
    tidyr::pivot_longer(cols = v1_resp:v6_resp,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "resp",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(var_num = as.factor(substr(var_num, 2, 2)))
  marks_longer <- df %>% 
    dplyr::select(c(key:v6_marks)) %>%
    tidyr::pivot_longer(cols = v1_marks:v6_marks,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "marks",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(var_num = as.factor(substr(var_num, 2, 2)))
  signal_longer <- df %>% 
    dplyr::select(c(key, sim_nm, v1_signal:v6_signal)) %>%
    tidyr::pivot_longer(cols = v1_signal:v6_signal,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "signal",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(var_num = as.factor(substr(var_num, 2, 2)))
  diff_longer <- df %>% 
    dplyr::select(c(key, sim_nm, v1_diff:v6_diff)) %>%
    tidyr::pivot_longer(cols = v1_diff:v6_diff,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "diff",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(var_num = as.factor(substr(var_num, 2, 2)))
  weight_longer <- df %>% 
    dplyr::select(c(key, sim_nm, v1_weight:v6_weight)) %>%
    tidyr::pivot_longer(cols = v1_weight:v6_weight,
                        names_to = "var_num",
                        names_prefix  = "var_num",
                        values_to = "weight",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(var_num = as.factor(substr(var_num, 2, 2)))
  dat <- NA
  if(all.equal(
    nrow(resp_longer), nrow(marks_longer),
    nrow(signal_longer), nrow(diff_longer), nrow(weight_longer)
  )){
    ## cbind(), left_join not working.
    dat <- cbind(
      resp_longer, marks_longer[, 4], signal_longer[, 4],
      diff_longer[, 4], weight_longer[, 4])
  }else{error("!!!all nrow() not equal!!!")}
  
  ret <- dat %>% dplyr::group_by(sim_nm, var_num) %>%
    dplyr::mutate(task_signal = sum(signal)) %>%
    dplyr::ungroup()
  return(ret)
}