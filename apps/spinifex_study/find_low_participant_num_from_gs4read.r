
#' @example 
#' raw <- googlesheets4::read_sheet(ss_id, sheet = 1L, range = "B:S")
#' find_low_participant_num_from_gs4read(read_sheet_b.s = raw)
find_low_participant_num_from_gs4read <- function(read_sheet_b.s){
  ## Setup raw
  raw <- read_sheet_b.s %>% filter(!is.na(plot_active)) # Remove dummy rows with application notes
  raw$full_perm_num = unlist(as.integer(raw$full_perm_num))
  raw$prolific_id = unlist(as.character(raw$prolific_id))
  ## Only plot_active rows AND Only prolific_ids (those with exactly nchar == 24)
  sub <- raw %>% filter(plot_active == TRUE,
                        nchar(prolific_id) == 24)
  
  ## Impute missing sec_to_resp AND add is_trainig flag
  .mean_diff <- mean(sub$sec_on_pg, na.rm = TRUE) - mean(sub$sec_to_resp, na.rm = TRUE)
  sub <- sub %>% 
    mutate(
      sec_to_resp = if_else(is.na(sec_to_resp), sec_on_pg - .mean_diff, sec_to_resp),
      is_training = if_else(substr(eval, 1L, 1L) == "t", TRUE, FALSE)
    )
  ## Keep only rows needed to get to qual_cnt_tbl
  sub <- sub %>% select(participant_num, full_perm_num, prolific_id, sec_to_resp, sec_on_pg, is_training)
  ## Filter to only within qual_flag
  q_vals <- quantile(sub$sec_to_resp, probs = probs)
  dat_qual <- sub %>% filter(sec_to_resp > q_vals[1],
                             sec_to_resp < q_vals[2])
  dat_qual_task_agg <- dat_qual %>% group_by(full_perm_num , is_training) %>%
    summarise(n = n(),
              cnt_even_studies = max(sum(is_training) / 3, sum(!is_training) / 6),
              cnt_u_participants = length(unique(participant_num))
    ) %>% ungroup()
  ## Remove training rows
  dat_qual_task_agg <- dat_qual_task_agg %>% filter(is_training == FALSE)
  ## Order by desc
  dat_qual_task_agg <- dat_qual_task_agg[order(dat_qual_task_agg$cnt_even_studies, decreasing = FALSE),]
  ## Return only the lowest full_perm_num
  return(dat_qual_task_agg$full_perm_num[1])
}
