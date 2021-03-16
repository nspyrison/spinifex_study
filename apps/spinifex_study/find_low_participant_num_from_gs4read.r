
#' @example 
#' raw <- googlesheets4::read_sheet(ss_id, sheet = 1L, range = "B:S")
#' find_low_participant_num_from_gs4read(read_sheet_b.s = raw)
find_low_participant_num_from_gs4read <- function(read_sheet_b.s, probs = c(.25, .98)){
  ## Remove dummy rows
  raw <- read_sheet_b.s %>% filter(!is.na(plot_active), ## dummy rows
                                   !is.na(participant_num)) ## 2 missing participant_nums
  ## Format 
  raw$full_perm_num = unlist(as.integer(raw$full_perm_num))
  raw$prolific_id = unlist(as.character(raw$prolific_id))
  
  ### Filter:
  ## -Only plot_active rows
  ## -Only prolific_ids (those with exactly nchar == 24)
  ## -Only non-training rows
  sub <- raw %>% filter(plot_active == TRUE,
                        nchar(stringr::str_trim(participant_num)) == 24L,
                        is_training == FALSE) %>%
    ## Create instance_id
    mutate(instance_id = paste(sep = "_", participant_num, full_perm_num, prolific_id))
  
  ## The flag is no longer on time, but rather fullness of evaluation...
  ## app_instance_agg
  dat_intance_agg <- dat_task_agg %>% filter(`is training` == FALSE) %>%
    group_by(instance_id, participant_num, `parameter permutation`, participant) %>%
    summarise(`n instance evals` = n()) %>% 
    ungroup() %>% 
    mutate(is_instance_even = if_else(`n instance evals` == 6, TRUE, FALSE)) %>% 
    arrange(desc(`n instance evals`))
  ## Find vector of evenly evaled instance_ids
  even_evaled_instance_ids <- dat_intance_agg %>%
    filter(is_instance_even == TRUE) %>%
    pull(instance_id)
  ## Decode the original dataset by evenness of instance_id.
  dat_qual_full <- dat_task_agg %>% mutate(
    `is even instance` = if_else(instance_id %in% even_evaled_instance_ids, TRUE, FALSE))
  dat_qual <- dat_qual_full %>% filter(`is training` == FALSE)
  
  
  
  
  
  
  
  dat_qual_task_agg <- dat_qual %>% group_by(full_perm_num , is_training) %>%
    summarise(n = n(),
              cnt_even_studies = max(sum(is_training) / 3L, sum(!is_training) / 6L),
              cnt_u_participants = length(unique(participant_num))
    ) %>% ungroup()
  ## Remove training rows
  
  
  ## Order by increasing count of study evaluations
  dat_qual_task_agg <- dat_qual_task_agg[order(dat_qual_task_agg$cnt_even_studies, decreasing = FALSE),]
  ## Return the first, lowest evaluated full_perm_num.
  return(dat_qual_task_agg$full_perm_num[1L])
}
