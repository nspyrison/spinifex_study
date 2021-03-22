
#' @example 
#' raw <- googlesheets4::read_sheet(ss_id, sheet = 1L, range = "B:S")
#' solve_partcipant_num_from_gs4read(read_sheet_b.s = raw)
solve_partcipant_num_from_gs4read <- function(read_sheet_b.s, n_perms = 36L){
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
                        nchar(stringr::str_trim(prolific_id)) == 24L,
                        substr(eval, 1L, 1L) %in% as.character(1:6)) %>% ## filter out is_training
    ## Create instance_id
    mutate(instance_id = paste(sep = "_", participant_num, full_perm_num, prolific_id))
  
  ## filtering evenness of evaluation
  EVEN_EVALED_instance_ids <- sub %>%
    group_by(instance_id) %>%
    summarise(`n instance evals` = n() / 6L) %>%
    ungroup() %>%
    filter(`n instance evals` == 1L) %>%
    pull(instance_id)
  ## filter the original dataset by evenness of instance_id.
  sub <- sub %>% filter(instance_id %in% EVEN_EVALED_instance_ids)
  
  ## BUT we want to remove over evaled instances when both permuations and participants are over evaled:
  OVER_EVALED_participants <- sub %>%
    group_by(prolific_id) %>%
    summarise(`even evaluations` = n() / 6L) %>%
    ungroup() %>%
    filter(`even evaluations` > 1L) %>%
    pull(prolific_id)
  OVER_EVALED_perm_num <- sub %>%
    group_by(full_perm_num) %>%
    summarise(`even evaluations` = n() / 6L) %>%
    ungroup() %>%
    filter(`even evaluations` > 3L) %>%
    pull(full_perm_num)
  
  ## Remove all instances where the participant AND the permutation number are over evaluated
  intance_agg_over_eval <- sub %>% ## already filtered on even instances
    mutate(`is over evaled participant` = if_else(prolific_id %in% OVER_EVALED_participants, TRUE, FALSE),
           `is over evaled perm num` = if_else(full_perm_num %in% OVER_EVALED_perm_num, TRUE, FALSE),
           ## THESE ARE IDEAL FOR REMOVAL.
           `is over evaled participant and perm` = `is over evaled participant` & `is over evaled perm num`)
  ## Remove both over evaled participants and permutations
  canidate_instances_for_removal <- intance_agg_over_eval %>%
    filter(`is over evaled participant and perm` == TRUE) %>%
    pull(instance_id)
  sub <- sub %>% filter(!(instance_id %in% canidate_instances_for_removal))
  
  ## Aggregate by permutation number and find tgt participant number
  perm_agg <- sub %>%
    group_by(full_perm_num) %>%
    summarise(n_even_evals = n() / 6L) %>%
    ungroup()
  ## backfill missing perm numbers in agg table
  missing_perm_nums <- which(!(1L:n_perms %in% unique(perm_agg$full_perm_num)))
  if(length(missing_perm_nums) > 0){
    for(i in 1L:length(missing_perm_nums)){
      perm_agg <- rbind(perm_agg, c(missing_perm_nums[i], 0L))
    }
  }
  ## remove NA
  perm_agg <- perm_agg[complete.cases(perm_agg), ]
  ## Order
  perm_agg <- perm_agg %>%
    ## only rows in 1:36, not 0, not NA
    filter(full_perm_num %in% 1L:n_perms) %>%
    arrange(n_even_evals)
  tgt_perm <- perm_agg$full_perm_num[1L]
  
  
  message(paste0("target permutation number: ", tgt_perm))
  used_participant_nums <- unique(sub$participant_num)
  browser()
  candidate <- tgt_perm
  n <- 0L
  while(candidate %in% used_participant_nums){
    n <- n + 1L
    candidate <- candidate + n_perms * n
  }
  
  message(paste0("lowest available participant numer for that tgt perm: ", candidate))
  ## Return the candidate participant_num, the first on not used
  return(candidate)
}
