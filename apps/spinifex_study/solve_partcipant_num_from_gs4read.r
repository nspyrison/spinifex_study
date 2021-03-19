
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
                        substr(eval, 1L, 1L) != "t") %>% ## filter out is_training
    ## Create instance_id
    mutate(instance_id = paste(sep = "_", participant_num, full_perm_num, prolific_id))
  
  
  ## The flag is no longer on time, but rather fullness of evaluation...
  ## app_instance_agg
  intance_agg <- sub %>%
    group_by(instance_id) %>%
    summarise(`n instance evals` = n()) %>%
    ungroup() %>%
    mutate(is_instance_even = if_else(`n instance evals` == 6L, TRUE, FALSE))

  ## Find vector of evenly evaled instance_ids
  even_evaled_instance_ids <- intance_agg %>%
    filter(is_instance_even == TRUE) %>%
    pull(instance_id) 
  ## Decode the original dataset by evenness of instance_id.
  
  even_evaled_pnpn <- sub %>% 
    filter(instance_id %in% even_evaled_instance_ids) %>%
    dplyr::select(participant_num, full_perm_num)
  
  ## Find a tgt_perm_num; of the evenly evaled instances aggregate the permutations of each, 
  perm_agg <- even_evaled_pnpn %>%
    group_by(full_perm_num) %>%
    summarise(n_even_evals = n()) %>%
    ungroup()
  ## backfill missing perm numbers in agg table
  missing_perm_nums <- which(!(1L:n_perms %in% unique(perm_agg$full_perm_num)))
  for(i in 1L:length(missing_perm_nums)){
    perm_agg <- rbind(perm_agg, c(missing_perm_nums[i], 0L))
  }
  
  perm_agg <- perm_agg[complete.cases(perm_agg), ]
  ## Order
  perm_agg <- perm_agg %>%
    ## only rows in 1:36, not 0, not NA
    filter(full_perm_num %in% 1L:n_perms) %>%
    arrange(n_even_evals)
  tgt_perm <- perm_agg$full_perm_num[1]
  
  message(paste0("target permutation number: ", tgt_perm))
  used_participant_nums <- unique(sub$participant_num) ## of participants that made it through sub filter, active, not training, with prolifico_ids
  
  candidate <- tgt_perm
  n <- 0L
  while(candidate %in% used_participant_nums){
    n = n + 1L
    candidate <- candidate + n_perms * n
  }
  
  message(paste0("lowest available participant numer for that tgt perm: ", candidate))
  ## Return the candidate participant_num, the first on not used
  return(candidate)
}
