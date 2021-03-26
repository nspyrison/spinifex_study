## Follow the loose setup of _analysis.rmd:
if(F)
  file.edit("./apps_supplementary/v4_prolifico_100/_analysis.rmd")

## Read from gsheets API4 and save local
if(F){
  ## Hash id of the google sheet
  ss_id <- "1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU" 
  raw <- googlesheets4::read_sheet(ss_id, sheet = 2L) ## the survey sheet
  dim(raw)
  saveRDS(raw, "./apps_supplementary/survey/raw.rds")
}

## Load load and clean, save cleaned
if(F){
  raw <- readRDS("./apps_supplementary/survey/raw.rds")
  
  ## Only Prolific participants that were in the 108 instance_ids in the analysis
  survey_prolific <- raw %>%
    mutate(instance_id = 
             paste(participant_num, full_perm_num, prolific_id, sep = "_")) %>%
    filter(nchar(stringr::str_trim(prolific_id)) == 24,
           instance_id %in% instance_id_whitelist)
  
  # ##### preview
  # hist(survey_prolific$sec_to_resp)
  # survey_prolific %>% filter(survey_num == 1) %>% pull(response) %>% table()
  # survey_prolific %>% filter(survey_num == 2) %>% pull(response) %>% table()
  # survey_prolific %>% filter(survey_num == 3) %>% pull(response) %>% table()
  # message("Need to convert to aggreegated counts of each question level")
  # #####
  
  survey_agg <- survey_prolific %>%
    group_by(survey_num, question, response) %>%
    summarise(`No. responses` = n()) %>%
    ungroup()
  
  survey_wider <- survey_prolific %>% dplyr::select(!c(key, survey_num, sec_to_resp, write_dt, scope)) %>% 
    pivot_wider(names_from = question, values_from = response)
  str(survey_wider)
  colnames(survey_wider)[5:22] <-
    c("pronouns", "age", "education", "task_understanding", "data_viz_exp", "analysis_exp", 
      "grand_familar", "grand_ease", "grand_confidence", "grand_like",
      "pca_familar", "pca_ease", "pca_confidence", "pca_like",
      "radial_familar", "radial_ease", "radial_confidence", "radial_like")
  survey_wider <- survey_wider %>% 
    mutate(participant_num = as.integer(participant_num),
           participant_num = as.integer(full_perm_num),
           task_understanding = as.integer(substr(task_understanding, 1, 1)),
           data_viz_exp = as.integer(substr(data_viz_exp, 1, 1)),
           analysis_exp = as.integer(substr(analysis_exp, 1, 1)),
           grand_familar = as.integer(substr(grand_familar, 1, 1)),
           grand_ease = as.integer(substr(grand_ease, 1, 1)),
           grand_confidence = as.integer(substr(grand_confidence, 1, 1)),
           grand_like = as.integer(substr(grand_like, 1, 1)),
           pca_familar = as.integer(substr(pca_familar, 1, 1)),
           pca_ease = as.integer(substr(pca_ease, 1, 1)),
           pca_confidence = as.integer(substr(pca_confidence, 1, 1)),
           pca_like = as.integer(substr(pca_like, 1, 1)),
           radial_familar = as.integer(substr(radial_familar, 1, 1)),
           radial_ease = as.integer(substr(radial_ease, 1, 1)),
           radial_confidence = as.integer(substr(radial_confidence, 1, 1)),
           radial_like = as.integer(substr(radial_like, 1, 1))
    )
  
  ## Save task aggregated data.
  saveRDS(survey_agg, "./apps_supplementary/survey/survey_agg.rds")
}
## Load aggregated data. of the 108 in analysis
survey_agg <- readRDS("./apps_supplementary/survey/survey_agg.rds")
str(survey_agg)
skimr::skim(survey_agg)

## change character to factor, include counts in the levels of sex?
demographic_heatmaps <- ggplot(survey_wider, aes(education, age)) + 
  stat_bin2d(aes(fill = after_stat(count))) + 
  facet_wrap(vars(pronouns)) + theme_minimal()
