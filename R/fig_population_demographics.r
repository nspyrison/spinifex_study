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
  
  ## Save task aggregated data.
  saveRDS(survey_agg, "./apps_supplementary/survey/survey_agg.rds")
}
## Load aggregated data. of the 108 in analysis
survey_agg <- readRDS("./apps_supplementary/survey/survey_agg.rds")
str(survey_agg)
skimr::skim(survey_agg)
