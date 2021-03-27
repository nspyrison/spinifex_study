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
  
  ## !! this is question-grained, probably want pivoted survey obs:
  # survey_agg <- survey_prolific %>%
  #   group_by(survey_num, question, response) %>%
  #   summarise(`No. responses` = n()) %>%
  #   ungroup()
  
  ## Pivot questions wider, 1 row is now a survey.
  survey_wider <- survey_prolific %>% dplyr::select(!c(key, survey_num, sec_to_resp, write_dt, scope)) %>% 
    pivot_wider(names_from = question, values_from = response)
  
  ## Decode question names.
  colnames(survey_wider)[5:22] <-
    c("pronouns", "age", "education", "task_understanding", "data_viz_exp", "analysis_exp", 
      "grand_familar", "grand_ease", "grand_confidence", "grand_like",
      "pca_familar", "pca_ease", "pca_confidence", "pca_like",
      "radial_familar", "radial_ease", "radial_confidence", "radial_like")
  survey_wider <- survey_wider %>%
    mutate(
      participant_num = as.integer(participant_num),
      participant_num = as.integer(full_perm_num),
      pronouns = factor(pronouns, levels = c("he/him", "she/her", "thy/them or other", 
                                             "decline to answer <default, blank, no change>")),
      age = as.factor(age), ## order already correct.
      education = factor(education, levels = 
                           c("Undergraduate degree (BA/BSc/other)", "Graduate degree (MA/MSc/MPhil/other)",
                             "Doctorate degree (PhD/other)", "decline to answer <default, blank, no change>")),
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
  ## Rename the levels of the factors
  survey_wider$pronouns <- plyr::mapvalues(
    survey_wider$pronouns, from = levels(survey_wider$pronouns),
    #table(survey_wider$pronouns)
    to = c("he/him (n=44)", "she/her (n=31)", "they/them or other (n=5)", "decline/default (n=4)"))
  survey_wider$education <- plyr::mapvalues(
    survey_wider$education, from = levels(survey_wider$education),
    #table(survey_wider$education)
    to = c("undergraduate", "graduate", "doctorate", "decline/default"))
  survey_wider$age <- plyr::mapvalues(
    survey_wider$age, from = "decline to answer <default, blank, no change>",
    to = "decline/default")
  
  ## Save task aggregated data.
  saveRDS(survey_wider, "./apps_supplementary/survey/survey_wirder.rds")
}
## Load aggregated data. of the 108 in analysis
survey_wider <- readRDS("./apps_supplementary/survey/survey_wider.rds")
str(survey_wider)
skimr::skim(survey_wider)

## change character to factor, include counts in the levels of sex?
(demographic_heatmaps <- ggplot(survey_wider, aes(education, age)) +
    stat_bin2d(aes(fill = after_stat(count))) +
    facet_grid(cols = vars(pronouns)) + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
    scale_fill_gradient(low = "lightpink", high = "firebrick", na.value = NA) +
    ggtitle("Survey demographics", "faceted on preffered pronouns"))

ggsave(filename = "./paper/figures/figSurveyDemographics.png", plot = demographic_heatmaps,
       width = 8, height = 3.4)
