require(tidyverse)
## Follow the loose setup of _analysis.rmd:
if(F)
  file.edit("./apps_supplementary/v4_prolifico_100/_analysis.rmd")

#### MANUAL READ -----
## Read from gsheets API4 and save local
if(F){
  ## Hash id of the google sheet
  ss_id <- "1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU"
  raw <- googlesheets4::read_sheet(ss_id, sheet = 2L) ## the survey sheet
  dim(raw)
  saveRDS(raw, "./apps_supplementary/survey/raw.rds")
}
##### Manual Load and format, pivot, format, save -----
## Load and clean, save cleaned
if(F){
  raw <- readRDS("./apps_supplementary/survey/raw.rds")
  instance_id_whitelist <- readRDS("./apps_supplementary/v4_prolifico_100/instance_id_whitelist.rds")
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
  # ##### end preview
  
  ## !! this is question-grained, probably want pivoted survey obs:
  # survey_agg <- survey_prolific %>%
  #   group_by(survey_num, question, response) %>%
  #   summarise(`No. responses` = n()) %>%
  #   ungroup()
  
  ###### pivot wider, 1 row is 1 survey -----
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
      participant_num = factor(participant_num),
      full_perm_num = factor(full_perm_num),
      prolific_id = factor(prolific_id),
      instance_id = factor(instance_id),
      pronouns = factor(pronouns, levels = c("he/him", "she/her", "thy/them or other", 
                                             "decline to answer <default, blank, no change>")),
      age = as.factor(age), ## order already correct.
      education = factor(education, levels = 
                           c("Undergraduate degree (BA/BSc/other)", "Graduate degree (MA/MSc/MPhil/other)",
                             "Doctorate degree (PhD/other)", "decline to answer <default, blank, no change>")),
      task_understanding = factor(substr(task_understanding, 1, 1),),
      data_viz_exp = factor(substr(data_viz_exp, 1, 1),),
      analysis_exp = factor(substr(analysis_exp, 1, 1),),
      grand_familar = factor(substr(grand_familar, 1, 1),),
      grand_ease = factor(substr(grand_ease, 1, 1),),
      grand_confidence = factor(substr(grand_confidence, 1, 1),),
      grand_like = factor(substr(grand_like, 1, 1),),
      pca_familar = factor(substr(pca_familar, 1, 1),),
      pca_ease = factor(substr(pca_ease, 1, 1),),
      pca_confidence = factor(substr(pca_confidence, 1, 1),),
      pca_like = factor(substr(pca_like, 1, 1),),
      radial_familar = factor(substr(radial_familar, 1, 1),),
      radial_ease = factor(substr(radial_ease, 1, 1),),
      radial_confidence = factor(substr(radial_confidence, 1, 1),),
      radial_like = factor(substr(radial_like, 1, 1))
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
  .l_lvls <- c("most negative", "negative", "neutral", "positive", "most positive")
  survey_wider$task_understanding <- plyr::mapvalues(survey_wider$task_understanding,
                                                    from = 1:5, to = .l_lvls)
  survey_wider$data_viz_exp <- plyr::mapvalues(survey_wider$data_viz_exp,
                                               from = 1:5, to = .l_lvls)
  survey_wider$analysis_exp <- plyr::mapvalues(survey_wider$analysis_exp,
                                               from = 1:5, to = .l_lvls)
  survey_wider$grand_familar <- plyr::mapvalues(survey_wider$grand_familar,
                                                from = 1:5, to = .l_lvls)
  survey_wider$grand_ease <- plyr::mapvalues(survey_wider$grand_ease ,
                                             from = 1:5, to = .l_lvls)
  survey_wider$grand_confidence <- plyr::mapvalues(survey_wider$grand_confidence,
                                                   from = 1:5, to = .l_lvls)
  survey_wider$grand_like <- plyr::mapvalues(survey_wider$grand_like,
                                             from = 1:5, to = .l_lvls)
  survey_wider$pca_familar <- plyr::mapvalues(survey_wider$pca_familar,
                                              from = 1:5, to = .l_lvls)
  survey_wider$pca_ease <- plyr::mapvalues(survey_wider$pca_ease,
                                           from = 1:5, to = .l_lvls)
  survey_wider$pca_confidence <- plyr::mapvalues(survey_wider$pca_confidence,
                                                 from = 1:5, to = .l_lvls)
  survey_wider$pca_like <- plyr::mapvalues(survey_wider$pca_like,
                                           from = 1:5, to = .l_lvls)
  survey_wider$radial_familar <- plyr::mapvalues(survey_wider$radial_familar,
                                                 from = 1:5, to = .l_lvls)
  survey_wider$radial_ease <- plyr::mapvalues(survey_wider$radial_ease,
                                              from = 1:5, to = .l_lvls)
  survey_wider$radial_confidence <- plyr::mapvalues(survey_wider$radial_confidence,
                                                    from = 1:5, to = .l_lvls)
  survey_wider$radial_like <- plyr::mapvalues(survey_wider$radial_like,
                                              from = 1:5, to = .l_lvls)
  
  ## Save task aggregated data.
  saveRDS(survey_wider, "./apps_supplementary/survey/survey_wider.rds")
}

#### Load and plot as demographic heat map ----
## Load aggregated data. of the 108 in analysis
survey_wider <- readRDS("./apps_supplementary/survey/survey_wider.rds")
str(survey_wider)
skimr::skim(survey_wider)

survey_wider

## change character to factor, include counts in the levels of sex?
(demographic_heatmaps <- ggplot(survey_wider, aes(education, age)) +
    stat_bin2d(aes(fill = after_stat(count))) +
    geom_text(aes(label = after_stat(count)), stat = "bin2d") +
    facet_grid(cols = vars(pronouns)) + theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          # legend.position = "bottom", 
          # legend.direction = "horizontal",
          legend.margin = margin(0,0,0,0))+ 
    scale_fill_gradient(low = "lightpink", high = "firebrick", na.value = NA) +
    ggtitle("Participant demographics"))
ggsave(filename = "./paper/figures/figSurveyDemographics.png",
       plot = demographic_heatmaps, width = 8, height = 3.4)


# ## Subjective measures of factor -----
# survey_wider <- readRDS("./apps_supplementary/survey/survey_wider.rds")
# str(survey_wider)
# skimr::skim(survey_wider)
# 
# ## pivot_longer within factor
# radial_longer <- survey_wider %>%
#   select(instance_id, radial_familar:radial_like) %>%
#   pivot_longer(radial_familar:radial_like, 
#                names_to = "factor", values_to = "value")
# grand_longer <- survey_wider %>%
#   select(instance_id, grand_familar:grand_like) %>%
#   pivot_longer(grand_familar:grand_like, 
#                names_to = "factor", values_to = "value")
# pca_longer <- survey_wider %>%
#   select(instance_id, pca_familar:pca_like) %>%
#   pivot_longer(pca_familar:pca_like, 
#                names_to = "factor", values_to = "value")
# ## Combine and split measure from factor
# subjective_longer <- rbind(radial_longer, grand_longer, pca_longer) %>%
#   separate(factor, c("factor", "measure"), sep = "_")
# 
# 
# ## 4 panes with {ggpubr}, ggviolin or ggbowplot with tests
# my_comparisons <- list( c("radial", "grand"), c("grand", "pca"), c("radial", "pca"))
# my_ggpubr <- function(df, title = "missing"){
#   ggboxplot(df,  x = "factor", y = "value", fill = "factor", alpha = .6,
#             palette = "Dark2",
#             add = "jitter", add.params = list(color = "factor", alpha = .3, width = .2)) +
#     stat_compare_means(comparisons = my_comparisons, label = "p.format") +
#     stat_compare_means(label.y = 7, method = "anova") + 
#     theme_minimal() +
#     ggtitle(title)
# }
# 
# (p_like <- my_ggpubr(subjective_longer %>% filter(measure == "like"), "Preference") +
#     theme(legend.position = "off"))
# (p_ease <-
#     my_ggpubr(subjective_longer %>% filter(measure == "ease"), "Ease of use") +
#     theme(legend.position = "off",
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()))
# (p_confident <-
#     my_ggpubr(subjective_longer %>% filter(measure == "confidence"), "Confidence") +
#     theme(legend.position = "off",
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()))
# (p_familar <-
#     my_ggpubr(subjective_longer %>% filter(measure == "familar"), "Familiarity") +
#     theme(legend.position = "off",
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()))
# 
# ## Cowplot and bringing it together
# library(cowplot)
# comp <- cowplot::plot_grid(p_like, p_ease, p_confident, p_familar,
#                            nrow = 1, rel_widths = c(1, 1, 1, 1))
# legend <- get_legend(
#   p_like + theme(legend.box.margin = margin(0, 12, 0, 12),
#                  legend.position = "bottom",
#                  legend.justification = "center")
# )
# supertitle <- ggdraw() +
#   draw_label(
#     "Participant subjective measures",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0
#   )
# subtitle <- ggdraw() +
#   draw_label(
#     "Likert [1-5], least to most",
#     x = 0,
#     hjust = 0
#   )
# (out <- plot_grid(
#   supertitle, subtitle, comp, legend,
#   ncol = 1,
#   rel_heights = c(.1, .1, 1, .1)
# ))
# 
# ## Save
# ggsave("./paper/figures/figSubjectiveMeasures.png", out, width = 8, height = 4, units = "in")
