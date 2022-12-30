require("tidyverse")
require("dplyr")

## Follow the loose setup of _analysis.rmd:

.u = "in"
.w = 6.25
.h = 9
.l_lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")

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
  
  ###### pivot wider, 1 row is 1 survey -----
  ## Pivot questions wider, 1 row is now a survey.
  survey_wider <- raw %>%
    mutate(prolific_id = stringr::str_trim(prolific_id),
           instance_id = paste(participant_num, full_perm_num, prolific_id, sep = "_")) %>%
    dplyr::select(!c(key, survey_num, sec_to_resp, write_dt, scope)) %>%
    pivot_wider(names_from = question, values_from = response)
  
  ## Decode question names.
  colnames(survey_wider)[5:22] <-
    c("pronoun", "age", "education", "task_understanding", "data_viz_exp", "analysis_exp",
      "grand_familar", "grand_ease", "grand_confidence", "grand_like",
      "pca_familar", "pca_ease", "pca_confidence", "pca_like",
      "radial_familar", "radial_ease", "radial_confidence", "radial_like")
  survey_wider <- survey_wider %>%
    mutate(
      participant_num = factor(participant_num),
      full_perm_num = factor(full_perm_num),
      prolific_id = factor(prolific_id),
      instance_id = factor(instance_id),
      pronoun = factor(pronoun, levels = c("he/him", "she/her", "thy/them or other", 
                                             "decline to answer <default, blank, no change>")),
      age = as.factor(age), ## Order already correct.
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
  
  survey_wider <- survey_wider %>%
    mutate(
      pronoun = plyr::mapvalues(
        pronoun, from = levels(pronoun),
        to = c("he/him (n=44)", "she/her (n=31)", "they/them or other (n=5)", "decline/default (n=4)")),
      education = plyr::mapvalues(
        education, from = levels(education),
        to = c("undergraduate", "graduate", "doctorate", "decline/default")),
      age = plyr::mapvalues(
        age, from = "decline to answer <default, blank, no change>",
        to = "decline/default"),
      task_understanding = factor(plyr::mapvalues(task_understanding, from = 1:5, to = .l_lvls)),
      data_viz_exp =
        factor(plyr::mapvalues(data_viz_exp, from = 1:5, to = .l_lvls)),
      analysis_exp =
        factor(plyr::mapvalues(analysis_exp, from = 1:5, to = .l_lvls)),
      grand_familar =
        factor(plyr::mapvalues(grand_familar, from = 1:5, to = .l_lvls)),
      grand_ease =
        factor(plyr::mapvalues(grand_ease, from = 1:5, to = .l_lvls)),
      grand_confidence =
        factor(plyr::mapvalues(grand_confidence, from = 1:5, to = .l_lvls)),
      grand_like =
        factor(plyr::mapvalues(grand_like, from = 1:5, to = .l_lvls)),
      pca_familar =
        factor(plyr::mapvalues(pca_familar, from = 1:5, to = .l_lvls)),
      pca_ease =
        factor(plyr::mapvalues(pca_ease, from = 1:5, to = .l_lvls)),
      pca_confidence =
        factor(plyr::mapvalues(pca_confidence, from = 1:5, to = .l_lvls)),
      pca_like =
        factor(plyr::mapvalues(pca_like, from = 1:5, to = .l_lvls)),
      radial_familar =
        factor(plyr::mapvalues(radial_familar,from = 1:5, to = .l_lvls)),
      radial_ease =
        factor(plyr::mapvalues(radial_ease, from = 1:5, to = .l_lvls)),
      radial_confidence =
        factor(plyr::mapvalues(radial_confidence, from = 1:5, to = .l_lvls)),
      radial_like =
        factor(plyr::mapvalues(survey_wider$radial_like, from = 1:5, to = .l_lvls))
    )
  ## Save task aggregated data.
  saveRDS(survey_wider, "./paper/data_survey/survey_wider.rds")
}

#### Load and plot as demographic heatmap ----
## Load aggregated data. filter to only surveys in the 108 instances in the analysis
survey_wider <- readRDS("./paper/data_survey/survey_wider.rds")
instance_id_whitelist <- readRDS("./paper/data_study/instance_id_whitelist.rds")
## Only Prolific participants that were in the 108 instance_ids in the analysis
survey_wider <- survey_wider %>%
  filter(nchar(as.character(prolific_id)) == 24,
         instance_id %in% instance_id_whitelist)
str(survey_wider)

## Change character to factor, include counts in the levels of sex?
(demographic_heatmaps <- ggplot(survey_wider, aes(education, age)) +
    stat_bin2d(aes(fill = after_stat(count))) +
    geom_text(aes(label = after_stat(count)), stat = "bin2d") +
    facet_grid(cols = vars(pronoun), labeller = label_wrap_gen(width=18)) + 
    theme_bw() +
    labs(x = "Education", y = "Age", fill = "Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          # legend.position = "bottom",
          # legend.direction = "horizontal",
          legend.margin = margin(0, 0, 0, 0)) +
    scale_fill_gradient(low = "lightpink", high = "firebrick", na.value = NA) +
    ggtitle("Participant demographics"))
if(F)
  ggsave(filename = "./paper/figures/figSurveyDemographics.pdf",
         plot = demographic_heatmaps, device = "pdf", width = .w, height = .w/2)


## Subjective measures, Boxplots -----
survey_wider <- readRDS("./paper/data_survey/survey_wider.rds")
str(survey_wider)

{
  ## pivot_longer within factor
  radial_longer <- survey_wider %>%
    dplyr::select(instance_id, radial_familar:radial_like) %>%
    tidyr::pivot_longer(radial_familar:radial_like,
                        names_to = "factor", values_to = "value")
  grand_longer <- survey_wider %>%
    dplyr::select(instance_id, grand_familar:grand_like) %>%
    tidyr::pivot_longer(grand_familar:grand_like,
                        names_to = "factor", values_to = "value")
  pca_longer <- survey_wider %>%
    dplyr::select(instance_id, pca_familar:pca_like) %>%
    tidyr::pivot_longer(pca_familar:pca_like,
                        names_to = "factor", values_to = "value")
  ## Combine and split measure from factor
  subjective_longer <- rbind(radial_longer, grand_longer, pca_longer) %>%
    tidyr::separate(factor, c("factor", "measure"), sep = "_")
}
## Technically not continuous numeric, will show side by side with Likert plot.
.lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
subjective_longer <- subjective_longer %>%
  mutate(value = as.integer(plyr::mapvalues(value, from = .lvls, to = 1L:5L)),
         measure = factor(plyr::mapvalues(measure,
                                          from = c("like", "ease", "confidence", "familar"),
                                          to = c("preference", "ease of use", "confidence", "familiarity"))),
         factor = factor(factor, levels = c("pca", "grand", "radial"))
  )

# ch4_fig7_subjective_measures -----
## Follow the loose setup of _analysis.rmd:
.u <- "in"
.w <- 6.25
.h <- 9
.l_lvls      <- c("most disagree", "disagree", "neutral", "agree", "most agree")

## indirect (via ggpubr/cowplot) ggplot2 helpers
#' @examples
#' df=dat_qual;x="Visual";y="Marks";title = waiver(); subtitle = waiver(); facet = NULL
require(ggpubr)
my_ggpubr <- function(
  df, x = "Visual", y = "Marks",
  title = waiver(), subtitle = waiver(), facet = NULL,
  y_pval_coef = .08, ## Subjective wants .032
  ylim_max_coef = .5 ## Subjective wants .6
){
  ## Plot
  ggplot(df, aes_string(x, y, color = x), size=2) +
    facet_grid(~measure) +
    stat_summary(geom="errorbar", fun.data=mean_cl_boot, width=0.2)+
    stat_summary(geom="point", fun.y=mean) +
    ggtitle(title, subtitle) +
    list(
      theme_bw(),
      scale_color_brewer(palette = "Dark2"),
      scale_fill_brewer( palette = "Dark2"),
      labs(x="Visual",y="Likert score [0, 5]"),
      theme(
            legend.position = "off",
            plot.margin = margin(t = .5,r = .5, b = .5, l = .5, "lines")
      )
    )
}
my_ggpubr_facet <- function(..., facet = "Location"){
  facet(my_ggpubr(..., facet = facet), facet.by = facet)
}

{
  ## pivot_longer within visual
  radial_longer <- survey_wider %>%
    dplyr::select(instance_id, radial_familar:radial_like) %>%
    tidyr::pivot_longer(radial_familar:radial_like,
                        names_to = "visual", values_to = "value")
  grand_longer <- survey_wider %>%
    dplyr::select(instance_id, grand_familar:grand_like) %>%
    tidyr::pivot_longer(grand_familar:grand_like,
                        names_to = "visual", values_to = "value")
  pca_longer <- survey_wider %>%
    dplyr::select(instance_id, pca_familar:pca_like) %>%
    tidyr::pivot_longer(pca_familar:pca_like,
                        names_to = "visual", values_to = "value")
  ## Combine and split measure from visual
  subjective_longer <- rbind(radial_longer, grand_longer, pca_longer) %>%
    tidyr::separate(visual, c("visual", "measure"), sep = "_")
}
## Technically not continuous numeric, will show side by side with Likert plot.
.lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
subjective_longer <- subjective_longer %>%
  mutate(value = as.integer(plyr::mapvalues(value, from = .lvls, to = 1L:5L)),
         measure = factor(plyr::mapvalues(
           measure,
           from = c("like", "ease", "confidence", "familar"),
           to = c("preference", "ease of use", "confidence", "familiarity"))),
         visual = factor(visual, levels = c("pca", "grand", "radial"))
  )


{ ## Subjective violins
  .l_lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
  col_idx <- 8:22
  col_nms <- colnames(survey_wider[, col_idx])
  survey_agg <- tibble()
  mute <- sapply(col_nms, function(col_nm){
    tmp <- survey_wider[col_nm] %>%
      group_by_all() %>%
      count() %>%
      as.data.frame()
    .this_agg <- data.frame(question = col_nm,
                            response = tmp[, 1],
                            n = tmp[, 2],
                            percent = 100 * tmp[, 2] / sum(tmp[, 2]))
    survey_agg <<- rbind(survey_agg, .this_agg)
  })
  #str(survey_agg)
  
  ## Format likert questions
  likert_q_nms <- colnames(survey_wider[, 11:22])
  likert <<- survey_agg %>% filter(question %in% likert_q_nms) %>%
    tidyr::separate(question, c("visual", "question"), sep = "_") %>%
    mutate(visual = factor(visual, levels = rev(c("pca", "grand", "radial"))),
           response = factor(response, levels = rev(.l_lvls)))
  likert$question <-
    plyr::mapvalues(likert$question,
                    from = c("like", "ease", "confidence", "familar"),
                    to = c("preference", "ease of use", "confidence", "familiarity")) %>%
    factor()
  #str(likert)
  }

## Likert plots -----
(subjectiveMeasures <-
   ggplot(likert, aes(x = percent, y = visual, fill = response)) +
   geom_bar(position = "fill", stat = "identity", width = .6) + facet_grid(vars(question)) +
   ggtitle("Subjective measures", "Likert scale [1-5]") +
   theme_bw() +
   scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "PRGn"))) +
   theme(legend.position = "bottom",
         legend.direction = "horizontal") +
   # Reverse order that fill is displayed in legend.
   guides(fill = guide_legend(reverse = TRUE)) +
   # x as % rather than rate.
   scale_x_continuous(labels = scales::percent) +
   coord_flip() +
   theme(legend.direction = "vertical") +
   guides(fill = guide_legend(reverse = FALSE)) +
   labs(x = "Visual", y = "Response rate", fill = "Response")
)

(measure_violins <- my_ggpubr_facet(
  df = subjective_longer, x = "visual", y = "value", facet = "measure",
  y_pval_coef = .032,  ## Subjective wants .032
  ylim_max_coef = .6) +  ## Subjective wants .6
    labs(x = "Visual", y = "Response", fill = "Visual") +
    theme(legend.position = "bottom", legend.direction = "horizontal"))
figSubjectiveMeasures_w.violin_hori <-  cowplot::plot_grid(
  subjectiveMeasures, measure_violins, ncol = 2)


### SAVING ------
## Cowplot and bringing it together
require("cowplot")

if(F){
  ggsave("./paper/figures/figSubjectiveMeasures_w.violin_hori.pdf",
         figSubjectiveMeasures_w.violin_hori, device = "pdf",
         width = .w, height = .w * 1, units = "in")
}

### Significance testing: ------
if(F)
  browseURL("https://bookdown.org/Rmadillo/likert/is-there-a-significant-difference.html#permutation-mann-whitney-tests")
?wilcox.test(value ~ variable, data = ex_1_long_y12)
?coin::oneway_test(value ~ variable, data = ex_1_long_y12, distribution = "exact")

# Subset to years 1 and 2
examp_2way <- likert %>% filter(question == "ease" & factor %in% c("radial", "pca"))
rstatix::wilcox_test(n~factor, data = examp_2way) ## W always in (7,9) p always .8 ...
examp_global <- likert %>%
  filter(question == "ease" & factor %in% c("radial", "pca")) %>%
  mutate(question = factor(question),
         dummy = factor(paste(factor, question))) %>% dplyr::select(dummy, n)

# str(examp_global)
# coin::oneway_test(n~dummy, data = examp_global) ## chi squared always 0; p = 1 ...
# coin::independence_test(n~dummy, data = examp_global)
