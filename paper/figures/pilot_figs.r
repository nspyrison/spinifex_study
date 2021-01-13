library(tidyverse)

df1 <- read.csv2("./apps/study/v1_pilot_studies/responses_1_v3pilot_Judy.csv", sep = ",", stringsAsFactors = F)
df2 <- read.csv2("./apps/study/v1_pilot_studies/responses_1_v3pilot_Sabrina.csv", sep = ",", stringsAsFactors = F)
df <- rbind(df1, df2)


### TASK 1 
task1 <- filter(df, task == 1, factor %in% c("pca", "grand", "manual"), 
                response != "none (default)" & !is.null(response) )
task1 <- mutate(task1, ttr = as.integer(ttr), 
                pos_score = max(abs(score)) - abs(score
                factor = factor(factor, levels = c("pca", ),"grand", "manual")))
str(task1)

(g1_t1 <- ggplot(task1) + geom_jitter(aes(x = factor, y = pos_score),
                                      width = .2, height = .2) + 
    labs(title = "Task1 (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))
(g2_t1 <- ggplot(task1) + geom_jitter(aes(x = factor, y = ttr),
                                      width = .2, height = .2) + 
    labs(title = "Task1 (3vpilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))

### TASK 2 PLOTS
task2_ln <- filter(df, task == 2, factor %in% c("pca", "grand", "manual"), 
                   response != "none (default)" & !is.null(response) )
task2_ln <- mutate(task2_ln, ttr = as.integer(ttr), 
                   pos_line_score = line_score + abs(min(line_score)),
                   factor = factor(factor, levels = c("pca", "grand", "manual")))

task2_tsk <- task2_ln %>%
    group_by(user_uid, factor, task, sim_id) %>%
    summarize(max_ttr = max(ttr),
              sum_pos_line_score = sum(pos_line_score),
              sum_manual_inter = sum(manual_inter),
              sum_pca_inter = sum(pca_inter))


(g1_t2 <- ggplot(task2_tsk) + geom_jitter(aes(x = factor, y = sum_pos_line_score),
                                          width = .2, height = .2) + 
    labs(title = "Task2 Task grain (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))
(g2_t2 <- ggplot(task2_tsk) + geom_jitter(aes(x = factor, y = max_ttr),
                                          width = .2, height = .2) + 
    labs(title = "Task2 Task grain (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))
gridExtra::grid.arrange(g1_t1, g2_t1, g1_t2, g2_t2, ncol=2)

(FIG1 <- gridExtra::arrangeGrob(g1_t1, g2_t1, g1_t2, g2_t2, ncol=2))
# ggsave("./paper/figures/v3pilot.png", plot = FIG1, width = 6, height = 6, units = "in")
# ggsave("../mid_canidature/figures/v3pilot.png", plot = FIG1, width = 6, height = 6, units = "in")

### Q did people use manual interactions enough?
task2_tsk_manual <- filter(task2_tsk, factor == "pca")
ggplot(task2_tsk_manual) + geom_jitter(aes(x = sum_manual_inter, y = sum_pos_line_score),
                                width = .2, height = .2) + 
    labs(title = "Task2 Task grain (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")

ggplot(task2_tsk_manual) + geom_jitter(aes(x = sum_pca_inter, y = sum_pos_line_score),
                                       width = .2, height = .2) + 
    labs(title = "Task2 Task grain (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")

### SURVEY
##TODO: RESUME HERE <<<<<<
survey <- df[which(df$factor == "survey"),]

survey$survey_factor <- substr(survey$task, 8, nchar(survey$task)) # THESE ARE NUMBERS NOT NAMES


survey$response <- as.integer(substr(survey$response, 1,1))
factor_survey <- survey[which(survey$survey_factor == ""),]

(g1_s <- ggplot(survey) + geom_boxplot(aes(x = factor, y = pos_score)) + 
    labs(title = "Task2 (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))



