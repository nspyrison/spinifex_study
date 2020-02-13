library(tidyverse)

# df1 <- read.csv2("./apps/study/responses_3_v2pilot_NICK1.csv", sep = ",")
# df2 <- read.csv2("./apps/study/responses_2_v2pilot_NICK2.csv", sep = ",")
df1 <- read.csv2("./apps/study/responses_1_v3pilot_Judy.csv", sep = ",", stringsAsFactors = F)
df2 <- read.csv2("./apps/study/responses_1_v3pilot_Sabrina.csv", sep = ",", stringsAsFactors = F)
str(df1)

# colnames(df1) <- colnames(df2)
# df1$user_uid <- paste0("1_", df1$user_uid)
# df1[41:72, ] <- df1b[41:72, ]
# write.csv(df1, file = "responses_1_v3_Judy_ab.csv", row.names = FALSE)

df <- rbind(df1, df2)


### TASK 1 
task1 <- filter(df, task == 1, factor %in% c("pca", "grand", "manual"), 
                response != "none (default)" & !is.null(response) )
task1 <- mutate(task1, ttr = as.integer(ttr), 
                pos_score = max(abs(score)) - abs(score),
                factor = factor(factor, levels = c("pca", "grand", "manual")))
str(task1)

(g1_t1 <- ggplot(task1) + geom_boxplot(aes(x = factor, y = pos_score)) + 
    labs(title = "Task1 (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))
(g2_t1 <- ggplot(task1) + geom_boxplot(aes(x = factor, y = ttr)) + 
    labs(title = "Task1 (3vpilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))

### TASK 2 PLOTS
task2_ln <- filter(df, task == 2, factor %in% c("pca", "grand", "manual"), 
                   response != "none (default)" & !is.null(response) )
task2_ln <- mutate(task2, ttr = as.integer(ttr), 
                   pos_line_score = line_score + abs(min(line_score)),
                   factor = factor(factor, levels = c("pca", "grand", "manual")))

task2_tsk <- task2_ln %>%
    group_by(user_uid, factor, task, sim_id) %>%
    summarize(max_ttr = max(ttr),
              sum_pos_line_score = sum(pos_line_score))


(g1_t2 <- ggplot(task2_tsk) + geom_boxplot(aes(x = factor, y = sum_pos_line_score)) + 
    labs(title = "Task2 Task grain (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))
(g2_t2 <- ggplot(task2_tsk) + geom_boxplot(aes(x = factor, y = max_ttr)) + 
    labs(title = "Task2 Task grain (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))

(FIG1 <- gridExtra::arrangeGrob(g1_t1, g2_t1, g1_t2, g2_t2, ncol=2))
ggsave("./paper/figures/v3pilot.png", plot = FIG1, width = 6, height = 6, units = "in")

### SURVEY
##TODO: RESUME HERE <<<<<<
survey <- filter(df, factor %in% c("survey_pca", "survey_grand", "survey_manual"))

survey <- mutate(survey, response = as.integer(substr(response, 1,1)),
                 aspect = if(question == "I felt confident in my answer with this visualization.") confidence)

(g1_t2 <- ggplot(task2) + geom_boxplot(aes(x = factor, y = pos_score)) + 
    labs(title = "Task2 (v3pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))



OUT_FIG2 <- 

