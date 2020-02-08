library(tidyverse)

df1 <- read.csv2("./apps/study/responses_3_v2pilot_NICK1.csv", sep = ",")
df2 <- read.csv2("./apps/study/responses_2_v2pilot_NICK2.csv", sep = ",")
str(df1)

df <- rbind(df1, df2)
df <- mutate(df, task = substr(taskblock, 1, 1), 
             block = substr(taskblock, 2, 2))





### TASK 1 
task1 <- filter(df, task == "n", factor != "training", response != "none (default)")
task1 <- mutate(task1, duration = as.integer(duration), 
                pos_score = max(abs(score)) - abs(score),
                factor = factor(factor, levels = c("pca", "grand", "manual")))
str(task1)

(g1_t1 <- ggplot(task1) + geom_boxplot(aes(x = factor, y = pos_score)) + 
    labs(title = "Task1 (Nick pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))
(g2_t1 <- ggplot(task1) + geom_boxplot(aes(x = factor, y = duration)) + 
    labs(title = "Task1 (Nick pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))

### TASK 2 PLOTS
task2 <- filter(df, task == "p", factor != "training", response != "none (default)")
task2 <- mutate(task2, duration = as.integer(duration), 
                pos_score = score + abs(min(score)),
                factor = factor(factor, levels = c("pca", "grand", "manual")))
str(task2)

(g1_t2 <- ggplot(task2) + geom_boxplot(aes(x = factor, y = pos_score)) + 
    labs(title = "Task2 (Nick pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))
(g2_t2 <- ggplot(task2) + geom_boxplot(aes(x = factor, y = duration)) + 
    labs(title = "Task2 (Nick pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))

OUT_FIG <- gridExtra::grid.arrange(g1_t1, g2_t1, g1_t2, g2_t2, ncol=2)
### SURVEY
survey <- filter(df, factor %in% c("survey_pca", "survey_grand", "survey_manual"))
##TODO: RESUME HERE <<<<<<
survey <- mutate(survey, response = as.integer(substr(response, 1,1)),
                 aspect = if(question == "I felt confident in my answer with this visualization.") confidence)

(g1_t2 <- ggplot(task2) + geom_boxplot(aes(x = factor, y = pos_score)) + 
    labs(title = "Task2 (Nick pilot)") + theme_minimal() + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2"))



OUT_FIG <- gridExtra::grid.arrange(g1_t1, g2_t1, g1_t2, g2_t2, ncol=2)
OUT_FIG2 <- 

