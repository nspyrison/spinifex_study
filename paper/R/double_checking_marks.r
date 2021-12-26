## In the paper the support of the marks is regularly outside of [-1, 1]
#### ; trying to double check that here. using the same simulation 
####   shown in figure 3; EEV_p6_33_66_rep2


# Checking original simulation -----
## Example checking mean diff between clusters a and b
load("~/R/spinifex_study/apps_supplementary/data/EEV_p6_33_66_rep2.rda")
str(EEV_p6_33_66_rep2)
## Attached means between clusters a and b
claim <- attr(EEV_p6_33_66_rep2, "var_mean_diff_ab") ## ABS mean diff between a and b
clas  <- attr(EEV_p6_33_66_rep2, "cluster")
## Double checking:
means_cl_a <- colMeans(EEV_p6_33_66_rep2[clas == "cl a", ])
means_cl_b <- colMeans(EEV_p6_33_66_rep2[clas == "cl b", ])
check_diff_ab <- abs(means_cl_a - means_cl_b)
all(claim == check_diff_ab) ## Consistent

## It seem like the ABS mean diff is correct at least for this example.
## before generalizing will check the weights as well.


# Checking resp_tbl/ans_tbl -----
## Example checking mean diff between clusters a and b

## Originally created and saved here: make_save_ans_tbl()
if(F)
  file.edit("./apps/spinifex_study/resp_tbl.r")
## Which creates and saves the following table
ans_tbl <- readRDS("~/R/spinifex_study/apps/spinifex_study/www/ans_tbl.rds")
#View(ans_tbl)
.sim <- ans_tbl[ans_tbl$sim_nm == "EEV_p6_33_66_rep2",]
print(.sim, width = 1000)
## Checking mean difference between clusters a and b
claim2 <- .sim[, 3:8] ## sum is 1!
### non abs sum.
check_rel_cl_sep___err <- check_diff_ab / sum(check_diff_ab)
all(claim2 == check_rel_cl_sep___err)
### Should do sum(abs(diff_ab)):
check_rel_cl_sep <- check_diff_ab / sum(abs(check_diff_ab))
claim2 - check_rel_cl_sep
warning("_!!Sizable difference here!!_")
## _!!Sizable difference here!!_ -----
## Checking weights; variable difference from bar
claim2_weights <- .sim[, 9:14]
check_weights  <- tibble::tibble(claim2 - 1 / 6)
all(claim2_weights == check_weights) ## Consistent
## Checking Accuracy:
claim2_accuracy <- .sim[, 15:20]
check_accuracy  <- sign(check_weights) * sqrt(abs(check_weights))
all(claim2_accuracy == check_accuracy) ## Consistent


## Finding 1 -----
#### it seems as though the ans_tbl tried to use the rate of cluster
#### separation, but instead of dividing bu the sum of the absolute difference
#### took the sum of the differences.

## Purposed solution 1
#### was going to create a for loop and fix here,
#### but resp_tbl.r was easy to fix, adding abs in line.

## Now will need to go through and make sure analysis is reran,
#### including all figure and tables here and in the thesis repo
load("~/R/spinifex_study/apps_supplementary/data/EEV_p6_33_66_rep2.rda")
str(EEV_p6_33_66_rep2)
## Attached means between clusters a and b
claim <- attr(EEV_p6_33_66_rep2, "var_mean_diff_ab") ## ABS mean diff between a and b
clas  <- attr(EEV_p6_33_66_rep2, "cluster")
## Double checking:
means_cl_a <- colMeans(EEV_p6_33_66_rep2[clas == "cl a", ])
means_cl_b <- colMeans(EEV_p6_33_66_rep2[clas == "cl b", ])
check_diff_ab <- abs(means_cl_a - means_cl_b)
all(claim == check_diff_ab) ## Consistent

ans_tbl <- readRDS("~/R/spinifex_study/apps/spinifex_study/www/ans_tbl.rds")
#View(ans_tbl)
.sim <- ans_tbl[ans_tbl$sim_nm == "EEV_p6_33_66_rep2",]
## Checking mean difference between clusters a and b
claim2 <- .sim[, 3:8] ## sum is 1!
### non abs sum.
check_rel_cl_sep___err <- check_diff_ab / sum(check_diff_ab)
all(claim2 == check_rel_cl_sep___err)
### Should do sum(abs(diff_ab)):
check_rel_cl_sep <- check_diff_ab / sum(abs(check_diff_ab))
claim2 - check_rel_cl_sep
message("Difference is now 0, but the result seem to be the same; double check the execution of mixed_model_regression.rmd?")

## Finding 2 -----

#### This has changed ans_tbl, but didn't change the results as they were
#### already written in /paper/data_study/raw.rds. will need to change there...

## Purposed solution 2:
#### Manually save a back up and adjust

library(dplyr)
raw <- readRDS("./paper/data_study/raw.rds")
dim(raw)
colnames(raw)
length(33:ncol(raw)) ## Note that the last 19 col are the ans_tbl that we fixed before
raw_l <- raw[, 1:32]
ans_tbl <- readRDS("~/R/spinifex_study/apps/spinifex_study/www/ans_tbl.rds")

tmp <- left_join(raw_l, ans_tbl, by = "sim_nm")
#str(tmp) ## NA's keep in mind the grain of the response table is app pager; finer than trial eval.
## BEFORE:
tmp %>% dplyr::select(factor, v1_marks) %>%
  filter(is.na(v1_marks) == FALSE) %>%
  mutate(factor = factor(factor)) %>% 
  group_by(factor) %>%
  dplyr::summarise(mean = mean(v1_marks),
                   sd   = sd(v1_marks))
## For each row, if *_weight not NA, paste over *_marks
.m <- sapply(1:nrow(tmp), FUN = function(i){
  ## If row has non-na value of v1_weight, replace marks
  if(tmp[i, "v1_weight"] %>% pull() %>% is.na() == FALSE){
    tmp$v1_marks[i] <<- tmp$v1_resp[i] * sign(tmp$v1_weight[i]) * sqrt(abs(tmp$v1_weight[i]))
    tmp$v2_marks[i] <<- tmp$v2_resp[i] * sign(tmp$v2_weight[i]) * sqrt(abs(tmp$v2_weight[i]))
    tmp$v3_marks[i] <<- tmp$v3_resp[i] * sign(tmp$v3_weight[i]) * sqrt(abs(tmp$v3_weight[i]))
    tmp$v4_marks[i] <<- tmp$v4_resp[i] * sign(tmp$v4_weight[i]) * sqrt(abs(tmp$v4_weight[i]))
    ## and for Var 5/6 if applicable.
    if(tmp[i, "v1_weight"] %>% pull() == "p6"){
      tmp$v5_marks[i] <<- tmp$v5_resp[i] * sign(tmp$v5_weight[i]) * sqrt(abs(tmp$v5_weight[i]))
      tmp$v6_marks[i] <<- tmp$v6_resp[i] * sign(tmp$v6_weight[i]) * sqrt(abs(tmp$v6_weight[i]))
    }
  }
})
## AFTER:
tmp %>% dplyr::select(factor, v1_marks) %>%
  filter(is.na(v1_marks) == FALSE) %>%
  mutate(factor = factor(factor)) %>%
  group_by(factor) %>%
  dplyr::summarise(mean = mean(v1_marks),
                   sd   = sd(v1_marks))
## Does have an effect!!

saveRDS(tmp, "./paper/data_study/raw.rds")


## Checking results with mixed_model_regression.rmd.
#### RESULTS are different, but doesn't look good from the violin plots
#### Will continue checking tomorrow.