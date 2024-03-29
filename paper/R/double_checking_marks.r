## In the paper the support of the marks is regularly outside of [-1, 1]
#### ; trying to double check that here. using the same simulation 
####   shown in figure 3; EEV_p6_33_66_rep2


# Checking original simulation -----
## Validate mean diff between clusters a and b, absolute, not as a rate
load("~/R/spinifex_study/apps_supplementary/data/EEV_p6_33_66_rep2.rda")
#str(EEV_p6_33_66_rep2)
## Attached means between clusters a and b
claim <- attr(EEV_p6_33_66_rep2, "var_mean_diff_ab") ## ABS mean diff between a and b
clas  <- attr(EEV_p6_33_66_rep2, "cluster")
## Double checking:
means_cl_a <- colMeans(EEV_p6_33_66_rep2[clas == "cl a", ])
means_cl_b <- colMeans(EEV_p6_33_66_rep2[clas == "cl b", ])
check_diff_ab <- abs(means_cl_a - means_cl_b)
all(claim == check_diff_ab) ## Checks out
## It seem like the ABS mean diff is correct.
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
claim_frac_abs_cl_sep <- .sim[, 3:8]
sum(claim_frac_abs_cl_sep) ## note that this is rate of cluster separation; sum is 1!
## create check, rate of cl sep.
check_frac_abs_cl_sep <- check_diff_ab / sum(check_diff_ab)
all(claim_frac_abs_cl_sep == check_frac_abs_cl_sep) ## Checks out

## Checking weights; variable difference from bar
claim_weights <- .sim[, 9:14]
check_weights <- check_frac_abs_cl_sep - 1 / 6
all(claim_weights == check_weights) ## Checks out
sum(check_weights) ## weights sum to 0

## Checking Accuracy:
claim_accuracy <- .sim[, 15:20]
check_accuracy <- sign(check_weights) * sqrt(abs(check_weights))
all(claim_accuracy == check_accuracy) ## checks out, see below
## Note thar check_ is for the original (sqrt) measure, had since changed to the [0, 1]
## Version and not going to the squared measure

# Below is about 01 measure ----
## now we are going to squared measure, no expectation of suming to 0 or in [0,1]
#### Di says no need to sum to 0 or be [0,1] bound; going to squared weights

## _!!!Here is an issue!!!_ ----
#### Sum of weights is 0, but not accuracy; issue with non-linear scaling.
sum(check_accuracy)
sum(check_accuracy[sign(check_accuracy) ==  1])
sum(check_accuracy[sign(check_accuracy) == -1])

## Findings ----
#### Accuracy not in [-1, 1] because of the non-linear scaling of more variables 
#### below uniform distribution, But still want to scale magnetude; so apply
#### a linear fraction sclaling to each sign of the accurcay after the ^2 scaling

## response: Changing ans_tbl, then resp_tbl again
if(F)
  file.edit("./apps/spinifex_study/resp_tbl.r")
## Weights going from: _ * sqrt(w_i)
##                 to: _ * { w_+^2 / sum(w_+^2) | Pos elements
##                         { w_-^2 / sum(w_-^2) | Neg elements

#### ans_tbl changed in file above
## Check ans_tbl ----
library(dplyr)
library(tidyr)
ans_tbl <- readRDS("~/R/spinifex_study/apps/spinifex_study/www/ans_tbl.rds")

### Check signal
## Check signal; fraction of total cluster separation
ans_tbl %>%
    rowwise() %>%
    dplyr::select(sim_nm, v1_signal, v2_signal, v3_signal, v4_signal, v5_signal, v6_signal) %>%
    pivot_longer(!sim_nm, "var", values_to = "signal", values_drop_na = TRUE) %>%
    group_by(sim_nm) %>%
    summarise(sum     = sum(signal),
              sum_pos = sum(signal[sign(signal) ==  1]),
              sum_neg = sum(signal[sign(signal) == -1]))
## Good

### Check weight
ans_tbl %>%
  rowwise() %>%
  dplyr::select(sim_nm, v1_weight, v2_weight, v3_weight, v4_weight, v5_weight, v6_weight) %>%
  pivot_longer(!sim_nm, "var", values_to = "weight", values_drop_na = TRUE) %>%
  group_by(sim_nm) %>%
  summarise(sum     = sum(weight),
            sum_pos = sum(weight[sign(weight) ==  1]),
            sum_neg = sum(weight[sign(weight) == -1]))
## Good; zeros out

## Check accuracy; 
ans_tbl %>%
  rowwise() %>%
  dplyr::select(sim_nm, v1_accuracy, v2_accuracy, v3_accuracy, v4_accuracy, v5_accuracy, v6_accuracy) %>%
  pivot_longer(!sim_nm, "var", values_to = "accuracy", values_drop_na = TRUE) %>%
  group_by(sim_nm) %>%
  summarise(sum     = sum(accuracy),
            sum_pos = sum(accuracy[sign(accuracy) ==  1]),
            sum_neg = sum(accuracy[sign(accuracy) == -1]))
## Good; zeros out

### Change measure to v2_01scaled -----
## then Apply to resp_tbl
library(dplyr)
raw <- readRDS("./paper/data_study/raw.rds")
colnames(raw)
length(33:ncol(raw)) ## Note that the last 19 columns are the ans_tbl
resp <- raw[, 1:32]
ans_tbl <- readRDS("~/R/spinifex_study/apps/spinifex_study/www/ans_tbl.rds")
tmp <- left_join(resp, ans_tbl, by = "sim_nm")
colnames(tmp)
#str(tmp) ## NA's keep in mind the grain of the response table is app pager; finer than trial eval.

## For each row, if *_accuracy not NA, apply over *_marks
r_idx <- which(!(tmp$v1_accuracy %>% is.na())) ## numeric row index of simulations
.m <- sapply(r_idx, FUN = function(i){
  ## If row has non-na value of v1_weight, replace marks
  tmp$v1_marks[i] <<- tmp$v1_resp[i] * tmp$v1_accuracy[i]
  tmp$v2_marks[i] <<- tmp$v2_resp[i] * tmp$v2_accuracy[i]
  tmp$v3_marks[i] <<- tmp$v3_resp[i] * tmp$v3_accuracy[i]
  tmp$v4_marks[i] <<- tmp$v4_resp[i] * tmp$v4_accuracy[i]
  ## and for Var 5/6 if applicable.
  if(tmp$p_dim[i] == "p6"){
    tmp$v5_marks[i] <<- tmp$v5_resp[i] * sign(tmp$v5_weight[i]) * sqrt(abs(tmp$v5_weight[i]))
    tmp$v6_marks[i] <<- tmp$v6_resp[i] * sign(tmp$v6_weight[i]) * sqrt(abs(tmp$v6_weight[i]))
    tmp$task_marks[i] <<- tmp$v1_marks[i] + tmp$v2_marks[i] +
      tmp$v3_marks[i] + tmp$v4_marks[i] + tmp$v5_marks[i] + tmp$v6_marks[i]
  }else{ ## Task marks for dim = p4
    tmp$task_marks[i] <<- tmp$v1_marks[i] + tmp$v2_marks[i] +
      tmp$v3_marks[i] + tmp$v4_marks[i]
  }
})
## Check accuracy
tmp %>%
  dplyr::filter(is.na(task_marks) == FALSE) %>%
  dplyr::select(factor, task_marks) %>%
  dplyr::group_by(factor) %>%
  dplyr::summarise(mean = mean(task_marks),
                   sd   = sd(task_marks),
                   min  = min(task_marks),
                   max  = max(task_marks)) ## Note Grand tour never gets all wrong.
saveRDS(tmp, "./paper/data_study/raw.rds")


## Checking results with mixed_model_regression.rmd.
#### RESULTS are different, but doesn't look too different from the violin plots
#### Will continue checking tomorrow.
factor_agg <- function(tib){
  tib %>%
    dplyr::filter(is.na(task_marks) == FALSE) %>%
    dplyr::select(factor, task_marks) %>%
    dplyr::group_by(factor) %>%
    dplyr::summarise(mean = mean(task_marks),
                     sd   = sd(task_marks),
                     min  = min(task_marks),
                     max  = max(task_marks))
}
v1 <- readRDS("./paper/data_study/raw___v1_sqrt.rds")
v2 <- readRDS("./paper/data_study/raw_01scaled.rds")
factor_agg(v1)
factor_agg(v2)

## Rerun mixed_model_regression.rmd ----
#### then other figures and tables

## After running mixed model regression still off; check dat_qual
dat_qual <- readRDS("./paper/data_study/dat_qual.rds")
dat_qual %>%
  dplyr::select(Factor, Marks) %>%
  dplyr::group_by(Factor) %>%
  dplyr::summarise(mean = mean(Marks),
                   sd   = sd(  Marks),
                   min  = min( Marks),
                   max  = max( Marks))
## Ok, good now bounded in [-1, 1]; but Means have changed a lot...

## Checking Logic getting to dat_qual, as Factor mean Marks have changed significantly.
#### seem quite reasonable, keep in mind that this data has training, partial 
#### trial and over sampled permutations that are removed in the analysis.

#### The sizable differnce in means may be an effort effect or easy of training,
#### resulting in much better performance






# Below is about squared measure ----
## ns, 05 Feb 2022
if(F)
  file.edit("./apps/spinifex_study/resp_tbl.r")
## Make change to the answer table then join it to the responses.
#### see make_save_ans_tbl()


### Change measure to v3_sq -----
## then Apply to resp_tbl
library(dplyr)
raw <- readRDS("./paper/data_study/raw.rds")
colnames(raw)
length(33:ncol(raw)) ## Note that the last 19 columns are the ans_tbl
resp <- raw[, 1:32]
ans_tbl <- readRDS("~/R/spinifex_study/apps/spinifex_study/www/ans_tbl.rds")
tmp <- left_join(resp, ans_tbl, by = "sim_nm")
colnames(tmp)
#str(tmp) ## NA's keep in mind the grain of the response table is app pager; finer than trial eval.

## For each (non NA) row, if *_accuracy, apply over *_marks
r_idx <- which(!(tmp$v1_accuracy %>% is.na())) ## numeric row index of simulations
.m <- sapply(r_idx, FUN = function(i){
  ## If row has non-na value of v1_weight, replace marks
  tmp$v1_marks[i] <<- tmp$v1_resp[i] * tmp$v1_accuracy[i]
  tmp$v2_marks[i] <<- tmp$v2_resp[i] * tmp$v2_accuracy[i]
  tmp$v3_marks[i] <<- tmp$v3_resp[i] * tmp$v3_accuracy[i]
  tmp$v4_marks[i] <<- tmp$v4_resp[i] * tmp$v4_accuracy[i]
  ## and for Var 5/6 if applicable.
  if(tmp$p_dim[i] == "p6"){
    tmp$v5_marks[i] <<- tmp$v5_resp[i] * sign(tmp$v5_weight[i]) * sqrt(abs(tmp$v5_weight[i]))
    tmp$v6_marks[i] <<- tmp$v6_resp[i] * sign(tmp$v6_weight[i]) * sqrt(abs(tmp$v6_weight[i]))
    tmp$task_marks[i] <<- tmp$v1_marks[i] + tmp$v2_marks[i] +
      tmp$v3_marks[i] + tmp$v4_marks[i] + tmp$v5_marks[i] + tmp$v6_marks[i]
  }else{ ## Task marks for dim = p4
    tmp$task_marks[i] <<- tmp$v1_marks[i] + tmp$v2_marks[i] +
      tmp$v3_marks[i] + tmp$v4_marks[i]
  }
})
## Check accuracy
tmp %>%
  dplyr::filter(is.na(task_marks) == FALSE) %>%
  dplyr::select(factor, task_marks) %>%
  dplyr::group_by(factor) %>%
  dplyr::summarise(mean = mean(task_marks),
                   sd   = sd(task_marks),
                   min  = min(task_marks),
                   max  = max(task_marks))
saveRDS(tmp, "./paper/data_study/raw.rds")


## Checking results with mixed_model_regression.rmd.
factor_agg <- function(tib){
  tib %>%
    dplyr::filter(is.na(task_marks) == FALSE) %>%
    dplyr::select(factor, task_marks) %>%
    dplyr::group_by(factor) %>%
    dplyr::summarise(mean = mean(task_marks),
                     sd   = sd(task_marks),
                     min  = min(task_marks),
                     max  = max(task_marks))
}
v1 <- readRDS("./paper/data_study/raw___v1_sqrt.rds") 
v2 <- readRDS("./paper/data_study/raw___v2_01scaled.rds")
v3 <- readRDS("./paper/data_study/raw.rds") ##~___v3_sq.rds
factor_agg(v1)
factor_agg(v2)
factor_agg(v3) ## Hmm grand-pca ~ as much as radial-pca, surprising

## Rerun mixed_model_regression.rmd ----
#### then other figures and tables
