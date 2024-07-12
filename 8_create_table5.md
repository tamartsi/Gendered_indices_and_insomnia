---
title: "Prepare table 5: sex and gendered indices association with insomnia"
output: 
  html_document:
    toc: true
    keep_md: true
---




```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.2     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.1     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(survey)
```

```
## Loading required package: grid
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
## 
## Loading required package: survival
## 
## Attaching package: 'survey'
## 
## The following object is masked from 'package:graphics':
## 
##     dotchart
```

```r
library(plyr)
```

```
## ------------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## ------------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:purrr':
## 
##     compact
```

```r
library(dplyr)
library(factoextra)
```

```
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
```

```r
library(labelled)
library(memisc)
```

```
## Loading required package: lattice
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## 
## Attaching package: 'memisc'
## 
## The following object is masked from 'package:plyr':
## 
##     rename
## 
## The following object is masked from 'package:Matrix':
## 
##     as.array
## 
## The following objects are masked from 'package:lubridate':
## 
##     as.interval, is.interval
## 
## The following objects are masked from 'package:dplyr':
## 
##     collect, recode, rename, syms
## 
## The following object is masked from 'package:purrr':
## 
##     %@%
## 
## The following object is masked from 'package:tibble':
## 
##     view
## 
## The following object is masked from 'package:ggplot2':
## 
##     syms
## 
## The following objects are masked from 'package:stats':
## 
##     contr.sum, contr.treatment, contrasts
## 
## The following object is masked from 'package:base':
## 
##     as.array
```

```r
library(sjlabelled)
```

```
## 
## Attaching package: 'sjlabelled'
## 
## The following objects are masked from 'package:labelled':
## 
##     copy_labels, remove_labels, to_character, to_factor, val_labels
## 
## The following object is masked from 'package:forcats':
## 
##     as_factor
## 
## The following object is masked from 'package:dplyr':
## 
##     as_label
## 
## The following object is masked from 'package:ggplot2':
## 
##     as_label
```

```r
library(boot)
```

```
## 
## Attaching package: 'boot'
## 
## The following object is masked from 'package:lattice':
## 
##     melanoma
## 
## The following object is masked from 'package:survival':
## 
##     aml
```

```r
library(glmnet)
```

```
## Loaded glmnet 4.1-7
```

```r
library(UpSetR)
```

```
## 
## Attaching package: 'UpSetR'
## 
## The following object is masked from 'package:lattice':
## 
##     histogram
```

```r
library(naniar)
library(RColorBrewer)
library(jtools)
```

```
## 
## Attaching package: 'jtools'
## 
## The following object is masked from 'package:memisc':
## 
##     %nin%
```

```r
library(reshape2)
```

```
## 
## Attaching package: 'reshape2'
## 
## The following object is masked from 'package:tidyr':
## 
##     smiths
```

```r
# source p-value formatting function
source("format_pvalue.R")
```

# Load and prepare the dataset


```r
folder_path <- "/Users/tamarsofer/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/Ongoing_papers/2022_gendered_indices"
dat_no_indices <- readRDS(file.path(folder_path, "Data/Data_with_miss.Rds"))

# the data with missing values:
# load the prepared dataset with indices
dat_miss <- readRDS(file.path(folder_path, "Data/Data_complete_case_with_indices.Rds"))


# load the imputed dataset with indices
dat_imp <- readRDS(file.path(folder_path, "Data/Data_imputed_with_indices.Rds"))

print(paste("The complete case dataset has", nrow(dat_miss), "individuals"))
```

```
## [1] "The complete case dataset has 13666 individuals"
```

```r
print(paste("The imputed dataset has", nrow(dat_imp), "individuals"))
```

```
## [1] "The imputed dataset has 16415 individuals"
```

```r
# because of the survey sampling design, we need to "merge" 
# the complete case dataset with design variables of the complete dataset
dat <- merge(dat_miss, 
             dat_no_indices[,c("ID", "STRAT" ,"PSU_ID", "WEIGHT_FINAL_NORM_OVERALL")], 
             by = c("ID", "STRAT" ,"PSU_ID", "WEIGHT_FINAL_NORM_OVERALL"), all = TRUE)

dat_imp <- merge(dat_imp, 
                 dat_no_indices[,c("ID", "WEIGHT_FINAL_NORM_OVERALL", "PSU_ID")], 
                 by = c("ID"))

nrow(dat) == nrow(dat_imp)
```

```
## [1] TRUE
```
# add scaled indices to describe effect per 1 SD increase


```r
survey_cc <- svydesign(id = ~PSU_ID,
                          strata = ~STRAT,
                          weights = ~WEIGHT_FINAL_NORM_OVERALL,
                          nest = TRUE,
                          data = dat)
survey_cc <- subset(survey_cc, complete.cases(dat))

survey_imp <- svydesign(id = ~PSU_ID,
                          strata = ~STRAT,
                          weights = ~WEIGHT_FINAL_NORM_OVERALL,
                          nest = TRUE,
                          data = dat_imp)

cur_mean <- svymean(~GISE, survey_cc)[1]
cur_sd <- sqrt(svyvar(~GISE, survey_cc)[1])
dat_miss$GISE_scaled <- (dat_miss$GISE - cur_mean)/cur_sd

cur_mean <- svymean(~GIPSE, survey_cc)[1]
cur_sd <- sqrt(svyvar(~GIPSE, survey_cc)[1])
dat_miss$GIPSE_scaled <- (dat_miss$GIPSE - cur_mean)/cur_sd

for (suffix in c("", ".1", ".2", ".3", ".4")){
  cur_mean <- svymean(as.formula(paste0("~GISE", suffix)), survey_imp)[1]
  cur_sd <- sqrt(svyvar(as.formula(paste0("~GISE", suffix)), survey_imp)[1])
  dat_imp[[paste0("GISE_scaled", suffix)]] <- (dat_imp[[paste0("GISE", suffix)]] - cur_mean)/cur_sd

  cur_mean <- svymean(as.formula(paste0("~GIPSE", suffix)), survey_imp)[1]
  cur_sd <- sqrt(svyvar(as.formula(paste0("~GIPSE", suffix)), survey_imp)[1])
  dat_imp[[paste0("GIPSE_scaled", suffix)]] <- (dat_imp[[paste0("GIPSE", suffix)]] - cur_mean)/cur_sd
}


# recompute the survey objects:
dat <- merge(dat_miss, 
             dat_no_indices[,c("ID", "STRAT" ,"PSU_ID", "WEIGHT_FINAL_NORM_OVERALL")], 
             by = c("ID", "STRAT" ,"PSU_ID", "WEIGHT_FINAL_NORM_OVERALL"), all = TRUE)

survey_cc <- svydesign(id = ~PSU_ID,
                          strata = ~STRAT,
                          weights = ~WEIGHT_FINAL_NORM_OVERALL,
                          nest = TRUE,
                          data = dat)
survey_cc <- subset(survey_cc, complete.cases(dat))

survey_imp <- svydesign(id = ~PSU_ID,
                          strata = ~STRAT,
                          weights = ~WEIGHT_FINAL_NORM_OVERALL,
                          nest = TRUE,
                          data = dat_imp)
```
# Prepare model variables


```r
model1 <- c("Age", "Center", "Background", "Sex")
model2 <- c("Age", "Center", "Background", "Sex", "GISE_scaled")
model3 <- c("Age", "Center", "Background", "Sex", "GIPSE_scaled")

# models 4 and 5 include components of the primary and secondary indices:
model4 <- c(model1, "Marital_status",
                    "Income_level", 
                    "Employment_status", 
                    "Occupation", 
                    "Education",
                    "Current_Health_insurance", 
                    "Language_pref", 
                    "Language_acculturation", 
                    "Social_acculturation", 
                    "Ethnic_identity_score", 
                    "Years_in_US")
model5 <- c(model4, c("CESD9", "STAI10"))

# for stratified model
model2_sex_strat <- setdiff(model2,  "Sex")
model3_sex_strat <-  setdiff(model3,  "Sex")
```

# prepare table 4: evaluate models

```r
# function to extract the estimated effect of an exposure on a binary outcome
extract_one_exp <- function(mod, exposure, round_digit = 2){
   est <- summary(mod)$coef
   confint <- confint(mod)
  
  ind <- grep(exposure, rownames(est))
  
  df_out <- data.frame(exposure = exposure, 
                       est = round(exp(est[ind, "Estimate"]),round_digit), 
                       CI = paste0("(", round(exp(confint[ind, 1]), round_digit),
                                   ",",round(exp(confint[ind, 2]), round_digit) , ")"),
                       pval = formatC(est[ind, "Pr(>|t|)"], digits = round_digit, format = "E"), 
                       paper_pval= format_pvalue(est[ind, "Pr(>|t|)"]))
 

  return(df_out)
}



table5 <- data.frame(row.names = c(paste0("all_model_",1:5 ),
                                   paste0("male_model_", 2:3),
                                   paste0("female_model_", 2:3)),
                     male_eff = rep(NA, 9),
                     male_CI = NA,
                     male_pval = NA,
                     male_paper_pval = NA,
                     index_eff = NA,
                     index_CI = NA,
                     index_pval = NA,
                     index_paper_pval = NA)

# fit models (sex combined)
model1_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model1, collapse = "+"))), design = survey_cc, family = quasibinomial())

model2_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model2, collapse = "+"))), design = survey_cc, family = quasibinomial())

model3_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model3, collapse = "+"))), design = survey_cc, family = quasibinomial())

model4_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model4, collapse = "+"))), design = survey_cc, family = quasibinomial())

model5_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model5, collapse = "+"))), design = survey_cc, family = quasibinomial())

for (i in 1:5){
  table5[paste0("all_model_", i), 
         c("male_eff", "male_CI", "male_pval", "male_paper_pval")] <- 
             extract_one_exp(get(paste0("model", i, "_fit")) , 
                                 exposure = "Sex")[c("est", "CI", "pval", "paper_pval")]
}

table5["all_model_2",
       c("index_eff", "index_CI",  "index_pval", "index_paper_pval")]  <- 
          extract_one_exp(get(paste0("model2_fit")) , 
                                 exposure = "GISE_scaled")[c("est", "CI", "pval","paper_pval")]

table5["all_model_3",
       c("index_eff", "index_CI", "index_pval", "index_paper_pval")]  <- 
          extract_one_exp(get(paste0("model3_fit")) , 
                                 exposure = "GIPSE_scaled")[c("est", "CI", "pval","paper_pval")]


survey_cc_male <- subset(survey_cc, Sex == "Male")
survey_cc_female <- subset(survey_cc, Sex == "Female")

model2_male_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model2_sex_strat, collapse = "+"))), design = survey_cc_male, family = quasibinomial())

model3_male_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model3_sex_strat, collapse = "+"))), design = survey_cc_male, family = quasibinomial())

model2_female_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model2_sex_strat, collapse = "+"))), design = survey_cc_female, family = quasibinomial())

model3_female_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(model3_sex_strat, collapse = "+"))), design = survey_cc_female, family = quasibinomial())

for (i in 2:3){
  for (sex in c("male", "female")){
    type <- ifelse(i == 2, "GISE", "GIPSE")
      table5[paste0(sex, "_model_", i), 
         c("index_eff", "index_CI", "index_pval", "index_paper_pval")]  <- 
          extract_one_exp(get(paste0("model", i, "_", sex, "_fit")) , 
                                 exposure = paste0(type, "_scaled"))[c("est", "CI", "pval","paper_pval")]
    
  }

}

table5
```

```
##                male_eff     male_CI male_pval male_paper_pval index_eff
## all_model_1        0.60 (0.53,0.67)  1.05E-16       <0.001***        NA
## all_model_2        0.63  (0.56,0.7)  3.90E-14       <0.001***      0.92
## all_model_3        0.78 (0.69,0.88)  7.09E-05       <0.001***      0.65
## all_model_4        0.62  (0.55,0.7)  3.45E-14       <0.001***        NA
## all_model_5        0.73 (0.64,0.84)  3.59E-06       <0.001***        NA
## male_model_2         NA        <NA>      <NA>            <NA>      0.93
## male_model_3         NA        <NA>      <NA>            <NA>      0.68
## female_model_2       NA        <NA>      <NA>            <NA>      0.92
## female_model_3       NA        <NA>      <NA>            <NA>      0.63
##                   index_CI index_pval index_paper_pval
## all_model_1           <NA>       <NA>             <NA>
## all_model_2    (0.87,0.99)   1.72E-02           0.017*
## all_model_3     (0.61,0.7)   2.87E-34        <0.001***
## all_model_4           <NA>       <NA>             <NA>
## all_model_5           <NA>       <NA>             <NA>
## male_model_2   (0.85,1.03)   1.58E-01            0.158
## male_model_3   (0.62,0.75)   9.93E-14        <0.001***
## female_model_2 (0.84,0.99)   3.42E-02           0.034*
## female_model_3 (0.58,0.69)   8.53E-25        <0.001***
```

```r
write.csv(table5, file = file.path(folder_path, "Results/table5.csv"))
```


# Table 4 using imputed dataset (Supplementary Table 1)

Functions that will help us

```r
# can't use the pool function from the MI package
# instead write a function that fit a model on the 5 datasets, and applies Rubin's rule.
# for simplicity, assume insomnia is outcome, assume 5 imputations

rubins_rule <- function(ests, ses, round_digit = 2){
  vars <- ses^2
  m <- length(ses)
  
  pooled_est <- mean(ests)
  within_imp_var <- mean(vars)
  between_imp_var <- sum((vars - within_imp_var)^2)/(m -1)
  
  pooled_var <- within_imp_var + (1+1/m)*between_imp_var
  pooled_se <- sqrt(pooled_var)
  
  F_test_stat <- pooled_est^2/pooled_var
  F_df1 <- 1
  r <- (1 + 1/m)*between_imp_var/pooled_est
  F_df2 <- (m - 1)*(1 + 1/r)^2
  
  F_pval <- pf(F_test_stat, F_df1, F_df2, lower.tail = FALSE)
  
  CI <- paste0("(", round(exp(pooled_est - 1.96*pooled_se), round_digit), ",", round(exp(pooled_est + 1.96*pooled_se), round_digit), ")")
  
  return(data.frame(est = round(exp(pooled_est), round_digit), CI = CI, pval = F_pval))
  
}


extract_one_exp_mi <- function(survey_imp,  model_vars, exposure, 
                               round_digit = 2){
  
  imp_suffix <-  c("", ".1", ".2", ".3", ".4")
  est <- se <- rep(NA, 5)
  names(est) <- names(se) <-  imp_suffix
  for (i in 1:length(imp_suffix)){
    suffix <- imp_suffix[i]
    cur_fit <- svyglm(formula = as.formula(paste0("Insomnia", suffix, "~", paste(paste0(model_vars, suffix), collapse = "+"))), design = survey_imp, family = quasibinomial())
    
    fit_est <- summary(cur_fit)$coef
    ind_exp <- grep(exposure, rownames(fit_est))
    est[i] <- fit_est[ind_exp, "Estimate"]
    se[i] <- fit_est[ind_exp, "Std. Error"]
    
  }
  
  # now apply Rubin's rule
  
  res <- rubins_rule(est, se, round_digit = round_digit)

  
  df_out <- data.frame(exposure = exposure, 
                       est = res[["est"]], 
                       CI = res[["CI"]], 
                       pval = formatC(res[["pval"]], digits = round_digit, format = "E"), 
                       paper_pval= format_pvalue(res[["pval"]]))
 

  return(df_out)
}
```




And prepare the table using imputed data:

```r
table5_imp <- data.frame(row.names = c(paste0("all_model_",1:5 ),
                                   paste0("male_model_", 2:3),
                                   paste0("female_model_", 2:3)),
                     male_eff = rep(NA, 9),
                     male_95_CI = NA,
                     male_pval = NA,
                     male_paper_pval = NA,
                     index_eff = NA,
                     index_95_CI = NA,
                     index_pval = NA,
                     index_paper_pval = NA)


table5_imp["all_model_1", c("male_eff", "male_95_CI", "male_pval", "male_paper_pval")] <- extract_one_exp_mi(survey_imp, model1, "Sex")[, c("est", "CI", "pval", "paper_pval")]

table5_imp["all_model_2", c("male_eff", "male_95_CI", "male_pval", "male_paper_pval")] <- extract_one_exp_mi(survey_imp, model2, "Sex")[, c("est", "CI", "pval", "paper_pval")]

table5_imp["all_model_2", c("index_eff", "index_95_CI", "index_pval", "index_paper_pval")] <- extract_one_exp_mi(survey_imp, model2, "GISE_scaled")[, c("est", "CI", "pval", "paper_pval")]

table5_imp["all_model_3", c("male_eff", "male_95_CI", "male_pval", "male_paper_pval")] <- extract_one_exp_mi(survey_imp, model3, "Sex")[, c("est", "CI", "pval", "paper_pval")]

table5_imp["all_model_3", c("index_eff", "index_95_CI", "index_pval", "index_paper_pval")] <- extract_one_exp_mi(survey_imp, model3, "GIPSE_scaled")[, c("est", "CI", "pval", "paper_pval")]


table5_imp["all_model_4", c("male_eff", "male_95_CI", "male_pval", "male_paper_pval")] <- extract_one_exp_mi(survey_imp, model4, "Sex")[, c("est", "CI", "pval", "paper_pval")]

table5_imp["all_model_5", c("male_eff", "male_95_CI", "male_pval", "male_paper_pval")] <- extract_one_exp_mi(survey_imp, model5, "Sex")[, c("est", "CI", "pval", "paper_pval")]



survey_imp_male <- subset(survey_imp, Sex == "Male")
survey_imp_female <- subset(survey_imp, Sex == "Female")



table5_imp["male_model_2", c("index_eff", "index_95_CI", "index_pval", "index_paper_pval")] <- extract_one_exp_mi(survey_imp_male, setdiff(model2, "Sex"), "GISE_scaled")[, c("est", "CI", "pval" ,"paper_pval")]


table5_imp["male_model_3", c("index_eff", "index_95_CI", "index_pval", "index_paper_pval")] <- extract_one_exp_mi(survey_imp_male, setdiff(model3, "Sex"), "GIPSE_scaled")[, c("est", "CI", "pval" ,"paper_pval")]


table5_imp["female_model_2", c("index_eff", "index_95_CI", "index_pval", "index_paper_pval")] <- extract_one_exp_mi(survey_imp_female, setdiff(model2, "Sex"), "GISE_scaled")[, c("est", "CI", "pval" ,"paper_pval")]


table5_imp["female_model_3", c("index_eff", "index_95_CI", "index_pval", "index_paper_pval")] <- extract_one_exp_mi(survey_imp_female, setdiff(model3, "Sex"), "GIPSE_scaled")[, c("est", "CI", "pval" ,"paper_pval")]

table5_imp
```

```
##                male_eff  male_95_CI male_pval male_paper_pval index_eff
## all_model_1        0.61 (0.55,0.68)  7.32E-20       <0.001***        NA
## all_model_2        0.63 (0.57,0.71)  1.71E-16       <0.001***      0.93
## all_model_3        0.79 (0.71,0.89)  4.85E-05       <0.001***      0.65
## all_model_4        0.63  (0.56,0.7)  1.45E-16       <0.001***        NA
## all_model_5        0.75 (0.66,0.84)  2.15E-06       <0.001***        NA
## male_model_2         NA        <NA>      <NA>            <NA>      0.94
## male_model_3         NA        <NA>      <NA>            <NA>      0.68
## female_model_2       NA        <NA>      <NA>            <NA>      0.92
## female_model_3       NA        <NA>      <NA>            <NA>      0.63
##                index_95_CI index_pval index_paper_pval
## all_model_1           <NA>       <NA>             <NA>
## all_model_2    (0.88,0.99)   1.62E-02           0.016*
## all_model_3    (0.61,0.69)   3.35E-46        <0.001***
## all_model_4           <NA>       <NA>             <NA>
## all_model_5           <NA>       <NA>             <NA>
## male_model_2   (0.86,1.03)   1.94E-01            0.194
## male_model_3   (0.62,0.75)   3.82E-16        <0.001***
## female_model_2 (0.85,0.99)   3.03E-02           0.030*
## female_model_3 (0.58,0.68)   4.13E-33        <0.001***
```

```r
write.csv(table5_imp, file = file.path(folder_path, "Results/Supp_table1.csv"))
```


```r
sessionInfo()
```

```
## R version 4.2.3 (2023-03-15)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Ventura 13.6
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] reshape2_1.4.4     jtools_2.2.2       RColorBrewer_1.1-3 naniar_1.0.0      
##  [5] UpSetR_1.4.0       glmnet_4.1-7       boot_1.3-28.1      sjlabelled_1.2.0  
##  [9] memisc_0.99.31.6   MASS_7.3-60        lattice_0.21-8     labelled_2.12.0   
## [13] factoextra_1.0.7   plyr_1.8.8         survey_4.2-1       survival_3.5-5    
## [17] Matrix_1.5-4.1     lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0     
## [21] dplyr_1.1.2        purrr_1.0.1        readr_2.1.4        tidyr_1.3.0       
## [25] tibble_3.2.1       ggplot2_3.4.2      tidyverse_2.0.0   
## 
## loaded via a namespace (and not attached):
##  [1] ggrepel_0.9.3     Rcpp_1.0.11       foreach_1.5.2     digest_0.6.33    
##  [5] utf8_1.2.3        R6_2.5.1          visdat_0.6.0      evaluate_0.21    
##  [9] pillar_1.9.0      rlang_1.1.1       rstudioapi_0.15.0 data.table_1.14.8
## [13] car_3.1-2         jquerylib_0.1.4   rmarkdown_2.23    splines_4.2.3    
## [17] pander_0.6.5      munsell_0.5.0     compiler_4.2.3    xfun_0.39        
## [21] pkgconfig_2.0.3   shape_1.4.6       htmltools_0.5.5   mitools_2.4      
## [25] insight_0.19.3    tidyselect_1.2.0  gridExtra_2.3     codetools_0.2-19 
## [29] fansi_1.0.4       crayon_1.5.2      tzdb_0.4.0        withr_2.5.0      
## [33] jsonlite_1.8.7    gtable_0.3.3      lifecycle_1.0.3   DBI_1.1.3        
## [37] magrittr_2.0.3    scales_1.2.1      cli_3.6.1         stringi_1.7.12   
## [41] cachem_1.0.8      carData_3.0-5     bslib_0.5.0       generics_0.1.3   
## [45] vctrs_0.6.3       iterators_1.0.14  tools_4.2.3       glue_1.6.2       
## [49] hms_1.1.3         abind_1.4-5       fastmap_1.1.1     yaml_2.3.7       
## [53] timechange_0.2.0  colorspace_2.1-0  knitr_1.43        haven_2.5.3      
## [57] sass_0.4.7
```
