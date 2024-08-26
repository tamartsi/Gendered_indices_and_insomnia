---
title: "Prepare a table providing associations of sociodemographic variables with insomnia by sex"
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
library(tableone)
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
library(mi)
```

```
## Loading required package: stats4
## mi (Version 1.1, packaged: 2022-06-05 05:31:15 UTC; ben)
## mi  Copyright (C) 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015 Trustees of Columbia University
## This program comes with ABSOLUTELY NO WARRANTY.
## This is free software, and you are welcome to redistribute it
## under the General Public License version 2 or later.
## Execute RShowDoc('COPYING') for details.
## 
## Attaching package: 'mi'
## 
## The following object is masked from 'package:tidyr':
## 
##     complete
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
library(svyVGAM)
```

```
## Loading required package: VGAM
## Loading required package: splines
## 
## Attaching package: 'VGAM'
## 
## The following objects are masked from 'package:boot':
## 
##     logit, simplex
## 
## The following object is masked from 'package:mi':
## 
##     multinomial
## 
## The following object is masked from 'package:memisc':
## 
##     Max
## 
## The following object is masked from 'package:survey':
## 
##     calibrate
```

```r
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

# Read in the data


```r
folder_path <- "/Users/tamarsofer/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/Ongoing_papers/2022_gendered_indices"
data_with_sleep <- read_csv(file.path(folder_path, "Data/sol_sofer_sorajja_joon_covariates_20220810.csv"))
```

```
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```
## Rows: 16415 Columns: 44
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (8): ID, CENTER, GENDER, BKGRD1_C7, SLEA7, WBEA1, WBEA5, WBEA10
## dbl (36): STRAT, PSU_ID, WEIGHT_FINAL_NORM_OVERALL, AGE, US_BORN, BMI, STAI1...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# load the prepared dataset
dat <- readRDS(file.path(folder_path, "Data/Data_with_miss.Rds"))
```

# Prepare for the analysis


```r
# survey object
survey_base <- svydesign(id = ~PSU_ID,
                          strata = ~STRAT,
                          weights = ~WEIGHT_FINAL_NORM_OVERALL,
                          nest = TRUE,
                          data = dat)
sleepDesign <- subset(survey_base, complete.cases(dat))

sleepDesign_female <- subset(sleepDesign, Sex == "Female")
sleepDesign_male <- subset(sleepDesign, Sex == "Male")

# baseline covariates to adjust for
adj_vars <- c("Age", "Center", "Background")

# multi-level variables to be analyzed in multinomial regression
multi_vars <- c("Marital_status", 
                "Occupation",
                "Income_level",
                "Employment_status", 
                "Education", 
                "Years_in_US")
# not multi-level ones:
single_vars  <- c("Language_pref",
                 "Current_Health_insurance",
                 "Language_acculturation",
                  "Social_acculturation",
                  "Ethnic_identity_score", 
                  "STAI10",
                  "CESD")

cont_vars  <- c( "Language_acculturation",
                  "Social_acculturation",
                 "Ethnic_identity_score", 
                  "STAI10",
                  "CESD9")

binary_vars  <- c("Language_pref",
                 "Current_Health_insurance")
```




# functions to extract and summarize results when insomnia is outcome



```r
extract_mult_exp <- function(mod, exposure, level_names, round_digit = 2, insomnia = TRUE){
  est <- summary(mod)$coef
  confint <- confint(mod)
  
  inds <- grep(exposure, rownames(est))
  
  if (insomnia == TRUE){
   df_out <- data.frame(exposure = paste0(exposure,": ", level_names), 
                       est = round(exp(est[inds, "Estimate"]),round_digit), 
                        CI = paste0("(", round(exp(confint[inds, 1]), round_digit), ",",
                                   round(exp(confint[inds, 2]), round_digit), ")"),
                       pval = formatC(est[inds, "Pr(>|t|)"], digits = round_digit, format = "E"),
                       paper_pval = format_pvalue(est[inds, "Pr(>|t|)"]))
  } else{ # WHIIRS is the outcome, not exponentiating estimates
     df_out <- data.frame(exposure = paste0(exposure,": ", level_names), 
                       est = round(est[inds, "Estimate"],round_digit), 
                        CI = paste0("(", round(confint[inds, 1], round_digit), ",",
                                   round(confint[inds, 2], round_digit), ")"),
                       pval = formatC(est[inds, "Pr(>|t|)"], digits = round_digit, format = "E"), 
                       paper_pval = format_pvalue(est[inds, "Pr(>|t|)"]))
    
  }
  return(df_out)
}


extract_one_exp <- function(mod, exposure, round_digit = 2, insomnia = TRUE){
   est <- summary(mod)$coef
   confint <- confint(mod)
  
  ind <- grep(exposure, rownames(est))
  
  if (insomnia == TRUE){
    df_out <- data.frame(exposure = exposure, 
                       est = round(exp(est[ind, "Estimate"]),round_digit),
                        CI = paste0("(", round(exp(confint[ind, 1]), round_digit), ",",
                                   round(exp(confint[ind, 2]), round_digit), ")"),
                       pval = formatC(est[ind, "Pr(>|t|)"], digits = round_digit, format = "E"),
                       paper_pval = format_pvalue(est[ind, "Pr(>|t|)"]))
    
  } else{  # WHIIRS is the outcome, not exponentiation estimates
    df_out <- data.frame(exposure = exposure, 
                       est = round(est[ind, "Estimate"],round_digit),
                        CI = paste0("(", round(confint[ind, 1], round_digit), ",",
                                   round(confint[ind, 2], round_digit), ")"),
                       pval = formatC(est[ind, "Pr(>|t|)"], digits = round_digit, format = "E"),
                       paper_pval = format_pvalue(est[ind, "Pr(>|t|)"]))
    
  }
  

  return(df_out)
}
```

# Prepare a table of results: insomnia is outcome, by sex strata


```r
female_out <- male_out <- interaction_out <- 
  vector(mode = "list", length = length(multi_vars) + length(single_vars))
ind <- 1

for (var in multi_vars){
   mod <- svyglm(formula = as.formula(paste0("Insomnia~Age+Center+Background+", var)), 
                 design = sleepDesign_female, family = quasibinomial())
   

  female_out[[ind]] <- extract_mult_exp(mod, 
                                     exposure = var, 
                                     level_names  = levels(dat[[var]])[-1])
  
    mod <- svyglm(formula = as.formula(paste0("Insomnia~Age+Center+Background+", var)), 
                 design = sleepDesign_male, family = quasibinomial())
   

  male_out[[ind]] <- extract_mult_exp(mod, 
                                     exposure = var, 
                                     level_names  = levels(dat[[var]])[-1])
  
    
  mod <- svyglm(formula = as.formula(paste0("Insomnia~Age+Center+Background+Sex*", var)), 
                 design = sleepDesign, family = quasibinomial())
  
  interaction_out[[ind]] <- extract_mult_exp(mod, 
                                     exposure = paste0("SexMale:",var), 
                                     level_names  = levels(dat[[var]])[-1])
   
      
  
  ind <- ind + 1
}


for (var in c(cont_vars, binary_vars)){
  mod <- svyglm(formula = as.formula(paste0("Insomnia~Age+Center+Background+", var)), 
                design = sleepDesign_female, family = quasibinomial())
  
  female_out[[ind]] <- extract_one_exp(mod, 
                                     exposure = var)
  
  mod <- svyglm(formula = as.formula(paste0("Insomnia~Age+Center+Background+", var)), 
                design = sleepDesign_male, family = quasibinomial())
  
  male_out[[ind]] <- extract_one_exp(mod, 
                                     exposure = var)
  
   mod <- svyglm(formula = as.formula(paste0("Insomnia~Age+Center+Background+Sex*", var)), 
                design = sleepDesign, family = quasibinomial())
  
  interaction_out[[ind]] <- extract_one_exp(mod, 
                                     exposure = paste0("SexMale:",var))
  
  
  ind <- ind + 1
}


insomnia_female_tab <- do.call(rbind, female_out)
rownames(insomnia_female_tab) <- paste0("r", 1:nrow(insomnia_female_tab))

insomnia_male_tab <- do.call(rbind, male_out)
rownames(insomnia_male_tab) <- paste0("r", 1:nrow(insomnia_male_tab))

interaction_tab <- do.call(rbind, interaction_out)
rownames(interaction_tab) <- paste0("r", 1:nrow(interaction_tab))

colnames(insomnia_female_tab) <- c("exposure", "female_est", "female_CI", "female_pval", "female_paper_pval")
colnames(insomnia_male_tab) <- c("exposure", "male_est", "male_CI", "male_pval", "male_paper_pval")
colnames(interaction_tab) <- c("exposure", "interation_est", "interaction_CI", "interaction_pval", "interaction_paper_pval")

combined_tab <- cbind(insomnia_female_tab,
                      insomnia_male_tab[, c("exposure", "male_est", "male_CI", "male_pval","male_paper_pval")],
                      interaction_tab$interaction_pval)

colnames(combined_tab)[grep("interaction_pval", colnames(combined_tab))] <- "Interaction_pval"
colnames(combined_tab)[grep("interaction_paper_pval", colnames(combined_tab))] <- "Interaction_paper_pval"
write.csv(combined_tab, file = file.path(folder_path, "Results/All_vars_assoc_insomnia_by_sex.csv"))

combined_tab
```

```
##                                                                          exposure
## r1                               Marital_status: Married or living with a partner
## r2                                Marital_status: Separated,divorced,or widow(er)
## r3                                                     Occupation: Service Worker
## r4                                                     Occupation: Skilled Worker
## r5  Occupation: Professional/technical, administrative/executive, or office staff
## r6                                                   Occupation: Other occupation
## r7                                                  Income_level: $10,001-$20,000
## r8                                                  Income_level: $20,001-$40,000
## r9                                                  Income_level: $40,001-$75,000
## r10                                               Income_level: More than $75,000
## r11                        Employment_status: Employed part-time(<=35 hours/week)
## r12                         Employment_status: Employed full-time(>35 hours/week)
## r13                                    Education: At most high school diploma/GED
## r14                                       Education: Greater than high school/GED
## r15                                                 Years_in_US: 10 Years or More
## r16                                                          Years_in_US: US born
## r17                                                        Language_acculturation
## r18                                                          Social_acculturation
## r19                                                         Ethnic_identity_score
## r20                                                                        STAI10
## r21                                                                         CESD9
## r22                                                                 Language_pref
## r23                                                      Current_Health_insurance
##     female_est   female_CI female_pval female_paper_pval
## r1        0.89 (0.74,1.08)    2.50E-01             0.250
## r2        1.10 (0.88,1.37)    4.15E-01             0.415
## r3        1.14 (0.87,1.49)    3.40E-01             0.340
## r4        0.97 (0.75,1.26)    8.33E-01             0.833
## r5        1.04 (0.82,1.32)    7.52E-01             0.752
## r6        1.08 (0.84,1.38)    5.58E-01             0.558
## r7        0.99  (0.82,1.2)    8.97E-01             0.897
## r8        0.85 (0.68,1.06)    1.45E-01             0.145
## r9        0.73 (0.53,0.99)    4.10E-02            0.041*
## r10       0.51 (0.33,0.78)    1.94E-03           0.002**
## r11       0.79 (0.65,0.96)    1.86E-02            0.019*
## r12       0.71  (0.6,0.84)    5.62E-05         <0.001***
## r13       0.94 (0.75,1.18)    5.88E-01             0.588
## r14       0.80 (0.67,0.96)    1.58E-02            0.016*
## r15       1.13 (0.94,1.36)    1.90E-01             0.190
## r16       1.47 (1.13,1.91)    3.64E-03           0.004**
## r17       1.12 (1.02,1.21)    1.23E-02            0.012*
## r18       1.05 (0.91,1.22)    4.94E-01             0.494
## r19       0.98 (0.86,1.12)    7.97E-01             0.797
## r20       1.12  (1.1,1.13)    4.73E-47         <0.001***
## r21       1.13 (1.11,1.15)    4.39E-45         <0.001***
## r22       1.21  (0.97,1.5)    8.59E-02             0.086
## r23       0.84     (0.7,1)    4.70E-02            0.047*
##                                                                          exposure
## r1                               Marital_status: Married or living with a partner
## r2                                Marital_status: Separated,divorced,or widow(er)
## r3                                                     Occupation: Service Worker
## r4                                                     Occupation: Skilled Worker
## r5  Occupation: Professional/technical, administrative/executive, or office staff
## r6                                                   Occupation: Other occupation
## r7                                                  Income_level: $10,001-$20,000
## r8                                                  Income_level: $20,001-$40,000
## r9                                                  Income_level: $40,001-$75,000
## r10                                               Income_level: More than $75,000
## r11                        Employment_status: Employed part-time(<=35 hours/week)
## r12                         Employment_status: Employed full-time(>35 hours/week)
## r13                                    Education: At most high school diploma/GED
## r14                                       Education: Greater than high school/GED
## r15                                                 Years_in_US: 10 Years or More
## r16                                                          Years_in_US: US born
## r17                                                        Language_acculturation
## r18                                                          Social_acculturation
## r19                                                         Ethnic_identity_score
## r20                                                                        STAI10
## r21                                                                         CESD9
## r22                                                                 Language_pref
## r23                                                      Current_Health_insurance
##     male_est     male_CI male_pval male_paper_pval Interaction_pval
## r1      0.74 (0.58,0.93)  9.13E-03         0.009**         1.46E-01
## r2      0.84 (0.63,1.12)  2.29E-01           0.229         8.38E-02
## r3      1.01 (0.74,1.39)  9.45E-01           0.945         6.65E-01
## r4      1.14 (0.92,1.43)  2.26E-01           0.226         4.37E-01
## r5      0.93 (0.67,1.28)  6.39E-01           0.639         5.16E-01
## r6      1.13 (0.87,1.46)  3.55E-01           0.355         7.53E-01
## r7      0.70 (0.54,0.92)  1.11E-02          0.011*         5.16E-02
## r8      0.61  (0.46,0.8)  5.18E-04       <0.001***         6.82E-02
## r9      0.65  (0.47,0.9)  1.06E-02          0.011*         6.51E-01
## r10     0.50 (0.28,0.89)  1.90E-02          0.019*         9.23E-01
## r11     0.77 (0.59,1.01)  5.92E-02           0.059         9.45E-01
## r12     0.66 (0.54,0.81)  6.27E-05       <0.001***         7.61E-01
## r13     1.10 (0.87,1.39)  4.26E-01           0.426         2.62E-01
## r14     1.07 (0.86,1.35)  5.31E-01           0.531         5.01E-02
## r15     1.12 (0.89,1.42)  3.28E-01           0.328         6.57E-01
## r16     1.54 (1.13,2.11)  7.14E-03         0.007**         7.61E-01
## r17     1.15 (1.04,1.27)  7.65E-03         0.008**         3.75E-01
## r18     0.97 (0.82,1.16)  7.72E-01           0.772         5.91E-01
## r19     0.97 (0.83,1.14)  7.30E-01           0.730         8.69E-01
## r20     1.15 (1.13,1.17)  4.91E-44       <0.001***         7.01E-03
## r21     1.13  (1.1,1.15)  8.40E-27       <0.001***         7.05E-01
## r22     1.43 (1.11,1.84)  5.08E-03         0.005**         1.88E-01
## r23     0.90 (0.74,1.09)  2.74E-01           0.274         9.09E-01
```



# The same analysis with WHIIRS as the outcome



```r
female_out <- male_out <- interaction_out <- 
  vector(mode = "list", length = length(multi_vars) + length(single_vars))
ind <- 1

for (var in multi_vars){
   mod <- svyglm(formula = as.formula(paste0("WHIIRS~Age+Center+Background+", var)), 
                 design = sleepDesign_female, family = gaussian())
   

  female_out[[ind]] <- extract_mult_exp(mod, 
                                     exposure = var, 
                                     level_names  = levels(dat[[var]])[-1])
  
    mod <- svyglm(formula = as.formula(paste0("WHIIRS~Age+Center+Background+", var)), 
                 design = sleepDesign_male, family = gaussian())
   

  male_out[[ind]] <- extract_mult_exp(mod, 
                                     exposure = var, 
                                     level_names  = levels(dat[[var]])[-1])
  
    
  mod <- svyglm(formula = as.formula(paste0("WHIIRS~Age+Center+Background+Sex*", var)), 
                 design = sleepDesign, family = gaussian())
  
  interaction_out[[ind]] <- extract_mult_exp(mod, 
                                     exposure = paste0("SexMale:",var), 
                                     level_names  = levels(dat[[var]])[-1])
   
      
  
  ind <- ind + 1
}


for (var in c(cont_vars, binary_vars)){
  mod <- svyglm(formula = as.formula(paste0("WHIIRS~Age+Center+Background+", var)), 
                design = sleepDesign_female, family = gaussian())
  
  female_out[[ind]] <- extract_one_exp(mod, 
                                     exposure = var)
  
  mod <- svyglm(formula = as.formula(paste0("WHIIRS~Age+Center+Background+", var)), 
                design = sleepDesign_male, family = gaussian())
  
  male_out[[ind]] <- extract_one_exp(mod, 
                                     exposure = var)
  
   mod <- svyglm(formula = as.formula(paste0("WHIIRS~Age+Center+Background+Sex*", var)), 
                design = sleepDesign, family = gaussian())
  
  interaction_out[[ind]] <- extract_one_exp(mod, 
                                     exposure = paste0("SexMale:",var))
  
  
  ind <- ind + 1
}


whiirs_female_tab <- do.call(rbind, female_out)
rownames(whiirs_female_tab) <- paste0("r", 1:nrow(whiirs_female_tab))

whiirs_male_tab <- do.call(rbind, male_out)
rownames(whiirs_male_tab) <- paste0("r", 1:nrow(whiirs_male_tab))

interaction_tab <- do.call(rbind, interaction_out)
rownames(interaction_tab) <- paste0("r", 1:nrow(interaction_tab))

colnames(whiirs_female_tab) <- c("exposure", "female_est", "female_CI", "female_pval", "female_paper_pval")
colnames(whiirs_male_tab) <- c("exposure", "male_est", "male_CI", "male_pval", "male_paper_pval")
colnames(interaction_tab) <- c("exposure", "interation_est", "interaction_CI", "interaction_pval", "interaction_paper_pval")

combined_tab <- cbind(whiirs_female_tab,
                      whiirs_male_tab[, c("exposure", "male_est", "male_CI", "male_pval", "male_paper_pval")],
                      interaction_tab$interaction_pval)

colnames(combined_tab)[grep("interaction_pval", colnames(combined_tab))] <- "Interaction_pval"
colnames(combined_tab)[grep("interaction_paper_pval", colnames(combined_tab))] <- "Interaction_paper_pval"

write.csv(combined_tab, file = file.path(folder_path, "Results/All_vars_assoc_WHIIRS_by_sex.csv"))

combined_tab
```

```
##                                                                          exposure
## r1                               Marital_status: Married or living with a partner
## r2                                Marital_status: Separated,divorced,or widow(er)
## r3                                                     Occupation: Service Worker
## r4                                                     Occupation: Skilled Worker
## r5  Occupation: Professional/technical, administrative/executive, or office staff
## r6                                                   Occupation: Other occupation
## r7                                                  Income_level: $10,001-$20,000
## r8                                                  Income_level: $20,001-$40,000
## r9                                                  Income_level: $40,001-$75,000
## r10                                               Income_level: More than $75,000
## r11                        Employment_status: Employed part-time(<=35 hours/week)
## r12                         Employment_status: Employed full-time(>35 hours/week)
## r13                                    Education: At most high school diploma/GED
## r14                                       Education: Greater than high school/GED
## r15                                                 Years_in_US: 10 Years or More
## r16                                                          Years_in_US: US born
## r17                                                        Language_acculturation
## r18                                                          Social_acculturation
## r19                                                         Ethnic_identity_score
## r20                                                                        STAI10
## r21                                                                         CESD9
## r22                                                                 Language_pref
## r23                                                      Current_Health_insurance
##     female_est   female_CI female_pval female_paper_pval
## r1        0.67 (0.43,1.04)    7.31E-02             0.073
## r2        1.48 (0.85,2.58)    1.65E-01             0.165
## r3        1.12 (0.61,2.04)    7.14E-01             0.714
## r4        0.98 (0.58,1.65)    9.34E-01             0.934
## r5        1.01  (0.59,1.7)    9.85E-01             0.985
## r6        1.21 (0.67,2.18)    5.26E-01             0.526
## r7        0.62    (0.38,1)    4.78E-02            0.048*
## r8        0.38 (0.22,0.65)    4.93E-04         <0.001***
## r9        0.31 (0.17,0.59)    3.85E-04         <0.001***
## r10       0.16 (0.06,0.41)    1.34E-04         <0.001***
## r11       0.54 (0.34,0.84)    6.68E-03           0.007**
## r12       0.38 (0.25,0.57)    3.15E-06         <0.001***
## r13       0.76 (0.46,1.26)    2.90E-01             0.290
## r14       0.49 (0.31,0.75)    1.18E-03           0.001**
## r15       1.27 (0.85,1.92)    2.48E-01             0.248
## r16       2.77 (1.57,4.86)    4.20E-04         <0.001***
## r17       1.43 (1.19,1.71)    1.35E-04         <0.001***
## r18       1.11 (0.78,1.57)    5.79E-01             0.579
## r19       0.90 (0.66,1.24)    5.24E-01             0.524
## r20       1.38 (1.34,1.41)    8.49E-89         <0.001***
## r21       1.43 (1.39,1.48)    2.02E-81         <0.001***
## r22       1.78 (1.13,2.82)    1.33E-02            0.013*
## r23       0.71 (0.48,1.04)    7.77E-02             0.078
##                                                                          exposure
## r1                               Marital_status: Married or living with a partner
## r2                                Marital_status: Separated,divorced,or widow(er)
## r3                                                     Occupation: Service Worker
## r4                                                     Occupation: Skilled Worker
## r5  Occupation: Professional/technical, administrative/executive, or office staff
## r6                                                   Occupation: Other occupation
## r7                                                  Income_level: $10,001-$20,000
## r8                                                  Income_level: $20,001-$40,000
## r9                                                  Income_level: $40,001-$75,000
## r10                                               Income_level: More than $75,000
## r11                        Employment_status: Employed part-time(<=35 hours/week)
## r12                         Employment_status: Employed full-time(>35 hours/week)
## r13                                    Education: At most high school diploma/GED
## r14                                       Education: Greater than high school/GED
## r15                                                 Years_in_US: 10 Years or More
## r16                                                          Years_in_US: US born
## r17                                                        Language_acculturation
## r18                                                          Social_acculturation
## r19                                                         Ethnic_identity_score
## r20                                                                        STAI10
## r21                                                                         CESD9
## r22                                                                 Language_pref
## r23                                                      Current_Health_insurance
##     male_est     male_CI male_pval male_paper_pval Interaction_pval
## r1      0.50 (0.31,0.81)  4.51E-03         0.005**         4.79E-01
## r2      0.84  (0.44,1.6)  6.03E-01           0.603         8.62E-02
## r3      1.61 (0.82,3.15)  1.67E-01           0.167         4.18E-01
## r4      1.62 (1.05,2.51)  2.97E-02          0.030*         2.56E-01
## r5      1.06 (0.57,1.97)  8.42E-01           0.842         8.95E-01
## r6      1.16 (0.71,1.91)  5.51E-01           0.551         8.49E-01
## r7      0.33 (0.16,0.66)  1.83E-03         0.002**         1.83E-01
## r8      0.22 (0.11,0.45)  4.30E-05       <0.001***         3.82E-01
## r9      0.27 (0.12,0.57)  6.76E-04       <0.001***         9.41E-01
## r10     0.18 (0.07,0.46)  4.16E-04       <0.001***         6.36E-01
## r11     0.41 (0.24,0.69)  7.73E-04       <0.001***         7.86E-01
## r12     0.30  (0.2,0.45)  7.39E-09       <0.001***         7.28E-01
## r13     0.97  (0.59,1.6)  9.12E-01           0.912         2.71E-01
## r14     1.01 (0.63,1.62)  9.72E-01           0.972         1.22E-02
## r15     1.30 (0.83,2.04)  2.58E-01           0.258         4.98E-01
## r16     4.21 (2.36,7.48)  1.25E-06       <0.001***         3.38E-01
## r17     1.60 (1.33,1.93)  8.85E-07       <0.001***         3.00E-01
## r18     1.28 (0.94,1.74)  1.14E-01           0.114         5.51E-01
## r19     0.92 (0.68,1.24)  5.64E-01           0.564         9.35E-01
## r20     1.44 (1.39,1.49)  6.27E-68       <0.001***         4.17E-02
## r21     1.39 (1.34,1.45)  3.01E-49       <0.001***         1.35E-01
## r22     2.84 (1.74,4.64)  3.25E-05       <0.001***         2.07E-01
## r23     0.83 (0.57,1.22)  3.53E-01           0.353         6.80E-01
```






```r
sessionInfo()
```

```
## R version 4.2.3 (2023-03-15)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS 14.6.1
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] splines   stats4    grid      stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] reshape2_1.4.4     jtools_2.2.2       svyVGAM_1.2        VGAM_1.1-9        
##  [5] RColorBrewer_1.1-3 naniar_1.0.0       UpSetR_1.4.0       glmnet_4.1-7      
##  [9] boot_1.3-28.1      mi_1.1             sjlabelled_1.2.0   memisc_0.99.31.6  
## [13] MASS_7.3-60        lattice_0.21-8     tableone_0.13.2    labelled_2.12.0   
## [17] factoextra_1.0.7   plyr_1.8.8         survey_4.2-1       survival_3.5-5    
## [21] Matrix_1.5-4.1     lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0     
## [25] dplyr_1.1.2        purrr_1.0.1        readr_2.1.4        tidyr_1.3.0       
## [29] tibble_3.2.1       ggplot2_3.4.2      tidyverse_2.0.0   
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-162      bit64_4.0.5       insight_0.19.3    tools_4.2.3      
##  [5] bslib_0.7.0       utf8_1.2.3        R6_2.5.1          DBI_1.1.3        
##  [9] colorspace_2.1-0  withr_2.5.0       tidyselect_1.2.0  gridExtra_2.3    
## [13] bit_4.0.5         compiler_4.2.3    cli_3.6.1         sass_0.4.9       
## [17] scales_1.2.1      digest_0.6.33     minqa_1.2.5       rmarkdown_2.27   
## [21] pkgconfig_2.0.3   htmltools_0.5.8.1 lme4_1.1-34       fastmap_1.1.1    
## [25] rlang_1.1.1       rstudioapi_0.15.0 shape_1.4.6       jquerylib_0.1.4  
## [29] generics_0.1.3    jsonlite_1.8.7    vroom_1.6.3       car_3.1-2        
## [33] magrittr_2.0.3    Rcpp_1.0.11       munsell_0.5.0     fansi_1.0.4      
## [37] abind_1.4-5       lifecycle_1.0.3   visdat_0.6.0      stringi_1.7.12   
## [41] yaml_2.3.7        carData_3.0-5     parallel_4.2.3    ggrepel_0.9.3    
## [45] crayon_1.5.2      haven_2.5.3       pander_0.6.5      hms_1.1.3        
## [49] knitr_1.43        pillar_1.9.0      codetools_0.2-19  glue_1.6.2       
## [53] evaluate_0.21     mitools_2.4       data.table_1.14.8 vctrs_0.6.3      
## [57] nloptr_2.0.3      tzdb_0.4.0        foreach_1.5.2     gtable_0.3.3     
## [61] cachem_1.0.8      xfun_0.39         coda_0.19-4       arm_1.13-1       
## [65] iterators_1.0.14  timechange_0.2.0
```
