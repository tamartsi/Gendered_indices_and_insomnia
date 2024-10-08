---
title: "Prepare supplementary table: psuedo R squared from models of sex and gendered indices association with insomnia"
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
We perform this analysis to assess how well sex and gendered indices, and also sociodemographic (and psychological) variables explain insomnia. 

We only focus on complete data here. 

# Load and prepare the dataset


```r
folder_path <- "/Users/tamarsofer/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/Ongoing_papers/2022_gendered_indices"
dat_no_indices <- readRDS(file.path(folder_path, "Data/Data_with_miss.Rds"))

# the data with missing values:
# load the prepared dataset with indices
dat_miss <- readRDS(file.path(folder_path, "Data/Data_complete_case_with_indices.Rds"))



# because of the survey sampling design, we need to "merge" 
# the complete case dataset with design variables of the complete dataset
dat <- merge(dat_miss, 
             dat_no_indices[,c("ID", "STRAT" ,"PSU_ID", "WEIGHT_FINAL_NORM_OVERALL")], 
             by = c("ID", "STRAT" ,"PSU_ID", "WEIGHT_FINAL_NORM_OVERALL"), all = TRUE)
```
# add scaled indices to describe effect per 1 SD increase


```r
survey_cc <- svydesign(id = ~PSU_ID,
                          strata = ~STRAT,
                          weights = ~WEIGHT_FINAL_NORM_OVERALL,
                          nest = TRUE,
                          data = dat)
survey_cc <- subset(survey_cc, complete.cases(dat))


cur_mean <- svymean(~GISE, survey_cc)[1]
cur_sd <- sqrt(svyvar(~GISE, survey_cc)[1])
dat_miss$GISE_scaled <- (dat_miss$GISE - cur_mean)/cur_sd

cur_mean <- svymean(~GIPSE, survey_cc)[1]
cur_sd <- sqrt(svyvar(~GIPSE, survey_cc)[1])
dat_miss$GIPSE_scaled <- (dat_miss$GIPSE - cur_mean)/cur_sd


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

survey_cc_male <- subset(survey_cc, Sex == "Male")
survey_cc_female <- subset(survey_cc, Sex == "Female")
```
# Prepare model variables


```r
model0 <- c("Age", "Center", "Background") # a very basic model without sex.
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
model0_sex_strat <- model0
# model1_sex_strat would just be model 0
model2_sex_strat <- setdiff(model2,  "Sex")
model3_sex_strat <-  setdiff(model3,  "Sex")
model4_sex_strat <- setdiff(model4,  "Sex")
model5_sex_strat <-  setdiff(model5,  "Sex")
```

# prepare pseudo R squared table

```r
psrsq_table <- data.frame(model = paste0("model_",0:5 ),
                     Sex_combined = NA,
                     Male_stratum = NA,
                     Female_stratum = NA)

# fit models (sex combined)
for (i in 0:5){
  cur_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(get(paste0("model",i)), collapse = "+"))), design = survey_cc, family = quasibinomial())
  psrsq_table[i+1, "Sex_combined"] <- round(psrsq(cur_fit)*100, 2) # in percentages - nicer
  
}

# male only
for (i in c(0, 2:5)){
  cur_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(get(paste0("model",i, "_sex_strat")), collapse = "+"))), design = survey_cc_male, family = quasibinomial())
  psrsq_table[i+1, "Male_stratum"] <- round(psrsq(cur_fit)*100, 2) # in percentages - nicer
  
}

# female only
for (i in c(0, 2:5)){
  cur_fit <- svyglm(formula = as.formula(paste0("Insomnia~", paste(get(paste0("model",i, "_sex_strat")), collapse = "+"))), design = survey_cc_female, family = quasibinomial())
  psrsq_table[i+1, "Female_stratum"] <- round(psrsq(cur_fit)*100, 2) # in percentages - nicer
  
}

psrsq_table
```

```
##     model Sex_combined Male_stratum Female_stratum
## 1 model_0         4.36         3.55           5.30
## 2 model_1         5.56           NA             NA
## 3 model_2         5.64         3.61           5.42
## 4 model_3         8.09         5.46           8.53
## 5 model_4         6.89         5.21           6.82
## 6 model_5        14.62        12.64          15.27
```

```r
write.csv(psrsq_table, file = file.path(folder_path, "Results/Pseudo_rsq_table.csv"))
```




```r
sessionInfo()
```

```
## R version 4.2.3 (2023-03-15)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS 14.5
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
