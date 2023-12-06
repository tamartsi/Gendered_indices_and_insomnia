---
title: "Train gendered indices over the multiply-imputed datasets"
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

# Load the dataset


```r
folder_path <- "/Users/tamarsofer/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/Ongoing_papers/2022_gender_measure"
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
dat <- readRDS(file.path(folder_path, "Data/Data_imputed.Rds"))

# the data with missing values:
# load the prepared dataset
dat_miss <- readRDS(file.path(folder_path, "Data/Data_with_miss.Rds"))

nrow(dat)
```

```
## [1] 16415
```

# Train the two indices
Primary gendered index: does not include psychological variables. Secondary gendered index: includes also trait anxiety scale and trait depression scale (without the sleep-related item).




```r
vars_primary <- c("Marital_status",
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
vars_secondary <- c(vars_primary, "STAI10", "CESD")


for (imp_version in c("", ".1", ".2", ".3", ".4")){
  # define the model matrix for the primary index 
  X_primary <- model.matrix(as.formula(paste("Gender ~",
                             paste(vars_primary, collapse = paste0(imp_version,"+")))), dat)
  
  X_secondary <- model.matrix(as.formula(paste("Gender ~",
                             paste(vars_secondary, collapse = paste0(imp_version,"+")))), dat)
                             
        
  # primary index
  set.seed(929)
  cv_lasso_primary <- cv.glmnet(X_primary, dat$Gender, alpha = 1, family = "binomial", nfolds = 5)

  # use the lambda that resulted in minimum CVM to refit the model

  set.seed(929)
  lasso_model_primary <- glmnet(X_primary, dat$Gender, family = "binomial", alpha = 1, lambda = cv_lasso_primary$lambda.min)
  primary_index <- lasso_model_primary %>% predict(newx = X_primary)

  # secondary index
  cv_lasso_secondary <- cv.glmnet(X_secondary, dat$Gender, alpha = 1, family = "binomial", nfolds = 5)

# use the lambda that resulted in minimum CVM to refit the model

  set.seed(929)
  lasso_model_secondary <- glmnet(X_secondary, dat$Gender, family = "binomial", alpha = 1, lambda = cv_lasso_secondary$lambda.min)
  secondary_index <- lasso_model_secondary %>% predict(newx = X_secondary)

## add to the dataset
  dat[[paste0("Primary_index", imp_version)]]  <- primary_index
  dat[[paste0("Secondary_index", imp_version)]]  <- secondary_index
             
}
```

# Summarize the results

```

## Creat supplemental figure 2: average imputed indices, distributions by gender

We are not standardizing them at this point, because it does not matter. 


```r
dat$Mean_imputed_primary_index <- rowMeans(dat[,paste0("Primary_index", 
                                            c("", ".1", ".2", ".3", ".4"))])

dat$Mean_imputed_secondary_index <- rowMeans(dat[,paste0("Secondary_index", 
                                            c("", ".1", ".2", ".3", ".4"))])
                                            
dat$Sex <- dat$Gender                                            

for_plot <- pivot_longer(dat, 
                         cols = c("Mean_imputed_primary_index", "Mean_imputed_secondary_index"), 
                         names_to = "Index_type", 
                         values_to = "Index")

p <- ggplot(for_plot, aes(x = Index)) + 
    geom_histogram(aes(fill = Sex), alpha = 0.6, position = "identity") +
    ggtitle("Histogram of indices stratified by sex") + 
    theme_bw() + 
    scale_fill_brewer(palette="Dark2") + 
    xlab("Index value") + ylab("Count") 

p + facet_wrap(~Index_type)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](7_train_scores_imputed_data_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
ggsave(file = file.path(folder_path, "Results/Indices_imputed_histograms.pdf"), 
       width = 6, height = 2.7)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

# Make a figure based only on imputed observations:


```r
ID_imp <- dat_miss$ID[!complete.cases(dat_miss)]
length(ID_imp)
```

```
## [1] 2749
```

```r
p <- ggplot(for_plot[for_plot$ID %in% ID_imp,], aes(x = Index)) + 
    geom_histogram(aes(fill = Sex), alpha = 0.6, position = "identity") +
    ggtitle("Histogram of indices stratified by sex") + 
    theme_bw() + 
    scale_fill_brewer(palette="Dark2") + 
    xlab("Index value") + ylab("Count") 

p + facet_wrap(~Index_type)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](7_train_scores_imputed_data_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
ggsave(file = file.path(folder_path, "Results/Indices_only_imputed_histograms.pdf"), 
       width = 6, height = 2.7)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

# Save the dataset for future use


```r
saveRDS(dat, file.path(folder_path, "Data/Data_imputed_with_indices.Rds"))
```


```r
sessionInfo()
```

```
## R version 4.2.3 (2023-03-15)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Ventura 13.3.1
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
##  [1] reshape2_1.4.4     jtools_2.2.1       RColorBrewer_1.1-3 naniar_1.0.0      
##  [5] UpSetR_1.4.0       glmnet_4.1-7       boot_1.3-28.1      sjlabelled_1.2.0  
##  [9] memisc_0.99.31.6   MASS_7.3-58.2      lattice_0.20-45    labelled_2.11.0   
## [13] factoextra_1.0.7   plyr_1.8.8         survey_4.2-1       survival_3.5-3    
## [17] Matrix_1.5-3       lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0     
## [21] dplyr_1.1.2        purrr_1.0.1        readr_2.1.4        tidyr_1.3.0       
## [25] tibble_3.2.1       ggplot2_3.4.2      tidyverse_2.0.0   
## 
## loaded via a namespace (and not attached):
##  [1] sass_0.4.6        bit64_4.0.5       vroom_1.6.3       jsonlite_1.8.4   
##  [5] splines_4.2.3     foreach_1.5.2     carData_3.0-5     bslib_0.4.2      
##  [9] highr_0.10        pander_0.6.5      yaml_2.3.7        ggrepel_0.9.3    
## [13] pillar_1.9.0      glue_1.6.2        visdat_0.6.0      digest_0.6.31    
## [17] colorspace_2.1-0  htmltools_0.5.5   pkgconfig_2.0.3   haven_2.5.2      
## [21] scales_1.2.1      tzdb_0.4.0        timechange_0.2.0  farver_2.1.1     
## [25] generics_0.1.3    car_3.1-2         cachem_1.0.8      withr_2.5.0      
## [29] cli_3.6.1         magrittr_2.0.3    crayon_1.5.2      evaluate_0.21    
## [33] fansi_1.0.4       textshaping_0.3.6 tools_4.2.3       data.table_1.14.8
## [37] hms_1.1.3         mitools_2.4       lifecycle_1.0.3   munsell_0.5.0    
## [41] compiler_4.2.3    jquerylib_0.1.4   systemfonts_1.0.4 rlang_1.1.1      
## [45] iterators_1.0.14  rstudioapi_0.14   labeling_0.4.2    rmarkdown_2.21   
## [49] gtable_0.3.3      codetools_0.2-19  abind_1.4-5       DBI_1.1.3        
## [53] R6_2.5.1          gridExtra_2.3     knitr_1.42        bit_4.0.5        
## [57] fastmap_1.1.1     utf8_1.2.3        ragg_1.2.5        insight_0.19.2   
## [61] shape_1.4.6       stringi_1.7.12    parallel_4.2.3    Rcpp_1.0.10      
## [65] vctrs_0.6.2       tidyselect_1.2.0  xfun_0.39
```
