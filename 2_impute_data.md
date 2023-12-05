---
title: "Imputing data for secondary analysis"
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
library(mice)
```

```
## 
## Attaching package: 'mice'
## 
## The following objects are masked from 'package:mi':
## 
##     complete, pool
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     cbind, rbind
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
```

# Read in the data


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
dat <- readRDS(file.path(folder_path, "Data/Data_with_miss.Rds"))
```

# Variables of interest (for analysis and imputation)

```r
# variables to impute or use in imputation
vars <-  c("Center", 
           "Age", 
           "Gender",
           "STRAT",
            "Marital_status",
            "Income_level",
            "Employment_status",
            "Occupation",
            "Language_pref",
            "US_native",
            "Language_acculturation",
            "Social_acculturation",
            "Ethnic_identity_score", 
            "Current_Health_insurance", 
            "Years_in_US",
            "Background", 
            "Education",
            "STAI10",
            "CESD",
            "Insomnia", 
            "WHIIRS")
```
# Impute using MICE

```r
set.seed(800)

  
imputedat <- dat[,c("ID", vars)]

cl <- parallel::makeCluster(5, setup_strategy = "sequential")
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

mdf <- missing_data.frame(imputedat)
```

```
## NOTE: The following pairs of variables appear to have the same missingness pattern.
##  Please verify whether they are in fact logically distinct variables.
##      [,1]       [,2]    
## [1,] "Insomnia" "WHIIRS"
```

```r
mdf1 <- mi(mdf)
saveRDS(mdf1, file.path(folder_path, "Data/Data_imputed_S4.Rds"))

imputed_data <- as.data.frame(mi:::complete(mdf1,m=5))
saveRDS(imputed_data, file.path(folder_path, "Data/Data_imputed.Rds"))
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
## [1] stats4    grid      stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] naniar_1.0.0     UpSetR_1.4.0     mice_3.16.0      glmnet_4.1-7    
##  [5] boot_1.3-28.1    mi_1.1           sjlabelled_1.2.0 memisc_0.99.31.6
##  [9] MASS_7.3-60      lattice_0.21-8   tableone_0.13.2  labelled_2.12.0 
## [13] factoextra_1.0.7 plyr_1.8.8       survey_4.2-1     survival_3.5-5  
## [17] Matrix_1.5-4.1   lubridate_1.9.2  forcats_1.0.0    stringr_1.5.0   
## [21] dplyr_1.1.2      purrr_1.0.1      readr_2.1.4      tidyr_1.3.0     
## [25] tibble_3.2.1     ggplot2_3.4.2    tidyverse_2.0.0 
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-162      bit64_4.0.5       insight_0.19.3    tools_4.2.3      
##  [5] backports_1.4.1   bslib_0.5.0       utf8_1.2.3        R6_2.5.1         
##  [9] rpart_4.1.19      DBI_1.1.3         colorspace_2.1-0  jomo_2.7-6       
## [13] nnet_7.3-19       withr_2.5.0       tidyselect_1.2.0  gridExtra_2.3    
## [17] bit_4.0.5         compiler_4.2.3    cli_3.6.1         sass_0.4.7       
## [21] scales_1.2.1      digest_0.6.33     minqa_1.2.5       rmarkdown_2.23   
## [25] pkgconfig_2.0.3   htmltools_0.5.5   lme4_1.1-34       fastmap_1.1.1    
## [29] rlang_1.1.1       rstudioapi_0.15.0 shape_1.4.6       jquerylib_0.1.4  
## [33] generics_0.1.3    jsonlite_1.8.7    vroom_1.6.3       car_3.1-2        
## [37] magrittr_2.0.3    Rcpp_1.0.11       munsell_0.5.0     fansi_1.0.4      
## [41] abind_1.4-5       lifecycle_1.0.3   visdat_0.6.0      stringi_1.7.12   
## [45] yaml_2.3.7        carData_3.0-5     parallel_4.2.3    ggrepel_0.9.3    
## [49] crayon_1.5.2      mitml_0.4-5       haven_2.5.3       splines_4.2.3    
## [53] hms_1.1.3         knitr_1.43        pillar_1.9.0      codetools_0.2-19 
## [57] pan_1.8           glue_1.6.2        evaluate_0.21     mitools_2.4      
## [61] data.table_1.14.8 vctrs_0.6.3       nloptr_2.0.3      tzdb_0.4.0       
## [65] foreach_1.5.2     gtable_0.3.3      cachem_1.0.8      xfun_0.39        
## [69] broom_1.0.5       coda_0.19-4       arm_1.13-1        iterators_1.0.14 
## [73] timechange_0.2.0
```
