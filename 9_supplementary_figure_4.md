---
title: "Prepare supplementary figure 4"
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
library(corrplot)
```

```
## corrplot 0.92 loaded
```

# Read in the data


```r
folder_path <- "/Users/tamarsofer/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/Ongoing_papers/2022_gender_measure"

# load the prepared dataset
dat <- readRDS(file.path(folder_path, "Data/Data_with_miss.Rds"))
```
# Select only variables we want for PC plot


```r
survey_obj <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=dat)

survy_obj_f <- subset(survey_obj,  Gender == "Female")
survy_obj_m <- subset(survey_obj,  Gender == "Male")


# variables for PC plot:

vars <-  c("Marital_status",
            "Income_level",
            "Employment_status",
            "Occupation",
            "Language_pref",
            "Language_acculturation", 
            "Social_acculturation", 
            "Ethnic_identity_score", 
            "Current_Health_insurance", 
            "Years_in_US",
            "Background", 
            "Education",
            "STAI10",
            "CESD")


pc_f <- svyprcomp(as.formula(paste0("~", 
                                  paste0(vars, collapse = "+"))),
                          design=survy_obj_f,scale=TRUE,scores=TRUE)

pc_m <- svyprcomp(as.formula(paste0("~", 
                                  paste0(vars, collapse = "+"))),
                          design=survy_obj_m,scale=TRUE,scores=TRUE)
```

# Extract loadings of PC 1 and make a figure


```r
male_loadings_1 <- get_pca_var(pc_m)$coord[,1]
female_loadings_1 <- get_pca_var(pc_f)$coord[,1]

all_loadings_1 <- cbind(male_loadings_1, female_loadings_1)
colnames(all_loadings_1) <- c("Males", "Females")

# because PCs can be rotated, if the correlation between the males
# and females loadings are negative we multiply it by -1:
cor_m_f_loading <- cor(male_loadings_1, female_loadings_1)
if (cor_m_f_loading < 0){
  all_loadings_1[,2] <- -all_loadings_1[,2] 
}

pdf(file = file.path(folder_path, "Results/PCA_loadings_1_male_v_female.pdf"),
       width = 12, height = 10 )
corrplot(all_loadings_1, 
         method = "shade", 
         addCoef.col = 'black', 
         tl.col = 'black', 
         cl.pos = 'n', 
         number.cex = 0.7)
dev.off()
```

```
## quartz_off_screen 
##                 2
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
##  [1] corrplot_0.92    labelled_2.11.0  factoextra_1.0.7 plyr_1.8.8      
##  [5] survey_4.2-1     survival_3.5-3   Matrix_1.5-3     lubridate_1.9.2 
##  [9] forcats_1.0.0    stringr_1.5.0    dplyr_1.1.2      purrr_1.0.1     
## [13] readr_2.1.4      tidyr_1.3.0      tibble_3.2.1     ggplot2_3.4.2   
## [17] tidyverse_2.0.0 
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.2.0 xfun_0.39        bslib_0.4.2      mitools_2.4     
##  [5] haven_2.5.2      splines_4.2.3    lattice_0.20-45  colorspace_2.1-0
##  [9] vctrs_0.6.2      generics_0.1.3   htmltools_0.5.5  yaml_2.3.7      
## [13] utf8_1.2.3       rlang_1.1.1      jquerylib_0.1.4  pillar_1.9.0    
## [17] glue_1.6.2       withr_2.5.0      DBI_1.1.3        lifecycle_1.0.3 
## [21] munsell_0.5.0    gtable_0.3.3     evaluate_0.21    knitr_1.42      
## [25] tzdb_0.4.0       fastmap_1.1.1    fansi_1.0.4      Rcpp_1.0.10     
## [29] scales_1.2.1     cachem_1.0.8     jsonlite_1.8.4   hms_1.1.3       
## [33] digest_0.6.31    stringi_1.7.12   ggrepel_0.9.3    cli_3.6.1       
## [37] tools_4.2.3      magrittr_2.0.3   sass_0.4.6       pkgconfig_2.0.3 
## [41] timechange_0.2.0 rmarkdown_2.21   rstudioapi_0.14  R6_2.5.1        
## [45] compiler_4.2.3
```
