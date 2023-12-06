---
title: "Prepare table 1"
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
# creat table 1


```r
vars <-  c("Center", 
           "Age", 
           "Gender",
           "STRAT",
            "Marital_status",
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
            "CESD",
            "Insomnia",
           "WHIIRS")

survey_obj <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=dat)


tbl1_noweight <- print(CreateTableOne(vars = vars, data =dat, strata = "Gender"), missing=TRUE,varLabels = TRUE,digits =3,pDigits=3,showAllLevels=TRUE)
```

```
##                                     Stratified by Gender
##                                      level                                                            
##   n                                                                                                   
##   Center (%)                         Bronx                                                            
##                                      Chicago                                                          
##                                      Miami                                                            
##                                      San Diego                                                        
##   Age (mean (SD))                                                                                     
##   Gender (%)                         Female                                                           
##                                      Male                                                             
##   STRAT (mean (SD))                                                                                   
##   Marital_status (%)                 Single                                                           
##                                      Married or living with a partner                                 
##                                      Separated,divorced,or widow(er)                                  
##   Income_level (%)                   Less than $10,000                                                
##                                      $10,001-$20,000                                                  
##                                      $20,001-$40,000                                                  
##                                      $40,001-$75,000                                                  
##                                      More than $75,000                                                
##   Employment_status (%)              Retired/not currently employed                                   
##                                      Employed part-time(<=35 hours/week)                              
##                                      Employed full-time(>35 hours/week)                               
##   Occupation (%)                     Non-skilled worker                                               
##                                      Service Worker                                                   
##                                      Skilled Worker                                                   
##                                      Professional/technical, administrative/executive, or office staff
##                                      Other occupation                                                 
##   Language_pref (%)                  Spanish                                                          
##                                      English                                                          
##   Language_acculturation (mean (SD))                                                                  
##   Social_acculturation (mean (SD))                                                                    
##   Ethnic_identity_score (mean (SD))                                                                   
##   Current_Health_insurance (%)       No                                                               
##                                      Yes                                                              
##   Years_in_US (%)                    Less than 10 Years                                               
##                                      10 Years or More                                                 
##                                      US born                                                          
##   Background (%)                     Domician                                                         
##                                      Central American                                                 
##                                      Cuban                                                            
##                                      Mexican                                                          
##                                      Puerto Rican                                                     
##                                      South American                                                   
##                                      More than one/Other heritage                                     
##   Education (%)                      <12                                                              
##                                      12                                                               
##                                      >12                                                              
##   STAI10 (mean (SD))                                                                                  
##   CESD (mean (SD))                                                                                    
##   Insomnia (%)                       No                                                               
##                                      Yes                                                              
##   WHIIRS (mean (SD))                                                                                  
##                                     Stratified by Gender
##                                      Female         Male           p      test
##   n                                   9835           6580                     
##   Center (%)                          2518 ( 25.6)   1600 ( 24.3)  <0.001     
##                                       2313 ( 23.5)   1821 ( 27.7)             
##                                       2353 ( 23.9)   1724 ( 26.2)             
##                                       2651 ( 27.0)   1435 ( 21.8)             
##   Age (mean (SD))                    46.54 (13.63)  44.81 (14.31)  <0.001     
##   Gender (%)                          9835 (100.0)      0 (  0.0)  <0.001     
##                                          0 (  0.0)   6580 (100.0)             
##   STRAT (mean (SD))                  16.41 (5.03)   16.15 (5.03)    0.001     
##   Marital_status (%)                  2552 ( 26.1)   1970 ( 30.1)  <0.001     
##                                       4726 ( 48.3)   3710 ( 56.7)             
##                                       2505 ( 25.6)    864 ( 13.2)             
##   Income_level (%)                    1587 ( 18.0)    751 ( 12.3)  <0.001     
##                                       3035 ( 34.5)   1834 ( 29.9)             
##                                       2873 ( 32.6)   2185 ( 35.7)             
##                                       1024 ( 11.6)    992 ( 16.2)             
##                                        283 (  3.2)    363 (  5.9)             
##   Employment_status (%)               5264 ( 54.5)   2689 ( 41.7)  <0.001     
##                                       1802 ( 18.7)    926 ( 14.4)             
##                                       2596 ( 26.9)   2832 ( 43.9)             
##   Occupation (%)                      2725 ( 28.2)   2025 ( 31.4)  <0.001     
##                                       1702 ( 17.6)    669 ( 10.4)             
##                                       1774 ( 18.3)   1680 ( 26.0)             
##                                       1666 ( 17.2)    699 ( 10.8)             
##                                       1802 ( 18.6)   1383 ( 21.4)             
##   Language_pref (%)                   7997 ( 81.3)   5122 ( 77.8)  <0.001     
##                                       1838 ( 18.7)   1458 ( 22.2)             
##   Language_acculturation (mean (SD))  1.88 (1.05)    2.10 (1.11)   <0.001     
##   Social_acculturation (mean (SD))    2.16 (0.60)    2.24 (0.60)   <0.001     
##   Ethnic_identity_score (mean (SD))   3.20 (0.57)    3.22 (0.57)    0.009     
##   Current_Health_insurance (%)        4583 ( 47.5)   3337 ( 51.8)  <0.001     
##                                       5065 ( 52.5)   3107 ( 48.2)             
##   Years_in_US (%)                     2328 ( 23.9)   1477 ( 22.6)  <0.001     
##                                       5829 ( 59.7)   3798 ( 58.1)             
##                                       1604 ( 16.4)   1259 ( 19.3)             
##   Background (%)                       963 (  9.8)    510 (  7.8)  <0.001     
##                                       1049 ( 10.7)    683 ( 10.4)             
##                                       1250 ( 12.8)   1098 ( 16.8)             
##                                       4022 ( 41.1)   2450 ( 37.4)             
##                                       1589 ( 16.2)   1139 ( 17.4)             
##                                        635 (  6.5)    437 (  6.7)             
##                                        276 (  2.8)    227 (  3.5)             
##   Education (%)                       3768 ( 38.5)   2439 ( 37.3)  <0.001     
##                                       2353 ( 24.1)   1827 ( 27.9)             
##                                       3660 ( 37.4)   2277 ( 34.8)             
##   STAI10 (mean (SD))                 17.98 (6.17)   16.18 (5.18)   <0.001     
##   CESD (mean (SD))                   10.57 (5.03)    9.15 (4.34)   <0.001     
##   Insomnia (%)                        6103 ( 64.1)   4750 ( 75.1)  <0.001     
##                                       3424 ( 35.9)   1578 ( 24.9)             
##   WHIIRS (mean (SD))                  7.68 (5.54)    6.22 (5.09)   <0.001     
##                                     Stratified by Gender
##                                      Missing
##   n                                         
##   Center (%)                         0.0    
##                                             
##                                             
##                                             
##   Age (mean (SD))                    0.0    
##   Gender (%)                         0.0    
##                                             
##   STRAT (mean (SD))                  0.0    
##   Marital_status (%)                 0.5    
##                                             
##                                             
##   Income_level (%)                   9.1    
##                                             
##                                             
##                                             
##                                             
##   Employment_status (%)              1.9    
##                                             
##                                             
##   Occupation (%)                     1.8    
##                                             
##                                             
##                                             
##                                             
##   Language_pref (%)                  0.0    
##                                             
##   Language_acculturation (mean (SD)) 0.6    
##   Social_acculturation (mean (SD))   4.4    
##   Ethnic_identity_score (mean (SD))  1.5    
##   Current_Health_insurance (%)       2.0    
##                                             
##   Years_in_US (%)                    0.7    
##                                             
##                                             
##   Background (%)                     0.5    
##                                             
##                                             
##                                             
##                                             
##                                             
##                                             
##   Education (%)                      0.6    
##                                             
##                                             
##   STAI10 (mean (SD))                 2.2    
##   CESD (mean (SD))                   2.5    
##   Insomnia (%)                       3.4    
##                                             
##   WHIIRS (mean (SD))                 3.4
```

```r
tbl1_weighted <- print(svyCreateTableOne(vars = vars,  data = survey_obj, strata= "Gender"), missing=TRUE, varLabels = TRUE,digits =3,pDigits=3, showAllLevels=TRUE)
```

```
##                                     Stratified by Gender
##                                      level                                                            
##   n                                                                                                   
##   Center (%)                         Bronx                                                            
##                                      Chicago                                                          
##                                      Miami                                                            
##                                      San Diego                                                        
##   Age (mean (SD))                                                                                     
##   Gender (%)                         Female                                                           
##                                      Male                                                             
##   STRAT (mean (SD))                                                                                   
##   Marital_status (%)                 Single                                                           
##                                      Married or living with a partner                                 
##                                      Separated,divorced,or widow(er)                                  
##   Income_level (%)                   Less than $10,000                                                
##                                      $10,001-$20,000                                                  
##                                      $20,001-$40,000                                                  
##                                      $40,001-$75,000                                                  
##                                      More than $75,000                                                
##   Employment_status (%)              Retired/not currently employed                                   
##                                      Employed part-time(<=35 hours/week)                              
##                                      Employed full-time(>35 hours/week)                               
##   Occupation (%)                     Non-skilled worker                                               
##                                      Service Worker                                                   
##                                      Skilled Worker                                                   
##                                      Professional/technical, administrative/executive, or office staff
##                                      Other occupation                                                 
##   Language_pref (%)                  Spanish                                                          
##                                      English                                                          
##   Language_acculturation (mean (SD))                                                                  
##   Social_acculturation (mean (SD))                                                                    
##   Ethnic_identity_score (mean (SD))                                                                   
##   Current_Health_insurance (%)       No                                                               
##                                      Yes                                                              
##   Years_in_US (%)                    Less than 10 Years                                               
##                                      10 Years or More                                                 
##                                      US born                                                          
##   Background (%)                     Domician                                                         
##                                      Central American                                                 
##                                      Cuban                                                            
##                                      Mexican                                                          
##                                      Puerto Rican                                                     
##                                      South American                                                   
##                                      More than one/Other heritage                                     
##   Education (%)                      <12                                                              
##                                      12                                                               
##                                      >12                                                              
##   STAI10 (mean (SD))                                                                                  
##   CESD (mean (SD))                                                                                    
##   Insomnia (%)                       No                                                               
##                                      Yes                                                              
##   WHIIRS (mean (SD))                                                                                  
##                                     Stratified by Gender
##                                      Female           Male             p     
##   n                                   8557.8           7857.2                
##   Center (%)                          2589.6 ( 30.3)   2170.8 ( 27.6)   0.001
##                                       1253.6 ( 14.6)   1337.2 ( 17.0)        
##                                       2441.0 ( 28.5)   2362.3 ( 30.1)        
##                                       2273.5 ( 26.6)   1987.0 ( 25.3)        
##   Age (mean (SD))                      41.79 (15.12)    40.26 (14.84)  <0.001
##   Gender (%)                          8557.8 (100.0)      0.0 (  0.0)  <0.001
##                                          0.0 (  0.0)   7857.2 (100.0)        
##   STRAT (mean (SD))                    17.65 (5.44)     17.56 (5.39)    0.490
##   Marital_status (%)                  2695.2 ( 31.6)   2964.5 ( 37.9)  <0.001
##                                       4003.3 ( 47.0)   3974.6 ( 50.8)        
##                                       1820.3 ( 21.4)    882.7 ( 11.3)        
##   Income_level (%)                    1337.2 ( 17.6)    833.8 ( 11.5)  <0.001
##                                       2562.6 ( 33.7)   2136.0 ( 29.5)        
##                                       2444.8 ( 32.2)   2496.7 ( 34.4)        
##                                        946.6 ( 12.5)   1213.5 ( 16.7)        
##                                        305.2 (  4.0)    568.2 (  7.8)        
##   Employment_status (%)               4764.6 ( 56.9)   3157.2 ( 41.1)  <0.001
##                                       1574.9 ( 18.8)   1156.8 ( 15.0)        
##                                       2036.0 ( 24.3)   3374.7 ( 43.9)        
##   Occupation (%)                      2013.6 ( 24.0)   2118.6 ( 27.5)  <0.001
##                                       1568.3 ( 18.7)    939.5 ( 12.2)        
##                                       1480.9 ( 17.7)   1987.1 ( 25.8)        
##                                       1640.6 ( 19.6)    902.0 ( 11.7)        
##                                       1679.2 ( 20.0)   1761.0 ( 22.8)        
##   Language_pref (%)                   6561.7 ( 76.7)   5726.0 ( 72.9)   0.001
##                                       1996.1 ( 23.3)   2131.3 ( 27.1)        
##   Language_acculturation (mean (SD))    2.03 (1.12)      2.25 (1.17)   <0.001
##   Social_acculturation (mean (SD))      2.21 (0.60)      2.28 (0.59)   <0.001
##   Ethnic_identity_score (mean (SD))     3.19 (0.56)      3.19 (0.57)    0.793
##   Current_Health_insurance (%)        3916.0 ( 46.8)   4020.6 ( 52.3)  <0.001
##                                       4442.8 ( 53.2)   3666.9 ( 47.7)        
##   Years_in_US (%)                     2403.6 ( 28.3)   2106.2 ( 27.0)  <0.001
##                                       4313.3 ( 50.8)   3746.8 ( 48.0)        
##                                       1775.5 ( 20.9)   1957.8 ( 25.1)        
##   Background (%)                       981.9 ( 11.5)    642.5 (  8.2)  <0.001
##                                        638.1 (  7.5)    571.6 (  7.3)        
##                                       1561.9 ( 18.3)   1709.2 ( 21.8)        
##                                       3245.7 ( 38.1)   2860.4 ( 36.6)        
##                                       1304.1 ( 15.3)   1334.6 ( 17.1)        
##                                        443.4 (  5.2)    370.7 (  4.7)        
##                                        338.9 (  4.0)    336.3 (  4.3)        
##   Education (%)                       2800.9 ( 32.9)   2486.5 ( 31.8)   0.001
##                                       2244.3 ( 26.3)   2365.0 ( 30.2)        
##                                       3475.0 ( 40.8)   2975.0 ( 38.0)        
##   STAI10 (mean (SD))                   17.82 (6.10)     16.17 (5.07)   <0.001
##   CESD (mean (SD))                     10.36 (4.97)      9.07 (4.24)   <0.001
##   Insomnia (%)                        5478.9 ( 66.4)   5765.8 ( 76.4)  <0.001
##                                       2768.4 ( 33.6)   1779.6 ( 23.6)        
##   WHIIRS (mean (SD))                    7.36 (5.49)      6.06 (5.03)   <0.001
##                                     Stratified by Gender
##                                      test Missing
##   n                                              
##   Center (%)                              0.0    
##                                                  
##                                                  
##                                                  
##   Age (mean (SD))                         0.0    
##   Gender (%)                              0.0    
##                                                  
##   STRAT (mean (SD))                       0.0    
##   Marital_status (%)                      0.5    
##                                                  
##                                                  
##   Income_level (%)                        9.1    
##                                                  
##                                                  
##                                                  
##                                                  
##   Employment_status (%)                   1.9    
##                                                  
##                                                  
##   Occupation (%)                          1.8    
##                                                  
##                                                  
##                                                  
##                                                  
##   Language_pref (%)                       0.0    
##                                                  
##   Language_acculturation (mean (SD))      0.6    
##   Social_acculturation (mean (SD))        4.4    
##   Ethnic_identity_score (mean (SD))       1.5    
##   Current_Health_insurance (%)            2.0    
##                                                  
##   Years_in_US (%)                         0.7    
##                                                  
##                                                  
##   Background (%)                          0.5    
##                                                  
##                                                  
##                                                  
##                                                  
##                                                  
##                                                  
##   Education (%)                           0.6    
##                                                  
##                                                  
##   STAI10 (mean (SD))                      2.2    
##   CESD (mean (SD))                        2.5    
##   Insomnia (%)                            3.4    
##                                                  
##   WHIIRS (mean (SD))                      3.4
```

```r
tbl1_w <- tbl1_weighted[,-which(colnames(tbl1_weighted) %in% c("p", "test"))]   
tbl1_unw <- tbl1_noweight[,-which(colnames(tbl1_noweight) %in% c("p", "test"))]

tbl1_comb <- tbl1_unw
col_inds_to_update <- which(colnames(tbl1_unw) %in% c("Female", "Male"))

# update tbl1_comb with the percentages from the weighted table
for (i in col_inds_to_update){
  counts <- sapply(tbl1_unw[,i], function(x){
                  strsplit(x, split = "(", fixed = TRUE)[[1]][1]
                 })
  percent <- sapply(tbl1_w[,i], function(x){
                paste0("(",  strsplit(x, split = "(", fixed = TRUE)[[1]][2])
              })
  tbl1_comb[,i] <- paste0(counts, percent)
}

  
write.csv(tbl1_comb, file.path(folder_path, "Results/Table1_sex_strat.csv"))
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
## [1] stats4    grid      stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] naniar_1.0.0     UpSetR_1.4.0     mice_3.16.0      glmnet_4.1-7    
##  [5] boot_1.3-28.1    mi_1.1           sjlabelled_1.2.0 memisc_0.99.31.6
##  [9] MASS_7.3-58.2    lattice_0.20-45  tableone_0.13.2  labelled_2.11.0 
## [13] factoextra_1.0.7 plyr_1.8.8       survey_4.2-1     survival_3.5-3  
## [17] Matrix_1.5-3     lubridate_1.9.2  forcats_1.0.0    stringr_1.5.0   
## [21] dplyr_1.1.2      purrr_1.0.1      readr_2.1.4      tidyr_1.3.0     
## [25] tibble_3.2.1     ggplot2_3.4.2    tidyverse_2.0.0 
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-162      bit64_4.0.5       insight_0.19.2    tools_4.2.3      
##  [5] backports_1.4.1   bslib_0.4.2       utf8_1.2.3        R6_2.5.1         
##  [9] rpart_4.1.19      DBI_1.1.3         colorspace_2.1-0  jomo_2.7-6       
## [13] nnet_7.3-18       withr_2.5.0       tidyselect_1.2.0  gridExtra_2.3    
## [17] bit_4.0.5         compiler_4.2.3    cli_3.6.1         sass_0.4.6       
## [21] scales_1.2.1      proxy_0.4-27      digest_0.6.31     minqa_1.2.5      
## [25] rmarkdown_2.21    pkgconfig_2.0.3   htmltools_0.5.5   lme4_1.1-33      
## [29] fastmap_1.1.1     rlang_1.1.1       rstudioapi_0.14   shape_1.4.6      
## [33] jquerylib_0.1.4   generics_0.1.3    zoo_1.8-12        jsonlite_1.8.4   
## [37] vroom_1.6.3       car_3.1-2         magrittr_2.0.3    Rcpp_1.0.10      
## [41] munsell_0.5.0     fansi_1.0.4       abind_1.4-5       lifecycle_1.0.3  
## [45] visdat_0.6.0      stringi_1.7.12    yaml_2.3.7        carData_3.0-5    
## [49] parallel_4.2.3    ggrepel_0.9.3     crayon_1.5.2      mitml_0.4-5      
## [53] haven_2.5.2       splines_4.2.3     hms_1.1.3         knitr_1.42       
## [57] pillar_1.9.0      codetools_0.2-19  pan_1.8           glue_1.6.2       
## [61] evaluate_0.21     mitools_2.4       data.table_1.14.8 vctrs_0.6.2      
## [65] nloptr_2.0.3      tzdb_0.4.0        foreach_1.5.2     gtable_0.3.3     
## [69] cachem_1.0.8      xfun_0.39         broom_1.0.4       e1071_1.7-13     
## [73] coda_0.19-4       class_7.3-21      arm_1.13-1        iterators_1.0.14 
## [77] timechange_0.2.0
```
