# el_bias


# Libraries

``` r
Sys.setenv(RETICULATE_PYTHON = "C:/Users/misak/anaconda3/python.exe")
#install.packages("tabulapdf")

# Load required packages
library(tidyverse)
```

    Warning: package 'tidyverse' was built under R version 4.4.2

    Warning: package 'ggplot2' was built under R version 4.4.3

    Warning: package 'tidyr' was built under R version 4.4.3

    Warning: package 'dplyr' was built under R version 4.4.3

    Warning: package 'forcats' was built under R version 4.4.2

    Warning: package 'lubridate' was built under R version 4.4.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(here)
```

    Warning: package 'here' was built under R version 4.4.3

    here() starts at C:/Users/misak/Git-projects/el_bias

``` r
library(parallel)
library(pdftools)
```

    Warning: package 'pdftools' was built under R version 4.4.3

    Using poppler version 23.08.0

``` r
library(stringr)
library(rJava)
library(tabulapdf) # for data tables extraction
```

    Warning: package 'tabulapdf' was built under R version 4.4.3

``` r
library(readxl)
library(reticulate)
```

    Warning: package 'reticulate' was built under R version 4.4.3

``` r
#py_install(c("pandas", "joblib", "scikit-learn"))
```

# Data Extraction

Data for RRFs are coming from the US FDA Clap List.
<https://cdrh-rst.fda.gov/chemicals-list-analytical-performance-clap>

Each file is in a pdf format and contains RRF values for 3 concentration
of a surrogate. LCMS RRFs CLAP Negative Ion Detection.pdf (255.54 KB)
LCMS RRFs CLAP Positive Ion Detection.pdf (279.48 KB) RRF GCMS for
CLAP.pdf (322.77 KB)

They are first extracted to perform the bootstrap simulation. Some data
are n.d. or below DL. The current strategy is to change the n.d. to 0
and below DL to 0.001 based on the input of the Subject Matter Expert.

# GC/MS data

``` r
gcms_clap <- read_excel( 
  path = "processed_data/GCMS.xlsx", 
  sheet = "GC-MS")

gcms_clap_clean <- gcms_clap %>%
  na.omit()

gcms_clap_clean
```

    # A tibble: 92 × 6
       Code  `Trivial Name`                 `CAS #`  `5 µg/mL` `10 µg/mL` `20 µg/mL`
       <chr> <chr>                          <chr>        <dbl>      <dbl>      <dbl>
     1 RM1   Irganox 1010                   6683-19…     0          0          0    
     2 RM2   Irganox 1076                   2082-79…     0.326      0.434      0.336
     3 RM3   Irgafos 168                    31570-0…     0.414      0.393      0.281
     4 RM4   2,6-Di-t-butyl-4- methylphenol 128-37-0     1.83       2.14       1.81 
     5 RM5   2,4-Di-t-butyl phenol          96-76-4      1          1          1    
     6 RM6   2,4-Dimethylphenol             105-67-9     0.71       0.377      0.424
     7 RM7   3-tert-Butyl-4-hydroxyanisole  25013-1…     0.606      0.506      0.583
     8 RM8   Di-(2-ethylhexyl)phthalate     117-81-7     0.823      0.824      0.838
     9 RM9   Dibutylphthalate               84-74-2      0.713      0.744      0.822
    10 RM10  Diphenyl phthalate             84-62-8      0.18       0.437      0.676
    # ℹ 82 more rows

``` r
gcms_clap_clean$"5 µg/mL" <- as.numeric(gcms_clap_clean$"5 µg/mL")
gcms_clap_clean$"10 µg/mL" <- as.numeric(gcms_clap_clean$"10 µg/mL")
gcms_clap_clean$"20 µg/mL" <- as.numeric(gcms_clap_clean$"20 µg/mL")

hist(gcms_clap_clean$"5 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 5 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "skyblue")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
hist(gcms_clap_clean$"10 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 10 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "orange")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-2-2.png)

``` r
hist(gcms_clap_clean$"20 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 20 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "red")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-2-3.png)

# LC/MS pos data

``` r
lcmspos_clap <- read_excel( 
  path = "processed_data/LCMS.xlsx", 
  sheet = "LCMSPOS")

lcmspos_clap_clean <- lcmspos_clap %>%
  na.omit()


hist(lcmspos_clap_clean$"5 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 5 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "skyblue")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
hist(lcmspos_clap_clean$"10 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 10 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "orange")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-3-2.png)

``` r
hist(lcmspos_clap_clean$"20 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 20 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "red")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-3-3.png)

# LC/MS neg data

``` r
lcmsneg_clap <- read_excel( 
  path = "processed_data/LCMS.xlsx", 
  sheet = "LCMSNEG")

lcmsneg_clap_clean <- lcmsneg_clap %>%
  na.omit()

lcmsneg_clap_clean
```

    # A tibble: 28 × 6
       Code  `Trivial Name`             `CAS #`   `5 µg/mL` `10 µg/mL` `20 µg/mL`
       <chr> <chr>                      <chr>         <dbl>      <dbl>      <dbl>
     1 RM2   Irganox 1076               2082-79-3     0          0          0    
     2 RM4   Butylated hydroxytoluene   128-37-0      0.001      0.001      0.001
     3 RM5   2,4-di-tert-butylphenol    96-76-4       0.001      0.001      0.001
     4 RM8   Di-(2-ethylhexyl)phthalate 117-81-7      0          0          0    
     5 RM13  Caprolactam                105-60-2      0          0          0    
     6 RM14  Bisphenol A                80-05-7       0          0          0    
     7 RM15  Erucamide                  112-84-5      0          0          0    
     8 RM16  Oleamide                   301-02-0      0          0          0    
     9 RM25  Dibenzylamine              103-49-1      0          0          0    
    10 RM26  Benzoic acid               65-85-0       0          0          0    
    # ℹ 18 more rows

``` r
hist(lcmsneg_clap_clean$"5 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 5 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "skyblue")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-4-1.png)

``` r
hist(lcmsneg_clap_clean$"10 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 10 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "orange")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-4-2.png)

``` r
hist(lcmsneg_clap_clean$"20 µg/mL", 
     main = "Histogram of RRFs for GC/MS with a 20 µg/mL surrogate Concentration", 
     xlab = "RRF",
     col = "red")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-4-3.png)

# Generate Base concentration

## Define AET

``` r
AET <- 5
```

## Uniform distribution

``` r
set.seed(123)
base_concentrations1 <- runif(100, min = 0.5 * AET, max = 2 * AET)
hist(base_concentrations1)
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-6-1.png)

## Log-Normal distribution

$log(0.001) = -6.907, log(100) = 4.605$ \\ $mid (meanlog) approx -1.15$
\\ $width = 4.605 - (-6.907) = 11.51$ \\ $width/6 = 11.51/6=1.91$

``` r
set.seed(123)
base_concentrations2 <- rlnorm(100, meanlog = -1.15, sdlog = 1.91)
hist(base_concentrations2)
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-7-1.png)

## Custom distribution

``` r
set.seed(123)
Z <- runif(70, min = 0.001, max = 1)
Y <- runif(20, min = 1, max = 10)
K <- runif(10, min = 10, max = 100)

custom <- c(Z, Y, K)

custom_sampled <- sample(custom, size = 100, replace = FALSE)

hist(custom_sampled)
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-8-1.png)

## Normal distribution (simulating a GC/MS RRF distribution)

``` r
set.seed(123)
RRF_norm <- rnorm(50, 1, 0.2)
RRF_norm[RRF_norm<0] <- 0.001 # avoiding negative numbers
hist(RRF_norm)
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-9-1.png)

## Log_Normal distribution (simulating a LC/MS RRF distribution)

``` r
set.seed(123)
RRF_lnorm <- rlnorm(50, 0, 0.5)
RRF_lnorm[RRF_lnorm<0] <- 0.001 # avoiding negative numbers
hist(RRF_lnorm)
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-10-1.png)

# Scenario with uniform distribution & GC/MS data from US FDA CLAP list

## Initialisation of the comparison table

``` r
summary_table <- data.frame(
  Method = character(),
  FN_Rate = numeric(),
  FP_Rate = numeric(),
  FN_SD = numeric(),
  FP_SD = numeric(),
  precision = numeric(),
  recall = numeric(),
  F1 = numeric(),
  f_beta = numeric()
)
```

## GC-MS No Correction Scenarios

``` r
set.seed(123)
N_sim <- 10000
N_extractables <- 80
total_FN <- 0
total_FP <- 0
total_TP <- 0
total_TN <- 0


for (i in 1:N_sim) {
## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
## I) No Correction
corrected_conc <- base_conc_sim * simulated_RRF

is_above_threshold <- base_conc_sim > AET
reported_above_threshold <- corrected_conc > AET

  total_FN <- total_FN + sum(is_above_threshold & !reported_above_threshold, na.rm = TRUE)
  total_FP <- total_FP + sum(!is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TP <- total_TP + sum(is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TN <- total_TN + sum(!is_above_threshold & !reported_above_threshold, na.rm = TRUE)
}

count_Above_AET <- total_TP + total_FN
count_Below_AET  <- total_TN + total_FP
FN_Rate_final <- total_FN / count_Above_AET
FP_Rate_final <- total_FP / count_Below_AET

recall <- total_TP / count_Above_AET
precision <- total_TP / (total_TP + total_FP)

F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

SD_FN <- sqrt(FN_Rate_final * (1 - FN_Rate_final) / count_Above_AET)
SD_FP <- sqrt(FP_Rate_final * (1 - FP_Rate_final) / count_Below_AET)

summary_table <- rbind(summary_table, data.frame(
  Method = "No Correction",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = SD_FN,
  FP_SD = SD_FP,
  precision = precision,
  recall = recall,
  F1 = F1,
  f_betal = f_beta
))
```

``` r
summary_table
```

             Method   FN_Rate    FP_Rate        FN_SD        FP_SD precision
    1 No Correction 0.8147185 0.05370723 0.0005318166 0.0004368801 0.8736549
         recall        F1   f_betal
    1 0.1852815 0.3057258 0.1910719

``` r
hist(base_conc_sim, 
     main = "Histogram of Base concentration around the AET", 
     xlab = "Concentration",
     col = "skyblue")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-14-1.png)

``` r
hist(simulated_RRF,
     main = "Histogram of RRFs (non-parametric bootstrap of GC/MS dataset 5µg/mL) from FDA Clap list", 
     xlab = "Concentration",
     col = "grey")
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-14-2.png)

## GC-MS Correction UF Factor

``` r
rsd <- function(x) {
  return(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
}


set.seed(123)
N_sim <- 10000
UF_RSD <- 1/(1 - rsd(gcms_clap_clean$"5 µg/mL")) # formula from ISO 10993-18
UF_list <- c(
  "UF (2)"   = 2, 
  "UF (4)"   = 4, 
  "UF (5)"   = 5, 
  "UF (10)"  = 10, 
  "UF (RSD)" = UF_RSD
)

N_extractables <- 80

for (m_name in names(UF_list)) {
  uf_val <- UF_list[[m_name]]
  total_FN <- 0
  total_FP <- 0
  total_TP <- 0
  total_TN <- 0
  
  
for (i in 1:N_sim) {
## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
## II) UF corrections
  
  exp_conc <- (base_conc_sim * simulated_RRF)
  corrected_conc <- exp_conc * uf_val 
  
  is_above_threshold <- base_conc_sim > AET
  reported_above_threshold <- corrected_conc > AET

  total_FN <- total_FN + sum(is_above_threshold & !reported_above_threshold, na.rm = TRUE)
  total_FP <- total_FP + sum(!is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TP <- total_TP + sum(is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TN <- total_TN + sum(!is_above_threshold & !reported_above_threshold, na.rm = TRUE)
}

count_Above_AET <- total_TP + total_FN
count_Below_AET  <- total_TN + total_FP
FN_Rate_final <- total_FN / count_Above_AET
FP_Rate_final <- total_FP / count_Below_AET

recall <- total_TP / count_Above_AET
precision <- total_TP / (total_TP + total_FP)

F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

SD_FN <- sqrt(FN_Rate_final * (1 - FN_Rate_final) / count_Above_AET)
SD_FP <- sqrt(FP_Rate_final * (1 - FP_Rate_final) / count_Below_AET)



summary_table <- rbind(summary_table, data.frame(
    Method = m_name,
    FN_Rate = FN_Rate_final,
    FP_Rate = FP_Rate_final,
    FN_SD = SD_FN,
    FP_SD = SD_FP,
    precision = precision,
    recall = recall,
    F1 = F1,
    f_betal = f_beta
))
}


UF_RSD
```

    [1] -4.170144

``` r
summary_table
```

             Method   FN_Rate    FP_Rate        FN_SD        FP_SD precision
    1 No Correction 0.8147185 0.05370723 0.0005318166 0.0004368801 0.8736549
    2        UF (2) 0.6275034 0.18296736 0.0006617763 0.0007492723 0.8031754
    3        UF (4) 0.4341804 0.37196468 0.0006788430 0.0009355552 0.7523746
    4        UF (5) 0.3825895 0.43369280 0.0006655807 0.0009594883 0.7399487
    5       UF (10) 0.2801532 0.61769860 0.0006151340 0.0009403829 0.6993274
    6      UF (RSD) 1.0000000 0.00000000 0.0000000000 0.0000000000       NaN
         recall        F1   f_betal
    1 0.1852815 0.3057258 0.1910719
    2 0.3724966 0.5089516 0.3803407
    3 0.5658196 0.6458962 0.5712676
    4 0.6174105 0.6731484 0.6213682
    5 0.7198468 0.7094388 0.7190354
    6 0.0000000       NaN       NaN

## GC-MS Correction Factor percentile

``` r
percentile <- function(x) {
  return(quantile(x, .16))
}

set.seed(123)
N_sim <- 10000
N_extractables <- 80
total_FN <- 0
total_FP <- 0
total_TP <- 0
total_TN <- 0


for (i in 1:N_sim) {
## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)

exp_conc <- (base_conc_sim * simulated_RRF)
## III) 16th percentile Correction
corrected_conc <- exp_conc * (mean(simulated_RRF) / percentile(simulated_RRF))

is_above_threshold <- base_conc_sim > AET
reported_above_threshold <- corrected_conc > AET

  total_FN <- total_FN + sum(is_above_threshold & !reported_above_threshold, na.rm = TRUE)
  total_FP <- total_FP + sum(!is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TP <- total_TP + sum(is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TN <- total_TN + sum(!is_above_threshold & !reported_above_threshold, na.rm = TRUE)
}

count_Above_AET <- total_TP + total_FN
count_Below_AET  <- total_TN + total_FP
FN_Rate_final <- total_FN / count_Above_AET
FP_Rate_final <- total_FP / count_Below_AET

recall <- total_TP / count_Above_AET
precision <- total_TP / (total_TP + total_FP)

F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

SD_FN <- sqrt(FN_Rate_final * (1 - FN_Rate_final) / count_Above_AET)
SD_FP <- sqrt(FP_Rate_final * (1 - FP_Rate_final) / count_Below_AET)


summary_table <- rbind(summary_table, data.frame(
  Method = "Percentile (16th)",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = SD_FN,
  FP_SD = SD_FP,
  precision = precision,
  recall = recall,
  F1 = F1,
  f_betal = f_beta
))
```

## GC-MS Correction RRFlow

This simulation assumes that we know the true RRF of all compounds
sampled and therefore performing the conditional correction. We also
force the UF to be positive by removing low and high responders.

``` r
## RSD is calculated only for RRFs between 0.05 and 1 
## 'Compounds with RRF lower than 0.05 are excluded from UF calculation (20 times lower than the internal standard).page 8
## Therefore, as a criterion for RRF selection, only compounds with an RRF less than 1 are actually considered for UF calculation. page 8

rsd <- function(x) {
  x_use = x[x<=1&x>0.05]
  return(sd(x_use, na.rm = TRUE) / mean(x_use, na.rm = TRUE))
}

## Simulation starts
set.seed(123)
N_sim <- 10000
N_extractables <- 80
total_FN <- 0
total_FP <- 0
total_TP <- 0
total_TN <- 0

# remove 0 values
RRF_GCMS_superior0 <- gcms_clap_clean$"5 µg/mL"
RRF_GCMS_superior0 <- RRF_GCMS_superior0[RRF_GCMS_superior0 > 0]

# Apply Part 18 formula for database
UF <- 1/(1-rsd(RRF_GCMS_superior0))


# Start simulation
for (i in 1:N_sim) {

## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Sampling of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
## IV) RRflow condition

exp_conc <- base_conc_sim * simulated_RRF   # experimental concentration : True concentration with the actual RRF
corrected_conc <- exp_conc * UF             # corrected concentration: Exp * UF (conservative method)
condition <- (simulated_RRF < 0.5) | (simulated_RRF > 2) # RRFlow conditions rescaling factor should be focused only to those compounds which exhibits RRF< 0.5 and RRF>2 
corrected_conc[condition] <- exp_conc[condition] / simulated_RRF[condition] # average value of the RRF RRFlow application in extractables amount rescaling ( The average RRF determined by the RRFlow will be used to re-calculate the amount of the corresponding compound detected in the extractables study , will assume that it is in the linearity range) 

  is_above_threshold <- base_conc_sim > AET
  reported_above_threshold <- corrected_conc > AET

  total_FN <- total_FN + sum(is_above_threshold & !reported_above_threshold, na.rm = TRUE)
  total_FP <- total_FP + sum(!is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TP <- total_TP + sum(is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TN <- total_TN + sum(!is_above_threshold & !reported_above_threshold, na.rm = TRUE)
}

count_Above_AET <- total_TP + total_FN
count_Below_AET  <- total_TN + total_FP
FN_Rate_final <- total_FN / count_Above_AET
FP_Rate_final <- total_FP / count_Below_AET

recall <- total_TP / count_Above_AET
precision <- total_TP / (total_TP + total_FP)

F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

SD_FN <- sqrt(FN_Rate_final * (1 - FN_Rate_final) / count_Above_AET)
SD_FP <- sqrt(FP_Rate_final * (1 - FP_Rate_final) / count_Below_AET)

summary_table <- rbind(summary_table, data.frame(
  Method = "RRFlow",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = SD_FN,
  FP_SD = SD_FP,
  precision = precision,
  recall = recall,
  F1 = F1,
  f_betal = f_beta
))
```

``` r
UF
```

    [1] 2.864589

``` r
summary_table
```

                 Method   FN_Rate    FP_Rate        FN_SD        FP_SD precision
    1     No Correction 0.8147185 0.05370723 0.0005318166 0.0004368801 0.8736549
    2            UF (2) 0.6275034 0.18296736 0.0006617763 0.0007492723 0.8031754
    3            UF (4) 0.4341804 0.37196468 0.0006788430 0.0009355552 0.7523746
    4            UF (5) 0.3825895 0.43369280 0.0006655807 0.0009594883 0.7399487
    5           UF (10) 0.2801532 0.61769860 0.0006151340 0.0009403829 0.6993274
    6          UF (RSD) 1.0000000 0.00000000 0.0000000000 0.0000000000       NaN
    7 Percentile (16th) 0.2278242 0.77059228 0.0005741165 0.0008147972 0.6676095
    8            RRFlow 0.0000000 0.27639312 0.0000000000 0.0009182391 0.8788913
         recall        F1   f_betal
    1 0.1852815 0.3057258 0.1910719
    2 0.3724966 0.5089516 0.3803407
    3 0.5658196 0.6458962 0.5712676
    4 0.6174105 0.6731484 0.6213682
    5 0.7198468 0.7094388 0.7190354
    6 0.0000000       NaN       NaN
    7 0.7721758 0.7160955 0.7675520
    8 1.0000000 0.9355424 0.9947280

## GC-MS Correction percentile 10th

Here we divide by the 10th percentile (which is arbitrary) assuming to
capture the left hand side of the RRF database (low responders)

``` r
set.seed(123)
N_sim <- 10000
N_extractables <- 80
total_FN <- 0
total_FP <- 0
total_TP <- 0
total_TN <- 0


RRF_GCMS_superior0 <- gcms_clap_clean$"5 µg/mL"
RRF_GCMS_superior0 <- RRF_GCMS_superior0[RRF_GCMS_superior0 > 0]

conservative_RRF_estimate <- quantile(gcms_clap_clean$"5 µg/mL", 0.10)

for (i in 1:N_sim) {

## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
## V RRF < 1

exp_conc <- base_conc_sim * simulated_RRF   # experimental concentration : True concentration with the actual RRF
corrected_conc <- exp_conc / conservative_RRF_estimate 

is_above_threshold <- base_conc_sim > AET
reported_above_threshold <- corrected_conc > AET

  total_FN <- total_FN + sum(is_above_threshold & !reported_above_threshold, na.rm = TRUE)
  total_FP <- total_FP + sum(!is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TP <- total_TP + sum(is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TN <- total_TN + sum(!is_above_threshold & !reported_above_threshold, na.rm = TRUE)
}

count_Above_AET <- total_TP + total_FN
count_Below_AET  <- total_TN + total_FP
FN_Rate_final <- total_FN / count_Above_AET
FP_Rate_final <- total_FP / count_Below_AET

recall <- total_TP / count_Above_AET
precision <- total_TP / (total_TP + total_FP)


F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

SD_FN <- sqrt(FN_Rate_final * (1 - FN_Rate_final) / count_Above_AET)
SD_FP <- sqrt(FP_Rate_final * (1 - FP_Rate_final) / count_Below_AET)

summary_table <- rbind(summary_table, data.frame(
  Method = "Percentile 10th",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = SD_FN,
  FP_SD = SD_FP,
  precision = precision,
  recall = recall,
  F1 = F1,
  f_betal = f_beta
))
```

``` r
summary_table
```

                 Method   FN_Rate    FP_Rate        FN_SD        FP_SD precision
    1     No Correction 0.8147185 0.05370723 0.0005318166 0.0004368801 0.8736549
    2            UF (2) 0.6275034 0.18296736 0.0006617763 0.0007492723 0.8031754
    3            UF (4) 0.4341804 0.37196468 0.0006788430 0.0009355552 0.7523746
    4            UF (5) 0.3825895 0.43369280 0.0006655807 0.0009594883 0.7399487
    5           UF (10) 0.2801532 0.61769860 0.0006151340 0.0009403829 0.6993274
    6          UF (RSD) 1.0000000 0.00000000 0.0000000000 0.0000000000       NaN
    7 Percentile (16th) 0.2278242 0.77059228 0.0005741165 0.0008147972 0.6676095
    8            RRFlow 0.0000000 0.27639312 0.0000000000 0.0009182391 0.8788913
    9   Percentile 10th 0.1085657 0.89080919 0.0004258268 0.0006043918 0.6673095
         recall        F1   f_betal
    1 0.1852815 0.3057258 0.1910719
    2 0.3724966 0.5089516 0.3803407
    3 0.5658196 0.6458962 0.5712676
    4 0.6174105 0.6731484 0.6213682
    5 0.7198468 0.7094388 0.7190354
    6 0.0000000       NaN       NaN
    7 0.7721758 0.7160955 0.7675520
    8 1.0000000 0.9355424 0.9947280
    9 0.8914343 0.7632590 0.8800658

## Optimised percentile

Here we find the best percentile to divide to target a recall of 95%
(which is also arbitrary)

``` r
set.seed(123)

target_recall <- 0.95
found <- FALSE
optimal_percentile <- NA

N_sim <- 10000
N_extractables <- 80

RRF_GCMS_superior0 <- gcms_clap_clean$"5 µg/mL"
RRF_GCMS_superior0 <- RRF_GCMS_superior0[RRF_GCMS_superior0 > 0]

for (test_percentile in seq(0.50, 0.01, by = -0.01)) {
  
  conservative_RRF_estimate <- quantile(RRF_GCMS_superior0, test_percentile, na.rm=TRUE)
  
  total_FN <- 0
  total_FP <- 0
  total_TP <- 0
  total_TN <- 0

  for (i in 1:N_sim) {
    base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
    simulated_RRF <- sample(RRF_GCMS_superior0, size = N_extractables, replace = FALSE)
    
    exp_conc <- base_conc_sim * simulated_RRF   

    corrected_conc <- exp_conc / conservative_RRF_estimate 

    is_above_threshold <- base_conc_sim > AET
    reported_above_threshold <- corrected_conc > AET

    total_FN <- total_FN + sum(is_above_threshold & !reported_above_threshold, na.rm = TRUE)
    total_FP <- total_FP + sum(!is_above_threshold & reported_above_threshold, na.rm = TRUE)
    total_TP <- total_TP + sum(is_above_threshold & reported_above_threshold, na.rm = TRUE)
    total_TN <- total_TN + sum(!is_above_threshold & !reported_above_threshold, na.rm = TRUE)
}

  # Calculate Recall (Sensitivity)
  # Denominator: All Truly Toxic (TP + FN)
  current_recall <- total_TP / (total_TP + total_FN)
  
  print(paste("Percentile:", test_percentile, "-> Recall:", round(current_recall, 4))) 

  if(current_recall >= target_recall) {
    optimal_percentile <- test_percentile
    found <- TRUE
    break  
  }
}
```

    [1] "Percentile: 0.5 -> Recall: 0.6074"
    [1] "Percentile: 0.49 -> Recall: 0.6107"
    [1] "Percentile: 0.48 -> Recall: 0.6186"
    [1] "Percentile: 0.47 -> Recall: 0.6288"
    [1] "Percentile: 0.46 -> Recall: 0.664"
    [1] "Percentile: 0.45 -> Recall: 0.6798"
    [1] "Percentile: 0.44 -> Recall: 0.6841"
    [1] "Percentile: 0.43 -> Recall: 0.6879"
    [1] "Percentile: 0.42 -> Recall: 0.6985"
    [1] "Percentile: 0.41 -> Recall: 0.707"
    [1] "Percentile: 0.4 -> Recall: 0.7139"
    [1] "Percentile: 0.39 -> Recall: 0.7195"
    [1] "Percentile: 0.38 -> Recall: 0.7221"
    [1] "Percentile: 0.37 -> Recall: 0.7259"
    [1] "Percentile: 0.36 -> Recall: 0.7273"
    [1] "Percentile: 0.35 -> Recall: 0.7318"
    [1] "Percentile: 0.34 -> Recall: 0.739"
    [1] "Percentile: 0.33 -> Recall: 0.7509"
    [1] "Percentile: 0.32 -> Recall: 0.7667"
    [1] "Percentile: 0.31 -> Recall: 0.7673"
    [1] "Percentile: 0.3 -> Recall: 0.7687"
    [1] "Percentile: 0.29 -> Recall: 0.7725"
    [1] "Percentile: 0.28 -> Recall: 0.7778"
    [1] "Percentile: 0.27 -> Recall: 0.7907"
    [1] "Percentile: 0.26 -> Recall: 0.8016"
    [1] "Percentile: 0.25 -> Recall: 0.8039"
    [1] "Percentile: 0.24 -> Recall: 0.8053"
    [1] "Percentile: 0.23 -> Recall: 0.8067"
    [1] "Percentile: 0.22 -> Recall: 0.8086"
    [1] "Percentile: 0.21 -> Recall: 0.8119"
    [1] "Percentile: 0.2 -> Recall: 0.8138"
    [1] "Percentile: 0.19 -> Recall: 0.8182"
    [1] "Percentile: 0.18 -> Recall: 0.829"
    [1] "Percentile: 0.17 -> Recall: 0.8411"
    [1] "Percentile: 0.16 -> Recall: 0.847"
    [1] "Percentile: 0.15 -> Recall: 0.8566"
    [1] "Percentile: 0.14 -> Recall: 0.8631"
    [1] "Percentile: 0.13 -> Recall: 0.8658"
    [1] "Percentile: 0.12 -> Recall: 1"

``` r
if(found) {
  print(paste("Optimal Percentile for 95% Recall:", optimal_percentile))
  
  count_Above_AET <- total_TP + total_FN
  count_Below_AET  <- total_TN + total_FP
  
  FN_Rate_final <- total_FN / count_Above_AET
  FP_Rate_final <- total_FP / count_Below_AET

  recall <- total_TP / count_Above_AET
  precision <- total_TP / (total_TP + total_FP)
  
  F1 <- (2 * precision * recall / (precision + recall))
  beta <- 5
  f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

  SD_FN <- sqrt(FN_Rate_final * (1 - FN_Rate_final) / count_Above_AET)
SD_FP <- sqrt(FP_Rate_final * (1 - FP_Rate_final) / count_Below_AET)

  summary_table <- rbind(summary_table, data.frame(
    Method = paste0("Optimized_P"),
    FN_Rate = FN_Rate_final,
    FP_Rate = FP_Rate_final,
    FN_SD = SD_FN,
    FP_SD = SD_FP,
    precision = precision,
    recall = recall,
    F1 = F1,
    f_betal = f_beta 
  ))
  
} else {
  print("1st percentile was not conservative enough to reach 95% recall.")
}
```

    [1] "Optimal Percentile for 95% Recall: 0.12"

``` r
summary_table
```

                  Method   FN_Rate    FP_Rate        FN_SD        FP_SD precision
    1      No Correction 0.8147185 0.05370723 0.0005318166 0.0004368801 0.8736549
    2             UF (2) 0.6275034 0.18296736 0.0006617763 0.0007492723 0.8031754
    3             UF (4) 0.4341804 0.37196468 0.0006788430 0.0009355552 0.7523746
    4             UF (5) 0.3825895 0.43369280 0.0006655807 0.0009594883 0.7399487
    5            UF (10) 0.2801532 0.61769860 0.0006151340 0.0009403829 0.6993274
    6           UF (RSD) 1.0000000 0.00000000 0.0000000000 0.0000000000       NaN
    7  Percentile (16th) 0.2278242 0.77059228 0.0005741165 0.0008147972 0.6676095
    8             RRFlow 0.0000000 0.27639312 0.0000000000 0.0009182391 0.8788913
    9    Percentile 10th 0.1085657 0.89080919 0.0004258268 0.0006043918 0.6673095
    10       Optimized_P 0.0000000 0.86558377 0.0000000000 0.0006609626 0.6983468
          recall        F1   f_betal
    1  0.1852815 0.3057258 0.1910719
    2  0.3724966 0.5089516 0.3803407
    3  0.5658196 0.6458962 0.5712676
    4  0.6174105 0.6731484 0.6213682
    5  0.7198468 0.7094388 0.7190354
    6  0.0000000       NaN       NaN
    7  0.7721758 0.7160955 0.7675520
    8  1.0000000 0.9355424 0.9947280
    9  0.8914343 0.7632590 0.8800658
    10 1.0000000 0.8223842 0.9836579

## Machine Learning

Loading the ML Model

``` python
import joblib
import pandas as pd

# Load the model and the feature list
model = joblib.load('rf_rrf_model.joblib')
```

    C:\Users\misak\ANACON~1\Lib\site-packages\sklearn\base.py:376: InconsistentVersionWarning: Trying to unpickle estimator DecisionTreeRegressor from version 1.7.2 when using version 1.5.1. This might lead to breaking code or invalid results. Use at your own risk. For more info please refer to:
    https://scikit-learn.org/stable/model_persistence.html#security-maintainability-limitations
      warnings.warn(
    C:\Users\misak\ANACON~1\Lib\site-packages\sklearn\base.py:376: InconsistentVersionWarning: Trying to unpickle estimator RandomForestRegressor from version 1.7.2 when using version 1.5.1. This might lead to breaking code or invalid results. Use at your own risk. For more info please refer to:
    https://scikit-learn.org/stable/model_persistence.html#security-maintainability-limitations
      warnings.warn(

``` python
feats = joblib.load('features.joblib')

def predict_rrf_raw(df):
    return model.predict(df[feats])
```

``` r
gc_ms <- read_csv("final_model_data.csv") %>%
  drop_na(BP, `Refractive Index`, TPSA, NHOHCount, NumHDonors, NumRotatableBonds, NOCount) 
```

    Rows: 92 Columns: 35
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (6): Code, IUPAC name, CAS #, Empirical Formula, pKa, SMILES
    dbl (29): DBE, MW, BP, logP, Refractive Index, 5 µg/mL, 10 µg/mL, 20 µg/mL, ...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
set.seed(123)
N_sim <- 10000
N_extractables <- 80
total_FN <- 0
total_FP <- 0
total_TP <- 0
total_TN <- 0

total_points <- N_sim * N_extractables

for (i in 1:N_sim) {
sim_data <- gc_ms %>% 
  sample_n(N_extractables, replace = TRUE)
  
## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## VI) RRF generated via the machine learning
simulated_RRF <- py$predict_rrf_raw(sim_data)
## no correction
corrected_conc <- base_conc_sim * simulated_RRF

is_above_threshold <- base_conc_sim > AET
reported_above_threshold <- corrected_conc > AET

  total_FN <- total_FN + sum(is_above_threshold & !reported_above_threshold, na.rm = TRUE)
  total_FP <- total_FP + sum(!is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TP <- total_TP + sum(is_above_threshold & reported_above_threshold, na.rm = TRUE)
  total_TN <- total_TN + sum(!is_above_threshold & !reported_above_threshold, na.rm = TRUE)
}

count_Above_AET <- total_TP + total_FN
count_Below_AET  <- total_TN + total_FP
FN_Rate_final <- total_FN / count_Above_AET
FP_Rate_final <- total_FP / count_Below_AET

recall <- total_TP / count_Above_AET
precision <- total_TP / (total_TP + total_FP)

F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

SD_FN <- sqrt(FN_Rate_final * (1 - FN_Rate_final) / count_Above_AET)
SD_FP <- sqrt(FP_Rate_final * (1 - FP_Rate_final) / count_Below_AET)


summary_table <- rbind(summary_table, data.frame(
  Method = "Machine Learning",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = SD_FN,
  FP_SD = SD_FP,
  precision = precision,
  recall = recall,
  F1 = F1,
  f_betal = f_beta
))
```

``` r
summary_table
```

                  Method   FN_Rate    FP_Rate        FN_SD        FP_SD precision
    1      No Correction 0.8147185 0.05370723 0.0005318166 0.0004368801 0.8736549
    2             UF (2) 0.6275034 0.18296736 0.0006617763 0.0007492723 0.8031754
    3             UF (4) 0.4341804 0.37196468 0.0006788430 0.0009355552 0.7523746
    4             UF (5) 0.3825895 0.43369280 0.0006655807 0.0009594883 0.7399487
    5            UF (10) 0.2801532 0.61769860 0.0006151340 0.0009403829 0.6993274
    6           UF (RSD) 1.0000000 0.00000000 0.0000000000 0.0000000000       NaN
    7  Percentile (16th) 0.2278242 0.77059228 0.0005741165 0.0008147972 0.6676095
    8             RRFlow 0.0000000 0.27639312 0.0000000000 0.0009182391 0.8788913
    9    Percentile 10th 0.1085657 0.89080919 0.0004258268 0.0006043918 0.6673095
    10       Optimized_P 0.0000000 0.86558377 0.0000000000 0.0006609626 0.6983468
    11  Machine Learning 1.0000000 0.00000000 0.0000000000 0.0000000000       NaN
          recall        F1   f_betal
    1  0.1852815 0.3057258 0.1910719
    2  0.3724966 0.5089516 0.3803407
    3  0.5658196 0.6458962 0.5712676
    4  0.6174105 0.6731484 0.6213682
    5  0.7198468 0.7094388 0.7190354
    6  0.0000000       NaN       NaN
    7  0.7721758 0.7160955 0.7675520
    8  1.0000000 0.9355424 0.9947280
    9  0.8914343 0.7632590 0.8800658
    10 1.0000000 0.8223842 0.9836579
    11 0.0000000       NaN       NaN

## Comparison

``` r
plot_data <- summary_table %>%
  pivot_longer(cols = c(FN_Rate, FP_Rate), names_to = "Type", values_to = "Rate") %>%
  mutate(
    SD = ifelse(Type == "FN_Rate", round(FN_SD*100, 2), round(FP_SD*100, 2)),
    Type = ifelse(Type == "FN_Rate", "False Negative", "False Positive")
  )

method_levels <- c(
  "No Correction",
  "UF (2)", 
  "UF (4)", 
  "UF (5)", 
  "UF (10)",  
  "UF (RSD)",
  "Percentile (16th)", 
  "RRFlow",
  "Percentile 10th",
  "Machine Learning",
  "Optimized_P")

plot_data$Method <- factor(plot_data$Method, levels = method_levels)


ggplot(plot_data, aes(x = Method, y = Rate*100, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = pmax(0, Rate*100 - 1.96*SD), ymax = Rate*100 + 1.96*SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    title = paste0("Comparison of GC-MS Correction Methods (5 µg/mL)", " AET=", AET, "µg/mL"),
    subtitle = "Conc. Uniform distribution (around AET), RRF sampled from CLAP list data",
    y = "Rate (%)",
    x = "Method") +
  geom_text(aes(label = sprintf("%.1f%%", Rate*100)), position = position_dodge(width = 0.9), hjust = -0.5, size = 3, vjust = 0.1) +
  scale_fill_manual(values = c("False Negative" = "red", "False Positive" = "grey")) + 
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  coord_flip()
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-28-1.png)

``` r
summary_table %>%
  select(Method, precision, recall, F1, f_betal)
```

                  Method precision    recall        F1   f_betal
    1      No Correction 0.8736549 0.1852815 0.3057258 0.1910719
    2             UF (2) 0.8031754 0.3724966 0.5089516 0.3803407
    3             UF (4) 0.7523746 0.5658196 0.6458962 0.5712676
    4             UF (5) 0.7399487 0.6174105 0.6731484 0.6213682
    5            UF (10) 0.6993274 0.7198468 0.7094388 0.7190354
    6           UF (RSD)       NaN 0.0000000       NaN       NaN
    7  Percentile (16th) 0.6676095 0.7721758 0.7160955 0.7675520
    8             RRFlow 0.8788913 1.0000000 0.9355424 0.9947280
    9    Percentile 10th 0.6673095 0.8914343 0.7632590 0.8800658
    10       Optimized_P 0.6983468 1.0000000 0.8223842 0.9836579
    11  Machine Learning       NaN 0.0000000       NaN       NaN

ROC and Precision Recall plots

``` r
ggplot(summary_table, aes(x = FP_Rate, y = recall, color = Method)) +
  geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  xlim(0, 1) + ylim(0, 1) +
  labs(title = "ROC",
       x = "False Positive Rate (1-Specificity)",
       y = "Recall (Sensitivity)") +
  theme_minimal()
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-30-1.png)

``` r
ggplot(summary_table, aes(x = recall, y = precision, color = Method)) +
  geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # Random guess line
  xlim(0, 1) + ylim(0, 1) +
  labs(title = "Precision/Recall",
       x = "Recall",
       y = "Precision") +
  theme_minimal()
```

    Warning: Removed 2 rows containing missing values or values outside the scale range
    (`geom_point()`).

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-30-2.png)
