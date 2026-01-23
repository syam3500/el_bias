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
total_points <- N_sim * N_extractables

for (i in 1:N_sim) {
## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
## I) No Correction
corrected_conc <- base_conc_sim * simulated_RRF

total_FN <- total_FN + sum((base_conc_sim > AET) & (corrected_conc < AET))
total_FP <- total_FP + sum((base_conc_sim < AET) & (corrected_conc > AET))
total_TP <- total_TP + sum((base_conc_sim > AET) & (corrected_conc > AET))
total_TN <- total_TN + sum((base_conc_sim < AET) & (corrected_conc < AET))

}

FN_Rate_final <- total_FN / total_points
FP_Rate_final <- total_FP / total_points
TP_Rate_final <- total_TP / total_points
TN_Rate_final <- total_TN / total_points
precision <- TP_Rate_final / (TP_Rate_final+FP_Rate_final)
recall <- TP_Rate_final / (TP_Rate_final + FN_Rate_final)
F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

summary_table <- rbind(summary_table, data.frame(
  Method = "No Correction",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = sqrt(FN_Rate_final * (1 - FN_Rate_final) / total_points),
  FP_SD = sqrt(FP_Rate_final * (1 - FP_Rate_final) / total_points),
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
    1 No Correction 0.5435425 0.01787625 0.0005568932 0.0001481414 0.8736549
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

Uniform concentration around the AET \\ Non Parametric Bootstrap with
replacement of the GC/MS dataset \\ UF correction : from 2, 4, 5, 10 and
UF = 1/(1-RSD)

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
total_points <- N_sim * N_extractables

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
  
  total_FN <- total_FN + sum((base_conc_sim > AET) & (corrected_conc < AET))
  total_FP <- total_FP + sum((base_conc_sim < AET) & (corrected_conc > AET))
  total_TP <- total_TP + sum((base_conc_sim > AET) & (corrected_conc > AET))
  total_TN <- total_TN + sum((base_conc_sim < AET) & (corrected_conc < AET))

}

FN_Rate_final <- total_FN / total_points
FP_Rate_final <- total_FP / total_points
TP_Rate_final <- total_TP / total_points
TN_Rate_final <- total_TN / total_points
precision <- TP_Rate_final / (TP_Rate_final+FP_Rate_final)
recall <- TP_Rate_final / (TP_Rate_final+TN_Rate_final)
F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)




summary_table <- rbind(summary_table, data.frame(
    Method = m_name,
    FN_Rate = FN_Rate_final,
    FP_Rate = FP_Rate_final,
    FN_SD = sqrt(FN_Rate_final * (1 - FN_Rate_final) / total_points),
    FP_SD = sqrt(FP_Rate_final * (1 - FP_Rate_final) / total_points),
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
    1 No Correction 0.5435425 0.01787625 0.0005568932 0.0001481414 0.8736549
    2        UF (2) 0.4186413 0.06090000 0.0005515668 0.0002673742 0.8031754
    3        UF (4) 0.2893275 0.12409625 0.0005069728 0.0003686061 0.7523746
    4        UF (5) 0.2550050 0.14462625 0.0004873108 0.0003932390 0.7399487
    5       UF (10) 0.1866387 0.20618625 0.0004356098 0.0004523183 0.6993274
    6      UF (RSD) 0.6664125 0.00000000 0.0005271467 0.0000000000       NaN
         recall        F1   f_betal
    1 0.1852815 0.3057258 0.1910719
    2 0.4774874 0.5989182 0.4850524
    3 0.6427958 0.6932819 0.6464168
    4 0.6854433 0.7116539 0.6873908
    5 0.7898279 0.7418276 0.7859161
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
total_points <- N_sim * N_extractables


for (i in 1:N_sim) {
## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)

exp_conc <- (base_conc_sim * simulated_RRF)
## III) 16th percentile Correction
corrected_conc <- exp_conc * (mean(simulated_RRF) / percentile(simulated_RRF))

total_FN <- total_FN + sum((base_conc_sim > AET) & (corrected_conc < AET))
total_FP <- total_FP + sum((base_conc_sim < AET) & (corrected_conc > AET))
total_TP <- total_TP + sum((base_conc_sim > AET) & (corrected_conc > AET))
total_TN <- total_TN + sum((base_conc_sim < AET) & (corrected_conc < AET))

}

FN_Rate_final <- total_FN / total_points
FP_Rate_final <- total_FP / total_points
TP_Rate_final <- total_TP / total_points
TN_Rate_final <- total_TN / total_points
precision <- TP_Rate_final / (TP_Rate_final+FP_Rate_final)
recall <- TP_Rate_final / (TP_Rate_final + FN_Rate_final)
F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

summary_table <- rbind(summary_table, data.frame(
  Method = "Percentile (16th)",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = sqrt(FN_Rate_final * (1 - FN_Rate_final) / total_points),
  FP_SD = sqrt(FP_Rate_final * (1 - FP_Rate_final) / total_points),
  precision = precision,
  recall = recall,
  F1 = F1,
  f_betal = f_beta
))
```

## GC-MS Correction RRFlow

``` r
rsd <- function(x) {
  x_use = x[x<=2&x>0.05]
  return(sd(x_use, na.rm = TRUE) / mean(x_use, na.rm = TRUE))
}

set.seed(123)
N_sim <- 10000
N_extractables <- 80
total_FN <- 0
total_FP <- 0
total_TP <- 0
total_TN <- 0
total_points <- N_sim * N_extractables

RRF_GCMS_superior0 <- gcms_clap_clean$"5 µg/mL"
RRF_GCMS_superior0 <- RRF_GCMS_superior0[RRF_GCMS_superior0 > 0]

UF <- 1/(1-rsd(RRF_GCMS_superior0))

for (i in 1:N_sim) {

## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
## IV) RRflow condition

exp_conc <- base_conc_sim * simulated_RRF
corrected_conc <- exp_conc
condition <- (simulated_RRF < 0.5) | (simulated_RRF > 2)
corrected_conc[!condition] <- exp_conc[!condition] * UF
corrected_conc[condition] <- base_conc_sim[condition]

total_FN <- total_FN + sum((base_conc_sim > AET) & (corrected_conc < AET), na.rm = TRUE)
total_FP <- total_FP + sum((base_conc_sim < AET) & (corrected_conc > AET), na.rm = TRUE)
total_TP <- total_TP + sum((base_conc_sim > AET) & (corrected_conc > AET), na.rm = TRUE)
total_TN <- total_TN + sum((base_conc_sim < AET) & (corrected_conc < AET), na.rm = TRUE)
}

FN_Rate_final <- total_FN / total_points
FP_Rate_final <- total_FP / total_points
TP_Rate_final <- total_TP / total_points
TN_Rate_final <- total_TN / total_points
precision <- TP_Rate_final / (TP_Rate_final+FP_Rate_final)
recall <- TP_Rate_final / (TP_Rate_final + FN_Rate_final)
F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

summary_table <- rbind(summary_table, data.frame(
  Method = "RRFlow",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = sqrt(FN_Rate_final * (1 - FN_Rate_final) / total_points),
  FP_SD = sqrt(FP_Rate_final * (1 - FP_Rate_final) / total_points),
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
    1     No Correction 0.5435425 0.01787625 0.0005568932 0.0001481414 0.8736549
    2            UF (2) 0.4186413 0.06090000 0.0005515668 0.0002673742 0.8031754
    3            UF (4) 0.2893275 0.12409625 0.0005069728 0.0003686061 0.7523746
    4            UF (5) 0.2550050 0.14462625 0.0004873108 0.0003932390 0.7399487
    5           UF (10) 0.1866387 0.20618625 0.0004356098 0.0004523183 0.6993274
    6          UF (RSD) 0.6664125 0.00000000 0.0005271467 0.0000000000       NaN
    7 Percentile (16th) 0.1519938 0.25648875 0.0004013908 0.0004882395 0.6676095
    8            RRFlow 0.0000000 0.08659000 0.0000000000 0.0003144284 0.8851201
         recall        F1   f_betal
    1 0.1852815 0.3057258 0.1910719
    2 0.4774874 0.5989182 0.4850524
    3 0.6427958 0.6932819 0.6464168
    4 0.6854433 0.7116539 0.6873908
    5 0.7898279 0.7418276 0.7859161
    6 0.0000000       NaN       NaN
    7 0.7721758 0.7160955 0.7675520
    8 1.0000000 0.9390596 0.9950329

## GC-MS Correction RRF \< 1

Uniform concentration around the AET \\ Non Parametric Bootstrap with
replacement of the GC/MS dataset \\ Correction by applying condition on
the RRFs.

``` r
set.seed(123)
N_sim <- 10000
N_extractables <- 80
total_FN <- 0
total_FP <- 0
total_TP <- 0
total_TN <- 0
total_points <- N_sim * N_extractables

RRF_GCMS_superior0 <- gcms_clap_clean$"5 µg/mL"
RRF_GCMS_superior0 <- RRF_GCMS_superior0[RRF_GCMS_superior0 > 0]

UF <- 1/(1-rsd(RRF_GCMS_superior0))

for (i in 1:N_sim) {

## 1) Concentration around the AET
base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
## A) Non-Parametric Bootstrap of the GC/MS dataset (5µg/mL)
simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
## V RRF < 1

exp_conc <- base_conc_sim * simulated_RRF
corrected_conc <- exp_conc
condition <- (simulated_RRF < 1)
corrected_conc[condition] <- exp_conc[condition] * UF

total_FN <- total_FN + sum((base_conc_sim > AET) & (corrected_conc < AET), na.rm = TRUE)
total_FP <- total_FP + sum((base_conc_sim < AET) & (corrected_conc > AET), na.rm = TRUE)
total_TP <- total_TP + sum((base_conc_sim > AET) & (corrected_conc > AET), na.rm = TRUE)
total_TN <- total_TN + sum((base_conc_sim < AET) & (corrected_conc < AET), na.rm = TRUE)
}

FN_Rate_final <- total_FN / total_points
FP_Rate_final <- total_FP / total_points
TP_Rate_final <- total_TP / total_points
TN_Rate_final <- total_TN / total_points
precision <- TP_Rate_final / (TP_Rate_final+FP_Rate_final)
recall <- TP_Rate_final / (TP_Rate_final + FN_Rate_final)
F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

summary_table <- rbind(summary_table, data.frame(
  Method = "RRF<1",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = sqrt(FN_Rate_final * (1 - FN_Rate_final) / total_points),
  FP_SD = sqrt(FP_Rate_final * (1 - FP_Rate_final) / total_points),
  precision = precision,
  recall = recall,
  F1 = F1,
  f_betal = f_beta
))
```

## optimized UF?

``` r
set.seed(123)

target_recall <- 0.95
found <- FALSE
optimal_UF <- NA

N_sim <- 10000
N_extractables <- 80
total_points <- N_sim * N_extractables

RRF_GCMS_superior0 <- gcms_clap_clean$"5 µg/mL"
RRF_GCMS_superior0 <- RRF_GCMS_superior0[RRF_GCMS_superior0 > 0]

for (test_UF in seq(1, 100, by = 1)) {
  

  total_FN <- 0
  total_FP <- 0
  total_TP <- 0
  total_TN <- 0

  for (i in 1:N_sim) {
    base_conc_sim <- runif(N_extractables, min = 0.5 * AET, max = 2 * AET)
    simulated_RRF <- sample(gcms_clap_clean$"5 µg/mL", size = N_extractables, replace = FALSE)
    
    exp_conc <- base_conc_sim * simulated_RRF
    corrected_conc <- exp_conc
    
    condition <- (simulated_RRF < 1)
    
    
    corrected_conc[condition] <- exp_conc[condition] * test_UF 

    total_FN <- total_FN + sum((base_conc_sim > AET) & (corrected_conc < AET), na.rm = TRUE)
    total_FP <- total_FP + sum((base_conc_sim < AET) & (corrected_conc > AET), na.rm = TRUE)
    total_TP <- total_TP + sum((base_conc_sim > AET) & (corrected_conc > AET), na.rm = TRUE)
    total_TN <- total_TN + sum((base_conc_sim < AET) & (corrected_conc < AET), na.rm = TRUE)
  }


  current_recall <- total_TP / (total_TP + total_FN)
  
  print(paste("Testing UF:", test_UF, "-> Recall:", round(current_recall, 4))) 

  if(current_recall >= target_recall) {
    optimal_UF <- test_UF
    found <- TRUE
    break 
  }
}
```

    [1] "Testing UF: 1 -> Recall: 0.1853"
    [1] "Testing UF: 2 -> Recall: 0.3722"
    [1] "Testing UF: 3 -> Recall: 0.4846"
    [1] "Testing UF: 4 -> Recall: 0.5656"
    [1] "Testing UF: 5 -> Recall: 0.6176"
    [1] "Testing UF: 6 -> Recall: 0.657"
    [1] "Testing UF: 7 -> Recall: 0.6823"
    [1] "Testing UF: 8 -> Recall: 0.6992"
    [1] "Testing UF: 9 -> Recall: 0.7115"
    [1] "Testing UF: 10 -> Recall: 0.7194"
    [1] "Testing UF: 11 -> Recall: 0.723"
    [1] "Testing UF: 12 -> Recall: 0.7259"
    [1] "Testing UF: 13 -> Recall: 0.7288"
    [1] "Testing UF: 14 -> Recall: 0.731"
    [1] "Testing UF: 15 -> Recall: 0.7319"
    [1] "Testing UF: 16 -> Recall: 0.7356"
    [1] "Testing UF: 17 -> Recall: 0.7369"
    [1] "Testing UF: 18 -> Recall: 0.739"
    [1] "Testing UF: 19 -> Recall: 0.7411"
    [1] "Testing UF: 20 -> Recall: 0.7417"
    [1] "Testing UF: 21 -> Recall: 0.7437"
    [1] "Testing UF: 22 -> Recall: 0.7453"
    [1] "Testing UF: 23 -> Recall: 0.7466"
    [1] "Testing UF: 24 -> Recall: 0.7484"
    [1] "Testing UF: 25 -> Recall: 0.7494"
    [1] "Testing UF: 26 -> Recall: 0.7512"
    [1] "Testing UF: 27 -> Recall: 0.7521"
    [1] "Testing UF: 28 -> Recall: 0.7535"
    [1] "Testing UF: 29 -> Recall: 0.7541"
    [1] "Testing UF: 30 -> Recall: 0.7553"
    [1] "Testing UF: 31 -> Recall: 0.7563"
    [1] "Testing UF: 32 -> Recall: 0.7567"
    [1] "Testing UF: 33 -> Recall: 0.7571"
    [1] "Testing UF: 34 -> Recall: 0.7566"
    [1] "Testing UF: 35 -> Recall: 0.7572"
    [1] "Testing UF: 36 -> Recall: 0.7576"
    [1] "Testing UF: 37 -> Recall: 0.7592"
    [1] "Testing UF: 38 -> Recall: 0.7598"
    [1] "Testing UF: 39 -> Recall: 0.761"
    [1] "Testing UF: 40 -> Recall: 0.7621"
    [1] "Testing UF: 41 -> Recall: 0.7621"
    [1] "Testing UF: 42 -> Recall: 0.7638"
    [1] "Testing UF: 43 -> Recall: 0.7635"
    [1] "Testing UF: 44 -> Recall: 0.764"
    [1] "Testing UF: 45 -> Recall: 0.7653"
    [1] "Testing UF: 46 -> Recall: 0.7656"
    [1] "Testing UF: 47 -> Recall: 0.7659"
    [1] "Testing UF: 48 -> Recall: 0.7668"
    [1] "Testing UF: 49 -> Recall: 0.7669"
    [1] "Testing UF: 50 -> Recall: 0.7671"
    [1] "Testing UF: 51 -> Recall: 0.7675"
    [1] "Testing UF: 52 -> Recall: 0.7673"
    [1] "Testing UF: 53 -> Recall: 0.7674"
    [1] "Testing UF: 54 -> Recall: 0.7681"
    [1] "Testing UF: 55 -> Recall: 0.7683"
    [1] "Testing UF: 56 -> Recall: 0.7683"
    [1] "Testing UF: 57 -> Recall: 0.7684"
    [1] "Testing UF: 58 -> Recall: 0.7693"
    [1] "Testing UF: 59 -> Recall: 0.7692"
    [1] "Testing UF: 60 -> Recall: 0.7689"
    [1] "Testing UF: 61 -> Recall: 0.7702"
    [1] "Testing UF: 62 -> Recall: 0.7699"
    [1] "Testing UF: 63 -> Recall: 0.7701"
    [1] "Testing UF: 64 -> Recall: 0.7703"
    [1] "Testing UF: 65 -> Recall: 0.7707"
    [1] "Testing UF: 66 -> Recall: 0.7707"
    [1] "Testing UF: 67 -> Recall: 0.771"
    [1] "Testing UF: 68 -> Recall: 0.7716"
    [1] "Testing UF: 69 -> Recall: 0.7717"
    [1] "Testing UF: 70 -> Recall: 0.7714"
    [1] "Testing UF: 71 -> Recall: 0.7719"
    [1] "Testing UF: 72 -> Recall: 0.7716"
    [1] "Testing UF: 73 -> Recall: 0.7711"
    [1] "Testing UF: 74 -> Recall: 0.7712"
    [1] "Testing UF: 75 -> Recall: 0.772"
    [1] "Testing UF: 76 -> Recall: 0.7719"
    [1] "Testing UF: 77 -> Recall: 0.7714"
    [1] "Testing UF: 78 -> Recall: 0.7712"
    [1] "Testing UF: 79 -> Recall: 0.7719"
    [1] "Testing UF: 80 -> Recall: 0.7721"
    [1] "Testing UF: 81 -> Recall: 0.7715"
    [1] "Testing UF: 82 -> Recall: 0.7715"
    [1] "Testing UF: 83 -> Recall: 0.7719"
    [1] "Testing UF: 84 -> Recall: 0.7716"
    [1] "Testing UF: 85 -> Recall: 0.772"
    [1] "Testing UF: 86 -> Recall: 0.7717"
    [1] "Testing UF: 87 -> Recall: 0.7719"
    [1] "Testing UF: 88 -> Recall: 0.7721"
    [1] "Testing UF: 89 -> Recall: 0.7721"
    [1] "Testing UF: 90 -> Recall: 0.7715"
    [1] "Testing UF: 91 -> Recall: 0.7715"
    [1] "Testing UF: 92 -> Recall: 0.772"
    [1] "Testing UF: 93 -> Recall: 0.7713"
    [1] "Testing UF: 94 -> Recall: 0.7719"
    [1] "Testing UF: 95 -> Recall: 0.7719"
    [1] "Testing UF: 96 -> Recall: 0.7721"
    [1] "Testing UF: 97 -> Recall: 0.7719"
    [1] "Testing UF: 98 -> Recall: 0.7715"
    [1] "Testing UF: 99 -> Recall: 0.7725"
    [1] "Testing UF: 100 -> Recall: 0.7725"

``` r
print(paste("Optimal UF for 95% Recall:", optimal_UF))
```

    [1] "Optimal UF for 95% Recall: NA"

``` r
FN_Rate_final <- total_FN / total_points
FP_Rate_final <- total_FP / total_points
TP_Rate_final <- total_TP / total_points
TN_Rate_final <- total_TN / total_points
precision <- TP_Rate_final / (TP_Rate_final+FP_Rate_final)
recall <- TP_Rate_final / (TP_Rate_final + FN_Rate_final)
F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)


summary_table <- rbind(summary_table, data.frame(
  Method = "RRF_optimized",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = sqrt(FN_Rate_final * (1 - FN_Rate_final) / total_points),
  FP_SD = sqrt(FP_Rate_final * (1 - FP_Rate_final) / total_points),
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
    1      No Correction 0.5435425 0.01787625 0.0005568932 0.0001481414 0.8736549
    2             UF (2) 0.4186413 0.06090000 0.0005515668 0.0002673742 0.8031754
    3             UF (4) 0.2893275 0.12409625 0.0005069728 0.0003686061 0.7523746
    4             UF (5) 0.2550050 0.14462625 0.0004873108 0.0003932390 0.7399487
    5            UF (10) 0.1866387 0.20618625 0.0004356098 0.0004523183 0.6993274
    6           UF (RSD) 0.6664125 0.00000000 0.0005271467 0.0000000000       NaN
    7  Percentile (16th) 0.1519938 0.25648875 0.0004013908 0.0004882395 0.6676095
    8             RRFlow 0.0000000 0.08659000 0.0000000000 0.0003144284 0.8851201
    9              RRF<1 0.2217487 0.15294125 0.0004644570 0.0004024149 0.7443934
    10     RRF_optimized 0.1518087 0.24027500 0.0004011902 0.0004776805 0.6820629
          recall        F1   f_betal
    1  0.1852815 0.3057258 0.1910719
    2  0.4774874 0.5989182 0.4850524
    3  0.6427958 0.6932819 0.6464168
    4  0.6854433 0.7116539 0.6873908
    5  0.7898279 0.7418276 0.7859161
    6  0.0000000       NaN       NaN
    7  0.7721758 0.7160955 0.7675520
    8  1.0000000 0.9390596 0.9950329
    9  0.6676197 0.7039194 0.6702786
    10 0.7724911 0.7244661 0.7685719

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

total_FN <- total_FN + sum((base_conc_sim > AET) & (corrected_conc < AET), na.rm = TRUE)
total_FP <- total_FP + sum((base_conc_sim < AET) & (corrected_conc > AET), na.rm = TRUE)
total_TP <- total_TP + sum((base_conc_sim > AET) & (corrected_conc > AET), na.rm = TRUE)
total_TN <- total_TN + sum((base_conc_sim < AET) & (corrected_conc < AET), na.rm = TRUE)
}

FN_Rate_final <- total_FN / total_points
FP_Rate_final <- total_FP / total_points
TP_Rate_final <- total_TP / total_points
TN_Rate_final <- total_TN / total_points
precision <- TP_Rate_final / (TP_Rate_final+FP_Rate_final)
recall <- TP_Rate_final / (TP_Rate_final + FN_Rate_final)
F1 <- (2 * precision * recall / (precision + recall))
beta <- 5
f_beta <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)


summary_table <- rbind(summary_table, data.frame(
  Method = "Machine Learning",
  FN_Rate = FN_Rate_final,
  FP_Rate = FP_Rate_final,
  FN_SD = sqrt(FN_Rate_final * (1 - FN_Rate_final) / total_points),
  FP_SD = sqrt(FP_Rate_final * (1 - FP_Rate_final) / total_points),
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
    1      No Correction 0.5435425 0.01787625 0.0005568932 0.0001481414 0.8736549
    2             UF (2) 0.4186413 0.06090000 0.0005515668 0.0002673742 0.8031754
    3             UF (4) 0.2893275 0.12409625 0.0005069728 0.0003686061 0.7523746
    4             UF (5) 0.2550050 0.14462625 0.0004873108 0.0003932390 0.7399487
    5            UF (10) 0.1866387 0.20618625 0.0004356098 0.0004523183 0.6993274
    6           UF (RSD) 0.6664125 0.00000000 0.0005271467 0.0000000000       NaN
    7  Percentile (16th) 0.1519938 0.25648875 0.0004013908 0.0004882395 0.6676095
    8             RRFlow 0.0000000 0.08659000 0.0000000000 0.0003144284 0.8851201
    9              RRF<1 0.2217487 0.15294125 0.0004644570 0.0004024149 0.7443934
    10     RRF_optimized 0.1518087 0.24027500 0.0004011902 0.0004776805 0.6820629
    11  Machine Learning 0.6663025 0.00000000 0.0005271900 0.0000000000       NaN
          recall        F1   f_betal
    1  0.1852815 0.3057258 0.1910719
    2  0.4774874 0.5989182 0.4850524
    3  0.6427958 0.6932819 0.6464168
    4  0.6854433 0.7116539 0.6873908
    5  0.7898279 0.7418276 0.7859161
    6  0.0000000       NaN       NaN
    7  0.7721758 0.7160955 0.7675520
    8  1.0000000 0.9390596 0.9950329
    9  0.6676197 0.7039194 0.6702786
    10 0.7724911 0.7244661 0.7685719
    11 0.0000000       NaN       NaN

``` r
hist(simulated_RRF)
```

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-27-1.png)

``` r
summary_table
```

                  Method   FN_Rate    FP_Rate        FN_SD        FP_SD precision
    1      No Correction 0.5435425 0.01787625 0.0005568932 0.0001481414 0.8736549
    2             UF (2) 0.4186413 0.06090000 0.0005515668 0.0002673742 0.8031754
    3             UF (4) 0.2893275 0.12409625 0.0005069728 0.0003686061 0.7523746
    4             UF (5) 0.2550050 0.14462625 0.0004873108 0.0003932390 0.7399487
    5            UF (10) 0.1866387 0.20618625 0.0004356098 0.0004523183 0.6993274
    6           UF (RSD) 0.6664125 0.00000000 0.0005271467 0.0000000000       NaN
    7  Percentile (16th) 0.1519938 0.25648875 0.0004013908 0.0004882395 0.6676095
    8             RRFlow 0.0000000 0.08659000 0.0000000000 0.0003144284 0.8851201
    9              RRF<1 0.2217487 0.15294125 0.0004644570 0.0004024149 0.7443934
    10     RRF_optimized 0.1518087 0.24027500 0.0004011902 0.0004776805 0.6820629
    11  Machine Learning 0.6663025 0.00000000 0.0005271900 0.0000000000       NaN
          recall        F1   f_betal
    1  0.1852815 0.3057258 0.1910719
    2  0.4774874 0.5989182 0.4850524
    3  0.6427958 0.6932819 0.6464168
    4  0.6854433 0.7116539 0.6873908
    5  0.7898279 0.7418276 0.7859161
    6  0.0000000       NaN       NaN
    7  0.7721758 0.7160955 0.7675520
    8  1.0000000 0.9390596 0.9950329
    9  0.6676197 0.7039194 0.6702786
    10 0.7724911 0.7244661 0.7685719
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
  "RRF<1",
  "Machine Learning")

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

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-29-1.png)

``` r
summary_table %>%
  select(Method, precision, recall, F1, f_betal)
```

                  Method precision    recall        F1   f_betal
    1      No Correction 0.8736549 0.1852815 0.3057258 0.1910719
    2             UF (2) 0.8031754 0.4774874 0.5989182 0.4850524
    3             UF (4) 0.7523746 0.6427958 0.6932819 0.6464168
    4             UF (5) 0.7399487 0.6854433 0.7116539 0.6873908
    5            UF (10) 0.6993274 0.7898279 0.7418276 0.7859161
    6           UF (RSD)       NaN 0.0000000       NaN       NaN
    7  Percentile (16th) 0.6676095 0.7721758 0.7160955 0.7675520
    8             RRFlow 0.8851201 1.0000000 0.9390596 0.9950329
    9              RRF<1 0.7443934 0.6676197 0.7039194 0.6702786
    10     RRF_optimized 0.6820629 0.7724911 0.7244661 0.7685719
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

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-31-1.png)

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

![](GC_MS_5_1A_files/figure-commonmark/unnamed-chunk-31-2.png)
