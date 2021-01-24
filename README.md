# What it is:
Praxis research work for University of Latvia working at SIA "Creamfinance Latvia" about class imbalance for logistic regression.

# How it works:
1. Run `Real_data.R` to run the experiments (you need to manually change the ID of the data used). It will save a large amount of data about the experiments in files ending with "_res.RDS" (for the data used in the paper it was around 2GB). The script can utilize multiple CPU's and it is recommended to have at least 4GB RAM available.
2. Run `diff.R` to extract main results needed from result RDS files and save it in files ending with "_diff.RDS".
3. Run `analysis_lv.R` to generate plots (in /images/analysis_lv/ by default) and tables.

# Other information:
* `functions.R` contains functions used in other script files.
* `analysis_lv.R` has UTF-8 encoding (it contains some latvian words). 

## sessionInfo()

```
R version 3.6.3 (2020-02-29)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: CentOS Linux 8 (Core)

Matrix products: default
BLAS:   /usr/lib64/R/lib/libRblas.so
LAPACK: /usr/lib64/R/lib/libRlapack.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods
[8] base

other attached packages:
 [1] DMwR_0.4.1       doSNOW_1.0.18    snow_0.4-3       iterators_1.0.12
 [5] foreach_1.4.8    pROC_1.16.1      glmnet_4.0-2     Matrix_1.2-18
 [9] caret_6.0-85     lattice_0.20-38  forcats_0.5.0    stringr_1.4.0
[13] dplyr_0.8.4      purrr_0.3.3      readr_1.3.1      tidyr_1.0.2
[17] tibble_2.1.3     ggplot2_3.3.0    tidyverse_1.3.0

loaded via a namespace (and not attached):
 [1] httr_1.4.1           jsonlite_1.6.1       splines_3.6.3
 [4] gtools_3.8.1         prodlim_2019.11.13   modelr_0.1.6
 [7] assertthat_0.2.1     TTR_0.23-6           stats4_3.6.3
[10] cellranger_1.1.0     ipred_0.9-9          pillar_1.4.3
[13] backports_1.1.5      glue_1.3.1           rvest_0.3.5
[16] colorspace_1.4-1     recipes_0.1.9        plyr_1.8.6
[19] timeDate_3043.102    pkgconfig_2.0.3      broom_0.5.5
[22] haven_2.2.0          scales_1.1.0         gdata_2.18.0
[25] gower_0.2.1          lava_1.6.7           generics_0.0.2
[28] withr_2.1.2          ROCR_1.0-7           nnet_7.3-12
[31] quantmod_0.4-16      cli_2.0.2            survival_3.1-8
[34] magrittr_1.5         crayon_1.3.4         readxl_1.3.1
[37] fs_1.3.2             fansi_0.4.1          gplots_3.0.3
[40] nlme_3.1-144         MASS_7.3-51.5        xts_0.12-0
[43] xml2_1.2.2           class_7.3-15         tools_3.6.3
[46] data.table_1.12.8    hms_0.5.3            lifecycle_0.1.0
[49] munsell_0.5.0        reprex_0.3.0         compiler_3.6.3
[52] caTools_1.18.0       rlang_0.4.5          rstudioapi_0.11
[55] bitops_1.0-6         gtable_0.3.0         ModelMetrics_1.2.2.1
[58] codetools_0.2-16     curl_4.3             abind_1.4-5
[61] DBI_1.1.0            reshape2_1.4.3       R6_2.4.1
[64] zoo_1.8-7            lubridate_1.7.4      KernSmooth_2.23-16
[67] shape_1.4.5          stringi_1.4.6        parallel_3.6.3
[70] Rcpp_1.0.3           vctrs_0.2.3          rpart_4.1-15
[73] dbplyr_1.4.2         tidyselect_1.0.0
```
