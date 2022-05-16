# Volker, Buskens & Raub “The future is made today: Concerns for reputation foster trust and cooperation”

This repository contains all data and code to reproduce the analyses.
Per data set, one script is used for reproducing the original findings
and analyzing the data for the current paper. This folder contains four
subdirectories, of which more information is included below. The data
sets are included in the folder `data`, the analysis scripts and results
objects are in the folder `R`. All objects are named such that they
correspond to the study of which the results emerged.

| Folder      | Content                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| thesis      | Contains all files belonging to the manuscript <br> - References in `thesis.bib` <br> - Text and code to create tables in `manuscript_volker.Rmd` <br> - Output document `manuscript_volker.pdf` <br> - Required `Latex` packages in `preamble.tex` <br> - Required frontpage in `frontmatter.tex`                                                                                                                                                                               |
| R           | All scripts to obtain the individual study results. <br> `[study].R` - These scripts reproduce the original findings (sometimes, `Stata` is required. If this is the case, the lines that should be run in `Stata` are commented out in the `R` code. <br> `create-tables.R` - Aggregates the results of the individual studies and reproduces the Tables in the manuscript. <br> - The folder `results` contains the workspaces resulting from the individual analysis scripts. |
| data        | Contains the data files that I obtained from the authors to reproduce these analyses.                                                                                                                                                                                                                                                                                                                                                                                            |
| other-files | Rather self explanatory, this folder contains random files and thoughts that developed somewhere during the project (this ranges from presentations about the topic, the initial project proposal, an intermediate report and some meeting notes).                                                                                                                                                                                                                               |

All data used in this paper stems from a secondary source. Some authors
published the original data on an online repository, while other authors
transferred the data through email. Because these secondary sources are
used, data is not shared with open access, but I will contact the
authors to ask whether the data can be made freely accessible. Ethical
approval has been granted by the FETC at Utrecht University (application
number 21-1922).

# Machine and package info

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value                       
     version  R version 4.1.0 (2021-05-18)
     os       macOS Big Sur 10.16         
     system   x86_64, darwin17.0          
     ui       X11                         
     language (EN)                        
     collate  en_US.UTF-8                 
     ctype    en_US.UTF-8                 
     tz       Europe/Amsterdam            
     date     2022-05-16                  

    ─ Packages ───────────────────────────────────────────────────────────────────
     package     * version  date       lib source        
     assertthat    0.2.1    2019-03-21 [1] CRAN (R 4.1.0)
     bain        * 0.2.4    2020-03-09 [1] CRAN (R 4.1.0)
     bdsmatrix     1.3-4    2020-01-13 [1] CRAN (R 4.1.0)
     boot          1.3-28   2021-05-03 [1] CRAN (R 4.1.0)
     cli           3.2.0    2022-02-14 [1] CRAN (R 4.1.2)
     collapse      1.7.6    2022-02-11 [1] CRAN (R 4.1.2)
     colorspace    2.0-3    2022-02-21 [1] CRAN (R 4.1.2)
     crayon        1.5.0    2022-02-14 [1] CRAN (R 4.1.2)
     DBI           1.1.1    2021-01-15 [1] CRAN (R 4.1.0)
     digest        0.6.29   2021-12-01 [1] CRAN (R 4.1.0)
     dplyr       * 1.0.8    2022-02-08 [1] CRAN (R 4.1.2)
     ellipsis      0.3.2    2021-04-29 [1] CRAN (R 4.1.0)
     evaluate      0.15     2022-02-18 [1] CRAN (R 4.1.2)
     extraDistr    1.9.1    2020-09-07 [1] CRAN (R 4.1.0)
     fansi         1.0.3    2022-03-24 [1] CRAN (R 4.1.0)
     fastmap       1.1.0    2021-01-25 [1] CRAN (R 4.1.0)
     Formula       1.2-4    2020-10-16 [1] CRAN (R 4.1.0)
     generics      0.1.2    2022-01-31 [1] CRAN (R 4.1.2)
     ggplot2     * 3.3.5    2021-06-25 [1] CRAN (R 4.1.0)
     glue          1.6.2    2022-02-24 [1] CRAN (R 4.1.2)
     gtable        0.3.0    2019-03-25 [1] CRAN (R 4.1.0)
     htmltools     0.5.2    2021-08-25 [1] CRAN (R 4.1.0)
     knitr         1.38     2022-03-25 [1] CRAN (R 4.1.2)
     lattice       0.20-44  2021-05-02 [1] CRAN (R 4.1.0)
     lavaan        0.6-8    2021-03-10 [1] CRAN (R 4.1.0)
     lifecycle     1.0.1    2021-09-24 [1] CRAN (R 4.1.0)
     lme4        * 1.1-27.1 2021-06-22 [1] CRAN (R 4.1.0)
     lmtest      * 0.9-38   2020-09-09 [1] CRAN (R 4.1.0)
     magrittr    * 2.0.3    2022-03-30 [1] CRAN (R 4.1.2)
     MASS          7.3-54   2021-05-03 [1] CRAN (R 4.1.0)
     Matrix      * 1.3-3    2021-05-04 [1] CRAN (R 4.1.0)
     maxLik        1.5-2    2021-07-26 [1] CRAN (R 4.1.0)
     minqa         1.2.4    2014-10-09 [1] CRAN (R 4.1.0)
     miscTools     0.6-26   2019-12-08 [1] CRAN (R 4.1.0)
     mnormt        2.0.2    2020-09-01 [1] CRAN (R 4.1.0)
     munsell       0.5.0    2018-06-12 [1] CRAN (R 4.1.0)
     nlme          3.1-152  2021-02-04 [1] CRAN (R 4.1.0)
     nloptr        1.2.2.2  2020-07-02 [1] CRAN (R 4.1.0)
     pbivnorm      0.6.0    2015-01-23 [1] CRAN (R 4.1.0)
     pillar        1.7.0    2022-02-01 [1] CRAN (R 4.1.2)
     pkgconfig     2.0.3    2019-09-22 [1] CRAN (R 4.1.0)
     plm         * 2.6-1    2022-03-05 [1] CRAN (R 4.1.2)
     purrr       * 0.3.4    2020-04-17 [1] CRAN (R 4.1.0)
     R6            2.5.1    2021-08-19 [1] CRAN (R 4.1.0)
     rbibutils     2.2.7    2021-12-07 [1] CRAN (R 4.1.0)
     Rcpp          1.0.8.3  2022-03-17 [1] CRAN (R 4.1.2)
     Rdpack        2.2      2022-03-19 [1] CRAN (R 4.1.0)
     rlang         1.0.2    2022-03-04 [1] CRAN (R 4.1.0)
     rmarkdown     2.14     2022-04-25 [1] CRAN (R 4.1.0)
     rstudioapi    0.13     2020-11-12 [1] CRAN (R 4.1.0)
     sandwich    * 3.0-1    2021-05-18 [1] CRAN (R 4.1.0)
     scales        1.1.1    2020-05-11 [1] CRAN (R 4.1.0)
     sessioninfo   1.1.1    2018-11-05 [1] CRAN (R 4.1.0)
     stringi       1.7.6    2021-11-29 [1] CRAN (R 4.1.0)
     stringr       1.4.0    2019-02-10 [1] CRAN (R 4.1.0)
     tibble        3.1.6    2021-11-07 [1] CRAN (R 4.1.0)
     tidyr       * 1.2.0    2022-02-01 [1] CRAN (R 4.1.2)
     tidyselect    1.1.2    2022-02-21 [1] CRAN (R 4.1.2)
     tmvnsim       1.0-2    2016-12-15 [1] CRAN (R 4.1.0)
     trend       * 1.1.4    2020-09-17 [1] CRAN (R 4.1.0)
     utf8          1.2.2    2021-07-24 [1] CRAN (R 4.1.0)
     vctrs         0.3.8    2021-04-29 [1] CRAN (R 4.1.0)
     withr         2.5.0    2022-03-03 [1] CRAN (R 4.1.0)
     xfun          0.30     2022-03-02 [1] CRAN (R 4.1.2)
     yaml          2.3.5    2022-02-21 [1] CRAN (R 4.1.2)
     zoo         * 1.8-9    2021-03-09 [1] CRAN (R 4.1.0)

    [1] /Library/Frameworks/R.framework/Versions/4.1/Resources/library
