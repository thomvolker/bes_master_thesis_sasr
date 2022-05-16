

# --------------------------------- METADATA -----------------------------------
# Thom Benjamin Volker
# 16-05-2022
# Tables in Volker, Buskens, Raub "The future is made today: Concerns for 
# reputation foster trust and cooperation"
# ______________________________________________________________________________


# -------------------------- AGGREGATION OF RESULTS ----------------------------
#
#   THIS CODE IS ALSO USED IN THE thesis_volker.Rmd FILE, CONTAINING THE 
#   MANUSCRIPT AND THE CODE TO OBTAIN THE CREATED TABLES. YET, TO FACILITATE
#   REPRODUCIBILITY WITHOUT HAVING TO GO THROUGH THE ENTIRE MANUSCRIPT, THE
#   RESULTS ARE ALSO REPORTED HERE. 
#
# ______________________________________________________________________________


# ---------------------- LOAD INDIVIDUAL STUDY RESULTS -------------------------

load("R/results/bolton.RData")
load("R/results/duffy_xie_lee.RData")
load("R/results/seinen.RData")
load("R/results/corten.RData")
load("R/results/veer.RData")
load("R/results/miltenburg.RData")
load("R/results/frey.RData")
load("R/results/barrera.RData")


# -------- STORE BAYES FACTORS FOR THE DIFFERENT SETS OF STUDIES TOGETHER ------

set1_tf_iu <- c(tf_bolton_bf$fit$BF.u[1],       # TRUSTFULNESS RANDOM PARTNER 
                tf_duffy_min_bf$fit$BF.u[1],    # MATCHING; UNCONSTRAINED
                tf_duffy_full_bf$fit$BF.u[1])   # ALTERNATIVE HYPOTHESIS


set1_tf_ic <- c(tf_bolton_bf$fit$BF.c[1],       # TRUSTFULNESS RANDOM PARTNER
                tf_duffy_min_bf$fit$BF.c[1],    # MATCHING; COMPLEMENT
                tf_duffy_full_bf$fit$BF.c[1])   # ALTERNATIVE HYPOTHESIS


set1_tw_iu <- c(tw_bolton_bf$fit$BF.u[1],       # TRUSTWORTHINESS RANDOM PARTNER
                tw_duffy_min_bf$fit$BF.u[1],    # MATCHING; UNCONSTRAINED
                tw_duffy_full_bf$fit$BF.u[1])   # ALTERNATIVE HYPOTHESIS


set1_tw_ic <- c(tw_bolton_bf$fit$BF.c[1],       # TRUSTWORTHINESS RANDOM PARTNER
                tw_duffy_min_bf$fit$BF.c[1],    # MATCHING; COMPLEMENT
                tw_duffy_full_bf$fit$BF.c[1])   # ALTERNATIVE HYPOTHESIS


set1_coop_iu <- c(coop_seinen_bf$fit$BF.u[1],   # COOPERATION; UNCONSTRAINED
                  coop_corten_bf$fit$BF.u[1])   # ALTERNATIVE HYPOTHESIS


set1_coop_ic <- c(coop_seinen_bf$fit$BF.c[1],   # COOPERATION; COMPLEMENT
                  coop_corten_bf$fit$BF.c[1])   # ALTERNATIVE HYPOTHESIS


set2_tf_iu <- c(tf_veer_bf$fit$BF.u[1],         # TRUSTFULNESS TRIADS; 
                tf_miltenburg_bf$fit$BF.u[1],   # UNCONSTRAINED ALTERNATIVE
                tf_frey_bf$fit$BF.u[1],         # HYPOTHESIS
                tf_barrera_bf$fit$BF.u[1])


set2_tf_ic <- c(tf_veer_bf$fit$BF.c[1],         # TRUSTFULNESS TRIADS;
                tf_miltenburg_bf$fit$BF.c[1],   # COMPLEMENT ALTERNATIVE 
                tf_frey_bf$fit$BF.c[1],         # HYPOTHESIS
                tf_barrera_bf$fit$BF.c[1])


set2_tw_iu <- c(tw_veer_bf$fit$BF.u[1],         # TRUSTWORTHINESS TRIADS;
                tw_miltenburg_bf$fit$BF.u[1],   # UNCONSTRAINED ALTERNATIVE
                tw_frey_bf$fit$BF.u[1],         # HYPOTHESIS
                tw_barrera_bf$fit$BF.u[1])


set2_tw_ic <- c(tw_veer_bf$fit$BF.c[1],         # TRUSTWORTHINESS TRIADS;
                tw_miltenburg_bf$fit$BF.c[1],   # COMPLEMENT ALTERNATIVE
                tw_frey_bf$fit$BF.c[1],         # HYPOTHESIS
                tw_barrera_bf$fit$BF.c[1])



# ----------- STORE BAYES FACTORS AFTER ROBUSTNESS ANALYSIS PER SET ------------

set1_tf_iu2 <- c(tf_bolton_bf$fit$BF.u[1],           # TRUSTFULNESS RANDOM PARTNER
                 tf_duffy_min_glmer_bf$fit$BF.u[1],  # MATCHING; UNCONSTRAINED
                 tf_duffy_full_glmer_bf$fit$BF.u[1]) # ALTERNATIVE HYPOTHESIS


set1_tf_ic2 <- c(tf_bolton_bf$fit$BF.c[1],           # TRUSTFULNESS RANDOM PARTNER
                 tf_duffy_min_glmer_bf$fit$BF.c[1],  # MATCHING; COMPLEMENT
                 tf_duffy_full_glmer_bf$fit$BF.c[1]) # ALTERNATIVE HYPOTHESIS


set1_tw_iu2 <- c(tw_bolton_bf$fit$BF.u[1],           # TRUSTWORTHINESS RANDOM
                 tw_duffy_min_glmer_bf$fit$BF.u[1],  # PARTNER MATCHING; UNCONSTRAINED
                 tw_duffy_full_glmer_bf$fit$BF.u[1]) # ALTERNATIVE HYPOTHESIS


set1_tw_ic2 <- c(tw_bolton_bf$fit$BF.c[1],           # TRUSTWORTHINESS RANDOM
                 tw_duffy_min_glmer_bf$fit$BF.c[1],  # PARTNER MATCHING; COMPLEMENT
                 tw_duffy_full_glmer_bf$fit$BF.c[1]) # ALTERNATIVE HYPOTHESIS


set1_coop_iu2 <- c(coop_seinen_bf$fit$BF.u[1],       # COOPERATION; UNCONSTRAINED
                   coop_corten_glmer_bf$fit$BF.u[1]) # ALTERNATIVE HYPOTHESIS


set1_coop_ic2 <- c(coop_seinen_bf$fit$BF.c[1],       # COOPERATION; COMPLEMENT 
                   coop_corten_glmer_bf$fit$BF.c[1]) # ALTERNATIVE HYPOTHESIS


set2_tf_iu2 <- c(tf_veer_bf$fit$BF.u[1],             # TRUSTFULNESS TRIADS; 
                 tf_miltenburg_glmer_bf$fit$BF.u[1], # UNCONSTRAINED ALTERNATIVE
                 tf_frey_glmer_bf$fit$BF.u[1],       # HYPOTHESIS
                 tf_barrera_lmer_bf$fit$BF.u[1])


set2_tf_ic2 <- c(tf_veer_bf$fit$BF.c[1],             # TRUSTFULNESS TRIADS;
                 tf_miltenburg_glmer_bf$fit$BF.c[1], # COMPLEMENT ALTERNATIVE 
                 tf_frey_glmer_bf$fit$BF.c[1],       # HYPOTHESIS
                 tf_barrera_bf$fit$BF.c[1])


set2_tw_iu2 <- c(tw_veer_bf$fit$BF.u[1],             # TRUSTWORTHINESS TRIADS;
                 tw_miltenburg_glmer_bf$fit$BF.u[1], # UNCONSTRAINED ALTERNATIVE
                 tw_frey_glmer_bf$fit$BF.u[1],       # HYPOTHESIS
                 tw_barrera_bf$fit$BF.u[1])


set2_tw_ic2 <- c(tw_veer_bf$fit$BF.c[1],             # TRUSTWORTHINESS TRIADS;
                 tw_miltenburg_glmer_bf$fit$BF.c[1], # COMPLEMENT ALTERNATIVE
                 tw_frey_glmer_bf$fit$BF.c[1],       # HYPOTHESIS
                 tw_barrera_lmer_bf$fit$BF.c[1])


# ------------------------- TABLE FORMAT FUNCTION ------------------------------

f <- function(x) {                                        # USE SCIENTIFIC NOTATION
  ifelse(x >= 1000,                                       # FOR NUMBERS GREATER THAN
         formatC(x, format = "e", digits = 2),            # 1000; AND ROUND NUMBERS
         format(x, digit=1, nsmall=2, scientific=FALSE))  # SMALLER THAN 1000 AT TWO
}                                                         # DECIMALS, OR THE FIRST
                                                          # NON-ZERO DECIMAL

# --------------------------------- TABLE 2 ------------------------------------

tibble(`Study` = c("(ref:bolton)", " ",
                   "(ref:duffy)", "No-NetMin",
                   "(ref:duffy)", "No-NetFull",
                   "(ref:veer)", " ",
                   "(ref:miltenburg)", " ",
                   "(ref:frey)", " ",
                   "(ref:barrera)", " ",
                   "(ref:seinen)",
                   "(ref:corten)"),
       `Outcome` = c("Trustfulness", "Trustworthiness",
                     "Trustfulness", "Trustworthiness",
                     "Trustfulness", "Trustworthiness",
                     "Trustfulness", "Trustworthiness",
                     "Trustfulness", "Trustworthiness",
                     "Trustfulness", "Trustworthiness",
                     "Trustfulness", "Trustworthiness",
                     "Cooperation (helping)",
                     "Cooperation"),
       `No Network` = c(tf_bolton_prop$tf[1],
                        tw_bolton_prop$tw[1],
                        tf_duffy_min_prop$tf[1],
                        tw_duffy_min_prop$tw[1],
                        tf_duffy_full_prop$tf[1], 
                        tw_duffy_full_prop$tw[1],
                        tf_veer_prop$tf[1],
                        tw_veer_prop$tw[1],
                        tf_miltenburg_prop$tf[1],
                        tw_miltenburg_prop$tw[1],
                        tf_frey_prop$tf[1],
                        tw_frey_prop$tw[1],
                        tf_barrera_prop$tf[1],
                        tw_barrera_prop$tw[1],
                        coop_seinen_prop$coop[1],
                        coop_corten_prop$coop[1]),
       `Network` = c(tf_bolton_prop$tf[2], 
                     tw_bolton_prop$tw[2],
                     tf_duffy_min_prop$tf[2], 
                     tw_duffy_min_prop$tw[2],
                     tf_duffy_full_prop$tf[2], 
                     tw_duffy_full_prop$tw[2],
                     tf_veer_prop$tf[2],
                     tw_veer_prop$tw[2],
                     tf_miltenburg_prop$tf[2],
                     tw_miltenburg_prop$tw[2],
                     tf_frey_prop$tf[2],
                     tw_frey_prop$tw[2],
                     tf_barrera_prop$tf[2],
                     tw_barrera_prop$tw[2],
                     coop_seinen_prop$coop[2],
                     coop_corten_prop$coop[2]),
       `$BF_{i,u}$` = c(tf_bolton_bf$fit$BF.u[1],
                        tw_bolton_bf$fit$BF.u[1],
                        tf_duffy_min_bf$fit$BF.u[1], 
                        tw_duffy_min_bf$fit$BF.u[1],
                        tf_duffy_full_bf$fit$BF.u[1], 
                        tw_duffy_full_bf$fit$BF.u[1],
                        tf_veer_bf$fit$BF.u[1],
                        tw_veer_bf$fit$BF.u[1],
                        tf_miltenburg_bf$fit$BF.u[1], 
                        tw_miltenburg_bf$fit$BF.u[1],
                        tf_frey_bf$fit$BF.u[1],
                        tw_frey_bf$fit$BF.u[1],
                        tf_barrera_bf$fit$BF.u[1],
                        tw_barrera_bf$fit$BF.u[1],
                        coop_seinen_bf$fit$BF.u[1],
                        coop_corten_bf$fit$BF.u[1]),
       `$BF_{i,c}$` = c(tf_bolton_bf$fit$BF.c[1],
                        tw_bolton_bf$fit$BF.c[1],
                        tf_duffy_min_bf$fit$BF.c[1], 
                        tw_duffy_min_bf$fit$BF.c[1],
                        tf_duffy_full_bf$fit$BF.c[1],
                        tw_duffy_full_bf$fit$BF.c[1],
                        tf_veer_bf$fit$BF.c[1], 
                        tw_veer_bf$fit$BF.c[1],
                        tf_miltenburg_bf$fit$BF.c[1],
                        tw_miltenburg_bf$fit$BF.c[1],
                        tf_frey_bf$fit$BF.c[1],
                        tw_frey_bf$fit$BF.c[1],
                        tf_barrera_bf$fit$BF.c[1],
                        tw_barrera_bf$fit$BF.c[1],
                        coop_seinen_bf$fit$BF.c[1],
                        coop_corten_bf$fit$BF.c[1])
) %>%
  mutate(across(where(is.numeric), .fns = f)) # number format function



# --------------------------------- TABLE 3 ------------------------------------


tibble(` ` = c("All studies and outcomes combined",
               " ",
               "Trustfulness ($H_1$)",
               "\\hspace{8pt}Random partner matching",
               "\\hspace{8pt}Triads",
               " ",
               "Trustworthiness ($H_2$)",
               "\\hspace{8pt}Random partner matching",
               "\\hspace{8pt}Triads",
               " ",
               "Cooperation ($H_3$)"),
       `$BF_{i,u}$` = c(prod(set1_tf_iu, set1_tw_iu, set1_coop_iu,
                             set2_tf_iu, set2_tw_iu),
                        NA,
                        prod(set1_tf_iu, set2_tf_iu),
                        prod(set1_tf_iu),
                        prod(set2_tf_iu),
                        NA,
                        prod(set1_tw_iu, set2_tw_iu),
                        prod(set1_tw_iu),
                        prod(set2_tw_iu),
                        NA,
                        prod(set1_coop_iu)),
       `$PMP_{i,u}$` = c(prod(set1_tf_iu, set1_tw_iu, set1_coop_iu,
                              set2_tf_iu, set2_tw_iu) / 
                           (1 + prod(set1_tf_iu, set1_tw_iu, set1_coop_iu,
                                     set2_tf_iu, set2_tw_iu)),
                         NA,
                         prod(set1_tf_iu, set2_tf_iu) / (1 + prod(set1_tf_iu, set2_tf_iu)),
                         prod(set1_tf_iu) / (1 + prod(set1_tf_iu)),
                         prod(set2_tf_iu) / (1 + prod(set2_tf_iu)),
                         NA,
                         prod(set1_tw_iu, set2_tw_iu) / (1 + prod(set1_tw_iu, set2_tw_iu)),
                         prod(set1_tw_iu) / (1 + prod(set1_tw_iu)),
                         prod(set2_tw_iu) / (1 + prod(set2_tw_iu)),
                         NA,
                         prod(set1_coop_iu) / (1 + prod(set1_coop_iu))),
       `$BF_{i,c}$` = c(prod(set1_tf_ic, set1_tw_ic, set1_coop_ic,
                             set2_tf_ic, set2_tw_ic),
                        NA,
                        prod(set1_tf_ic, set2_tf_ic),
                        prod(set1_tf_ic),
                        prod(set2_tf_ic),
                        NA,
                        prod(set1_tw_ic, set2_tw_ic),
                        prod(set1_tw_ic),
                        prod(set2_tw_ic),
                        NA,
                        prod(set1_coop_ic)),
       `$PMP_{i,c}$` = c(prod(set1_tf_ic, set1_tw_ic, set1_coop_ic, set2_tf_ic, set2_tw_ic) /
                           (1 + prod(set1_tf_ic, set1_tw_ic, set1_coop_ic, set2_tf_ic, set2_tw_ic)),
                         NA,
                         prod(set1_tf_ic, set2_tf_ic) / (1 + prod(set1_tf_ic, set2_tf_ic)),
                         prod(set1_tf_ic) / (1 + prod(set1_tf_ic)),
                         prod(set2_tf_ic) / (1 + prod(set2_tf_ic)),
                         NA,
                         prod(set1_tw_ic, set2_tw_ic) / (1 + prod(set1_tw_ic, set2_tw_ic)),
                         prod(set1_tw_ic) / (1 + prod(set1_tw_ic)),
                         prod(set2_tw_ic) / (1 + prod(set2_tw_ic)),
                         NA,
                         prod(set1_coop_ic) / (1 + prod(set1_coop_ic))),
       `Amount of support` = c("Very strong",
                               " ",
                               "Strong",
                               "Substantial",
                               "Positive",
                               " ",
                               "Very strong",
                               "Very strong",
                               "Very strong",
                               " ",
                               "Undecisive")) %>% 
  mutate(across(where(is.numeric), .fns = f))




# ----------- APPENDIX B - ROBUSTNESS CHECK AND ESTIMATES - TABLE 4 ------------


library(lme4)
library(sandwich)


tibble(`Study` =
         c("(ref:bolton)", " ",
           "(ref:duffy):", "No-MinEmb", " ", " ",
           "(ref:duffy):", "No-FullEmb", " ", " ",
           "(ref:veer)", " ",
           "(ref:miltenburg)", " ", " ", " ",
           "(ref:frey)", " ", " ", " ",
           "(ref:barrera)", " ", " ", " ",
           "(ref:seinen)",
           "(ref:corten)", " "),
       `Outcome` =
         c("Trustfulness", "Trustworthiness",
           "Trustfulness", "\\hspace{8pt}Robustness check",
           "Trustworthiness", "\\hspace{8pt}Robustness check",
           "Trustfulness", "\\hspace{8pt}Robustness check",
           "Trustworthiness", "\\hspace{8pt}Robustness check",
           "Trustfulness", "Trustworthiness",
           "Trustfulness", "\\hspace{8pt}Robustness check",
           "Trustworthiness", "\\hspace{8pt}Robustness check",
           "Trustfulness", "\\hspace{8pt}Robustness check",
           "Trustworthiness", "\\hspace{8pt}Robustness check",
           "Trustfulness", "\\hspace{8pt}Robustness check",
           "Trustworthiness", "\\hspace{8pt}Robustness check",
           "Cooperation (helping)",
           "Cooperation", "\\hspace{8pt}Robustness check"),
       `Coef` = c(coef(tf_bolton_glm)[2],
                  coef(tw_bolton_glm)[2],
                  coef(tf_duffy_min_glm)[2],
                  fixef(tf_duffy_min_glmer)[2],
                  coef(tf_duffy_min_glm)[2],
                  fixef(tw_duffy_min_glmer)[2],
                  coef(tf_duffy_full_glm)[2],
                  fixef(tf_duffy_full_glmer)[2],
                  coef(tw_duffy_full_glm)[2],
                  fixef(tw_duffy_full_glmer)[2],
                  coef(tf_veer_glm)[2],
                  coef(tw_veer_glm)[2],
                  coef(tf_miltenburg_glm)[2],
                  fixef(tf_miltenburg_glmer)[2],
                  coef(tw_miltenburg_glm)[2],
                  fixef(tw_miltenburg_glmer)[2],
                  coef(tf_frey_glm)[2],
                  fixef(tf_frey_glmer)[2],
                  coef(tw_frey_glm)[2],
                  fixef(tw_frey_glmer)[2],
                  coef(tf_barrera_lm)[2],
                  fixef(tf_barrera_lmer)[2],
                  coef(tw_barrera_lm)[2],
                  fixef(tw_barrera_lmer)[2],
                  coef(coop_seinen_glm)[2],
                  coef(coop_corten_glm)[2],
                  fixef(coop_corten_glmer)[2]),
       `SE` = c(sqrt(diag(vcov(tf_bolton_glm)))[2],
                sqrt(diag(vcov(tw_bolton_glm)))[2],
                sqrt(diag(vcovCL(tf_duffy_min_glm, ~Session + IDinSession)))[2],
                sqrt(diag(as.matrix(vcov(tf_duffy_min_glmer))))[2],
                sqrt(diag(vcovCL(tw_duffy_min_glm, ~Session + IDinSession)))[2],
                sqrt(diag(as.matrix(vcov(tw_duffy_min_glmer))))[2],
                sqrt(diag(vcovCL(tf_duffy_full_glm, ~Session + IDinSession)))[2],
                sqrt(diag(as.matrix(vcov(tf_duffy_full_glmer))))[2],
                sqrt(diag(vcovCL(tw_duffy_full_glm, ~Session + IDinSession)))[2],
                sqrt(diag(as.matrix(vcov(tw_duffy_full_glmer))))[2],
                sqrt(diag(vcov(tf_veer_glm)))[2],
                sqrt(diag(vcov(tw_veer_glm)))[2],
                sqrt(diag(vcovCL(tf_miltenburg_glm, ~person_id + session)))[2],
                sqrt(diag(as.matrix(vcov(tf_miltenburg_glmer))))[2],
                sqrt(diag(vcovCL(tw_miltenburg_glm, ~person_id + session)))[2],
                sqrt(diag(as.matrix(vcov(tw_miltenburg_glmer))))[2],
                sqrt(diag(vcovCL(tf_frey_glm, ~session + eSubj)))[2],
                sqrt(diag(as.matrix(vcov(tf_frey_glmer))))[2],
                sqrt(diag(vcovCL(tw_frey_glm, ~session + eSubj)))[2],
                sqrt(diag(as.matrix(vcov(tw_frey_glmer))))[2],
                sqrt(diag(vcov(tf_barrera_lm)))[2],
                sqrt(diag(as.matrix(vcov(tf_barrera_lmer))))[2],
                sqrt(diag(vcovCL(tw_barrera_lm, ~subjectid + session)))[2],
                sqrt(diag(as.matrix(vcov(tw_barrera_lmer))))[2],
                sqrt(diag(vcov(coop_seinen_glm)))[2],
                sqrt(diag(vcovCL(coop_corten_glm, ~id_subj1)))[2],
                sqrt(diag(as.matrix(vcov(coop_corten_glmer))))[2]),
       `$BF_{i,u}$` = c(tf_bolton_bf$fit$BF.u[1],
                        tw_bolton_bf$fit$BF.u[1],
                        tf_duffy_min_bf$fit$BF.u[1],
                        tf_duffy_min_glmer_bf$fit$BF.u[1],
                        tw_duffy_min_bf$fit$BF.u[1],
                        tw_duffy_min_glmer_bf$fit$BF.u[1],
                        tf_duffy_full_bf$fit$BF.u[1],
                        tf_duffy_full_glmer_bf$fit$BF.u[1],
                        tw_duffy_full_bf$fit$BF.u[1],
                        tw_duffy_full_glmer_bf$fit$BF.u[1],
                        tf_veer_bf$fit$BF.u[1],
                        tw_veer_bf$fit$BF.u[1],
                        tf_miltenburg_bf$fit$BF.u[1],
                        tf_miltenburg_glmer_bf$fit$BF.u[1],
                        tw_miltenburg_bf$fit$BF.u[1],
                        tw_miltenburg_glmer_bf$fit$BF.u[1],
                        tf_frey_bf$fit$BF.u[1],
                        tf_frey_glmer_bf$fit$BF.u[1],
                        tw_frey_bf$fit$BF.u[1],
                        tw_frey_glmer_bf$fit$BF.u[1],
                        tf_barrera_bf$fit$BF.u[1],
                        tf_barrera_lmer_bf$fit$BF.u[1],
                        tw_barrera_bf$fit$BF.u[1],
                        tw_barrera_lmer_bf$fit$BF.u[1],
                        coop_seinen_bf$fit$BF.u[1],
                        coop_corten_bf$fit$BF.u[1],
                        coop_corten_glmer_bf$fit$BF.u[1]),
       `$BF_{i,c}$` = c(tf_bolton_bf$fit$BF.c[1],
                        tw_bolton_bf$fit$BF.c[1],
                        tf_duffy_min_bf$fit$BF.c[1],
                        tf_duffy_min_glmer_bf$fit$BF.c[1],
                        tw_duffy_min_bf$fit$BF.c[1],
                        tw_duffy_min_glmer_bf$fit$BF.c[1],
                        tf_duffy_full_bf$fit$BF.c[1],
                        tf_duffy_full_glmer_bf$fit$BF.c[1],
                        tw_duffy_full_bf$fit$BF.c[1],
                        tw_duffy_full_glmer_bf$fit$BF.c[1],
                        tf_veer_bf$fit$BF.c[1],
                        tw_veer_bf$fit$BF.c[1],
                        tf_miltenburg_bf$fit$BF.c[1],
                        tf_miltenburg_glmer_bf$fit$BF.c[1],
                        tw_miltenburg_bf$fit$BF.c[1],
                        tw_miltenburg_glmer_bf$fit$BF.c[1],
                        tf_frey_bf$fit$BF.c[1],
                        tf_frey_glmer_bf$fit$BF.c[1],
                        tw_frey_bf$fit$BF.c[1],
                        tw_frey_glmer_bf$fit$BF.c[1],
                        tf_barrera_bf$fit$BF.c[1],
                        tf_barrera_lmer_bf$fit$BF.c[1],
                        tw_barrera_bf$fit$BF.c[1],
                        tw_barrera_lmer_bf$fit$BF.c[1],
                        coop_seinen_bf$fit$BF.c[1],
                        coop_corten_bf$fit$BF.c[1],
                        coop_corten_glmer_bf$fit$BF.c[1])
) %>%
  mutate(across(where(is.numeric), .fns = f))

# ----------- APPENDIX B - ROBUSTNESS CHECK AND ESTIMATES - TABLE 5 ------------

tibble(` ` = c("All studies and outcomes combined",
               " ",
               "Trustfulness ($H_1$)",
               "\\hspace{8pt}Random partner matching",
               "\\hspace{8pt}Triads",
               " ",
               "Trustworthiness ($H_2$)",
               "\\hspace{8pt}Random partner matching",
               "\\hspace{8pt}Triads",
               " ",
               "Cooperation ($H_3$)"),
       `$BF_{i,u}$` = c(prod(set1_tf_iu2, set1_tw_iu2, set1_coop_iu2,
                             set2_tf_iu2, set2_tw_iu2),
                        NA,
                        prod(set1_tf_iu2, set2_tf_iu2),
                        prod(set1_tf_iu2),
                        prod(set2_tf_iu2),
                        NA,
                        prod(set1_tw_iu2, set2_tw_iu2),
                        prod(set1_tw_iu2),
                        prod(set2_tw_iu2),
                        NA,
                        prod(set1_coop_iu2)),
       `$PMP_{i,u}$` = c(prod(set1_tf_iu2, set1_tw_iu2, set1_coop_iu2,
                              set2_tf_iu2, set2_tw_iu2) /
                           (1 + prod(set1_tf_iu2, set1_tw_iu2, set1_coop_iu2,
                                     set2_tf_iu2, set2_tw_iu2)),
                         NA,
                         prod(set1_tf_iu2, set2_tf_iu2) / (1 + prod(set1_tf_iu2, set2_tf_iu2)),
                         prod(set1_tf_iu2) / (1 + prod(set1_tf_iu2)),
                         prod(set2_tf_iu2) / (1 + prod(set2_tf_iu2)),
                         NA,
                         prod(set1_tw_iu2, set2_tw_iu2) / (1 + prod(set1_tw_iu2, set2_tw_iu2)),
                         prod(set1_tw_iu2) / (1 + prod(set1_tw_iu2)),
                         prod(set2_tw_iu2) / (1 + prod(set2_tw_iu2)),
                         NA,
                         prod(set1_coop_iu2) / (1 + prod(set1_coop_iu2))),
       `$BF_{i,c}$` = c(prod(set1_tf_ic2, set1_tw_ic2, set1_coop_ic2,
                             set2_tf_ic2, set2_tw_ic2),
                        NA,
                        prod(set1_tf_ic2, set2_tf_ic2),
                        prod(set1_tf_ic2),
                        prod(set2_tf_ic2),
                        NA,
                        prod(set1_tw_ic2, set2_tw_ic2),
                        prod(set1_tw_ic2),
                        prod(set2_tw_ic2),
                        NA,
                        prod(set1_coop_ic2)),
       `$PMP_{i,c}$` = c(prod(set1_tf_ic2, set1_tw_ic2, set1_coop_ic2, set2_tf_ic2, set2_tw_ic2) /
                           (1 + prod(set1_tf_ic2, set1_tw_ic2, set1_coop_ic2, set2_tf_ic2, set2_tw_ic2)),
                         NA,
                         prod(set1_tf_ic2, set2_tf_ic2) / (1 + prod(set1_tf_ic2, set2_tf_ic2)),
                         prod(set1_tf_ic2) / (1 + prod(set1_tf_ic2)),
                         prod(set2_tf_ic2) / (1 + prod(set2_tf_ic2)),
                         NA,
                         prod(set1_tw_ic2, set2_tw_ic2) / (1 + prod(set1_tw_ic2, set2_tw_ic2)),
                         prod(set1_tw_ic2) / (1 + prod(set1_tw_ic2)),
                         prod(set2_tw_ic2) / (1 + prod(set2_tw_ic2)),
                         NA,
                         prod(set1_coop_ic2) / (1 + prod(set1_coop_ic2)))) %>%
  mutate(across(where(is.numeric), .fns = f))





