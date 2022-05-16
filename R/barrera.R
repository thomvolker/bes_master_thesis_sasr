
# ------------------------------- LOAD PACKAGES --------------------------------

library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sandwich)
library(lme4)
library(bain)

# --------------------------------- LOAD DATA ----------------------------------


barrera <- readstata13::read.dta13("data/barrera_buskens_2009/sixtetall_db250806.dta",
                                   generate.factors = TRUE)

# ------------ DATA MANIPULATION FOR REPRODUCING ORIGINAL RESULTS --------------

barrera <- barrera %>%
  arrange(session, treatment, period, subject, group1, col) %>%
  mutate(idtriad = session * 100 + group1 * 10 + col,
         idnetwork = session * 100 + group1 * 10,
         man = as.factor(sex),
         economics = ifelse(studynum == 1, 1, 0) %>% as.factor,
         period = ifelse(period == 2 & as.numeric(treatment) == 1, -1, period),
         period = ifelse(period == 1 & as.numeric(treatment) == 1, -2, period),
         uncertainty = as.numeric(uncertainty) - 1,
         info_1 = case_when(as.numeric(treatment) %in% c(2, 4) & (as.numeric(condition) %% 2 == 0) ~ 1,
                            as.numeric(treatment) %in% c(2, 4) ~ 0),
         info_2 = case_when(as.numeric(treatment) %in% c(3, 4) & (as.numeric(condition) %in% c(2, 4, 5, 7)) ~ 1,
                            as.numeric(treatment) %in% c(3, 4) ~ 0),
         firstround = ifelse(period == 1, 1, 0),
         lastround = ifelse(period == 15, 1, 0)) %>%
  arrange(session, treatment, subject, period)  %>%
  mutate(sent0 = ifelse(sapply(lag(choicea1) == 0, isTRUE), 1, 0),
         future = ifelse(as.numeric(treatment) > 1, 15 - period, NA),
         propret_a1 = ifelse(role == "trustor" & period > 0, 
                             ifelse(choicea1 == 0, 0, choiceb11 / (3 * choicea1)), 
                             NA),
         propret_a2 = ifelse(role == "trustor" & period > 0, 
                             ifelse(choicea2 == 0, 0, choiceb12 / (3 * choicea2)), 
                             NA),
         propret_a3 = ifelse(role == "trustor" & period > 0, 
                             ifelse(choicea3 == 0, 0, choiceb21 / (3 * choicea3)), 
                             NA),
         across(.cols = c(choicea1, choicea2, choicea3), 
                .fns =~ ifelse(role == "trustor" & period > 0, .x / 10, NA),
                .names = "propsent_a{1:3}"),
         endscale = case_when(choicea1 == 0 & role == "trustor" ~ -1,
                              choicea1 == 10 & role == "trustor" ~ 1,
                              TRUE ~ 0),
         cha1low = ifelse(choicea1 == 0, NA, choicea1 - 0.5),
         cha1high = ifelse(choicea1 == 10, NA, choicea1 + 0.5)) %>%
  arrange(subjectid, treatment, period) %>%
  mutate(sentdif = ifelse(period > 1, choicea1 - lag(choicea1), NA),
         propsendif2 = case_when(period == 1 & role == "trustor" ~ 0,
                                 period > 1 & role == "trustor" ~ lag(propsent_a2) - lag(propsent_a1)),
         propsendif3 = case_when(period == 1 & role == 1 ~ 0,
                                 period > 1 & role == "trustor" ~ lag(propsent_a3) - lag(propsent_a1)),
         futinfo = sapply(info_1 == 1, isTRUE) * future,
         futunc  = ifelse(uncertainty == 1, future, 0),
         earned_a1 = case_when(role == "trustor" & period > 1 ~ lag(choiceb11 - choicea1),
                               role == "trustor" ~ integer(1)),
         earned_a2 = case_when(role == "trustor" & period > 1 ~ (lag(choiceb12 - choicea2)) * sapply(info_1 == 1, isTRUE),
                               role == "trustor" ~ integer(1)),
         earned_a3 = case_when(role == "trustor" & period > 1 ~ (lag(choiceb21) - lag(choicea3)) * sapply(info_2 == 1, isTRUE),
                               role == "trustor" ~ integer(1)),
         grate2 = case_when(period > 1 & sapply(info_1 == 1, isTRUE) & role == "trustor" ~ pmax(0, lag(propret_a1 - propret_a2)),
                            role == "trustor" ~ 0),
         grate3 = case_when(period > 1 & sapply(info_2 == 1, isTRUE) & role == "trustor" ~ pmax(0, lag(propret_a1 - propret_a3)),
                            role == "trustor" ~ 0),
         envy2 = case_when(period > 1 & sapply(info_1 == 1, isTRUE) & role == "trustor" ~ pmax(0, lag(propret_a2 - propret_a1)),
                           role == "trustor" ~ 0),
         envy3 = case_when(period > 1 & sapply(info_2 == 1, isTRUE) & role == "trustor" ~ pmax(0, lag(propret_a3 - propret_a1)),
                           role == "trustor" ~ 0),
         across(.cols = propret_a1:propret_a3,
                .fns = ~ case_when(period == 1 & role == "trustor" ~ 0,
                                   period == 2 & role == "trustor" ~ lag(.x)),
                .names = "past_{1:3}"),
         across(.cols = propsent_a1:propsent_a3,
                .fns = ~ case_when(period == 1 & role == "trustor" ~ 0,
                                   period > 1 & role == "trustor" ~ lag(.x)),
                .names = "sent_{1:3}"),
         envyd2 = ifelse(role == "trustor" & period == 1, 0, NA),
         grated2 = ifelse(role == "trustor" & period == 1, 0, NA),
         envyd3 = ifelse(role == "trustor" & period == 1, 0, NA),
         grated3 = ifelse(role == "trustor" & period == 1, 0, NA),
         earned_d1 = ifelse(role == "trustor" & period == 1, 0, NA),
         earned_d2 = ifelse(role == "trustor" & period == 1, 0, NA),
         earned_d3 = ifelse(role == "trustor" & period == 1, 0, NA),
         grated2 = ifelse(period > 1 & role == "trustor", grate2, grated2),
         grated3 = ifelse(period > 1 & role == "trustor", grate3, grated3),
         earned_d1 = ifelse(period > 1 & role == "trustor", earned_a1, earned_d1),
         earned_d2 = ifelse(period > 1 & role == "trustor", earned_a2, earned_d2),
         earned_d3 = ifelse(period > 1 & role == "trustor", earned_a3, earned_d3),
         sent_d1 = ifelse(role == "trustor" & period == 1, 0, NA),
         difsent_d2 = ifelse(role == "trustor" & period == 1, 0, NA),
         difsent_d3 = ifelse(role == "trustor" & period == 1, 0, NA))

for (i in 2:15) {
  barrera$sent_d1[barrera$period == i & barrera$role == "trustor"] <- 
    barrera$sent_d1[barrera$period == (i - 1) & barrera$role == "trustor"] * 0.5 +
    barrera$sent_1[barrera$period == i & barrera$role == "trustor"]
  
  barrera$envyd2[barrera$period == i & barrera$role == "trustor"] <-
    barrera$envyd2[barrera$period == (i - 1) & barrera$role == "trustor"] * 0 +
    barrera$envy2[barrera$period == i & barrera$role == "trustor"]
  
  barrera$envyd3[barrera$period == i & barrera$role == "trustor"] <-
    barrera$envyd3[barrera$period == (i - 1) & barrera$role == "trustor"] * 0 +
    barrera$envy3[barrera$period == i & barrera$role == "trustor"]
}

for (i in 3:15) {
  barrera$past_1[barrera$period == i & barrera$role == "trustor"] <- 
    barrera$past_1[barrera$period == (i - 2) & barrera$role == "trustor"] * 0.9 +
    barrera$propret_a1[barrera$period == (i - 1) & barrera$role == "trustor"]
  
  barrera$past_2[barrera$period == i & barrera$role == "trustor"] <- 
    barrera$past_2[barrera$period == (i - 2) & barrera$role == "trustor"] * 0 +
    barrera$propret_a2[barrera$period == (i - 1) & barrera$role == "trustor"]
  
  barrera$past_3[barrera$period == i & barrera$role == "trustor"] <- 
    barrera$past_3[barrera$period == (i - 2) & barrera$role == "trustor"] * 0 +
    barrera$propret_a3[barrera$period == (i - 1) & barrera$role == "trustor"]
}

barrera <- barrera %>% 
  mutate(past_1unc = past_1 * (uncertainty == 1),
         past_2full = past_2 * sapply(info_1 == 1, isTRUE),
         past_3full = past_3 * sapply(info_2 == 1, isTRUE),
         past_2part = past_2 * sapply(info_1 == 0, isTRUE),
         past_3part = past_3 * sapply(info_1 == 0, isTRUE),
         past_2func = past_2 * sapply(info_1 == 1, isTRUE) * sapply(uncertainty == 1, isTRUE),
         past_3func = past_3 * sapply(info_2 == 1, isTRUE) * sapply(uncertainty == 1, isTRUE),
         across(.cols = c(envyd2, envyd3, grated2, grated3),
                .fns = ~ .x * (uncertainty == 1),
                .names = "{.col}unc"),
         difsent_d2 = ifelse(period > 1 & role == "trustor", propsendif2, difsent_d2),
         difsent_d3 = ifelse(period > 1 & role == "trustor", propsendif3, difsent_d3),
         difsent_2full = difsent_d2 * sapply(info_1 == 1, isTRUE),
         difsent_3full = difsent_d3 * sapply(info_2 == 1, isTRUE),
         difsent_2part = difsent_d2 * sapply(info_1 == 0, isTRUE),
         difsent_3part = difsent_d3 * sapply(info_2 == 0, isTRUE),
         difsent_2func = difsent_d2 * sapply(info_1 == 1, isTRUE) * uncertainty,
         difsent_3func = difsent_d3 * sapply(info_2 == 1, isTRUE) * uncertainty,
         difsent_2punc = difsent_d2 * sapply(info_1 == 0, isTRUE) * uncertainty,
         difsent_3punc = difsent_d3 * sapply(info_2 == 0, isTRUE) * uncertainty)


# ------------------------ REPRODUCE ORIGINAL RESULTS --------------------------

# Recreate figure in paper

barrera %>%
  filter(treatment %in% c("within", "between")) %>%
  group_by(uncertainty, network, treatment, period) %>%
  summarize(sent = mean((choicea1 + choicea2 + choicea3 + choicea4)/4, na.rm=T)) %>%
  ggplot(aes(x = period,
             y = sent,
             linetype = interaction(uncertainty, network),
             col = interaction(uncertainty, network))) +
  geom_line() +
  facet_wrap(uncertainty ~ treatment) +
  theme_minimal()

bind_rows(
  Link12 = barrera %>%
    filter(treatment %in% c("mixed")) %>%
    group_by(uncertainty, link = link12, period) %>%
    summarize(sent = mean((choicea1 + choicea2 + choicea3 + choicea4)/4, na.rm=T)),
  Link13 = barrera %>%
    filter(treatment %in% c("mixed")) %>%
    group_by(uncertainty, link = link13, period) %>%
    summarize(sent = mean((choicea1 + choicea2 + choicea3 + choicea4)/4, na.rm=T)),
  .id = "Link"
) %>%
  ggplot(aes(x = period, 
             y = sent, 
             linetype = interaction(link, Link), 
             col = interaction(link, Link))) +
  geom_line() +
  facet_wrap(uncertainty ~ Link) +
  theme_minimal()

## REPLICATION ANALYSES

# Note that the analyses cannot be replicated in R (there is no package for
# mixed interval regression models), at least not one that gives similar
# results. Therefore, I have saved a data file that can be loaded into stata,
# that will yields results that are identical to the results obtained by
# barrera and buskens (2009).

readstata13::save.dta13(barrera, file = "data/barrera_buskens_2009/stata_rep_barrera.dta")

## Then, run in stata (after uncommenting the code)

# // Set path to file
# use "stata_rep_barrera.dta", clear
#   xtintreg cha1low cha1high info_1        uncertainty ///
#     if role==1 & treatment==2 & personid<1000, i(subjectid) intreg
#   xtintreg cha1low cha1high info_1        uncertainty firstround sent_d1 earned_d1 past_1 future lastround past_2full difsent_2full difsent_2part envyd2 futinfo ///
#     if role==1 & treatment==2 & personid<1000,  i(subjectid) intreg
#   quadchk, nooutput
#   xtintreg cha1low cha1high info_1        uncertainty firstround sent_d1 earned_d1 past_1 future lastround past_2full difsent_2full difsent_2part envyd2 futinfo past_1unc past_2func ///
#     if role==1 & treatment==2 & personid<1000,  i(subjectid) intreg
# 
#   xtintreg cha1low cha1high info_2        uncertainty ///
#     if role==1 & treatment==3 & personid<1000,  i(subjectid) intreg
#   xtintreg cha1low cha1high info_2        uncertainty firstround sent_d1 earned_d1 past_1 future lastround past_3full difsent_3full difsent_3part envyd3 ///
#     if role==1 & treatment==3 & personid<1000,  i(subjectid) intreg
#   quadchk, nooutput
#   xtintreg cha1low cha1high info_2        uncertainty firstround sent_d1 earned_d1 past_1 future lastround past_3full difsent_3full difsent_3part envyd3 past_1unc past_3func envyd3unc ///
#     if role==1 & treatment==3 & personid<1000,  i(subjectid) intreg
# 
#   xtintreg cha1low cha1high info_1 info_2 uncertainty ///
#     if role==1 & treatment==4 & personid<1000,  i(subjectid) intreg
#   xtintreg cha1low cha1high info_1 info_2 uncertainty firstround sent_d1 earned_d1 past_1 future lastround past_2full past_3full difsent_2full difsent_2part difsent_3full difsent_3part envyd2 envyd3 futinfo ///
#     if role==1 & treatment==4 & personid<1000, i(subjectid) intreg
#   quadchk, nooutput
#   xtintreg cha1low cha1high info_1 info_2 uncertainty firstround sent_d1 earned_d1 past_1 future lastround past_2full past_3full difsent_2full difsent_2part difsent_3full difsent_3part envyd2 envyd3 futinfo past_1unc past_2func past_3func envyd3unc ///
#     if role==1 & treatment==4 & personid<1000,  i(subjectid) intreg

# ---------------------- ANALYSES VOLKER, BUSKENS, RAUB ------------------------


barrera_tf <- barrera %>% # trustfulness data
  filter(period == 1, 
         personid < 1000,
         treatment %in% c("within", "between"),
         network %in% c("F-N", "N-F"),
         role == "trustor",
         uncertainty == 0) %>%
  mutate(network2 = recode_factor(network, "N-F" = "NoEmb", "F-N" = "Emb"),
         sent = choicea1 / 10,
         groupid = session*100 + group1)

tf_barrera_prop <- barrera_tf %>% # proportion trustfulness per network condition
  group_by(network2) %>%
  summarize(tf = mean(sent))

tf_barrera_lm <- lm(sent ~ network2, data = barrera_tf) # fit lm model

vcov(tf_barrera_lm)
vcovCL(tf_barrera_lm, ~ session + subjectid) # decreases se
vcovCL(tf_barrera_lm, ~ session) # decreases se
vcovCL(tf_barrera_lm, ~ subjectid) # decreases se



tf_barrera_bf <- bain(coef(tf_barrera_lm), # calculate trustfulness bayes factor
                      hypothesis = "network2Emb > 0",
                      Sigma = vcov(tf_barrera_lm),
                      n = nrow(barrera_tf))

barrera_tw <- barrera %>% # trustworthiness data
  filter(period == 1,
         personid < 1000,
         treatment %in% c("within", "between"),
         network %in% c("F-N", "N-F"),
         role == "trustee",
         uncertainty == 0)

barrera_tw <- # bind trustworthiness data together (trustee makes two choices)
  bind_rows(barrera_tw %>% mutate(choicea = choicea1, # make separate row per choice
                                  choiceb = choiceb11,
                                  link = link12),
            barrera_tw %>% mutate(choicea = choicea2,
                                  choiceb = choiceb12)) %>% 
  filter(choicea > 0) %>%
  mutate(returned = choiceb / (3*choicea),
         network2 = recode_factor(network, "N-F" = "NoEmb", "F-N" = "Emb"),
         groupid = session * 100 + group1)

tw_barrera_prop <- barrera_tw %>% # proportion trustworthiness per network condition
  group_by(network2) %>%
  summarize(tw = mean(returned))

tw_barrera_lm <- lm(returned ~ network2, barrera_tw) # fit trustworthiness lm

vcov(tw_barrera_lm)
vcovCL(tw_barrera_lm, ~personid + session + treatment) # decreases se
vcovCL(tw_barrera_lm, ~subjectid + session) # increases se, so choice to go


tw_barrera_bf <- bain(coef(tw_barrera_lm), # calculate trustworthiness bf
                      hypothesis = "network2Emb > 0",
                      Sigma = vcovCL(tw_barrera_lm, cluster = ~ subjectid + session),
                      n = nrow(barrera_tw))


# ----------------- ROBUSTNESS ANALYSIS WITH MULTILEVEL MODELS -----------------

tf_barrera_lmer <- lmer(sent ~ network2 + (1 | personid),
                        data = barrera_tf)

summary(tf_barrera_lmer)

tf_barrera_lmer_bf <- bain(fixef(tf_barrera_lmer),
                           hypothesis = "network2Emb > 0",
                           Sigma = as.matrix(vcov(tf_barrera_lmer)),
                           n = length(tf_barrera_lmer@resp$y))


tw_barrera_lmer <- lmer(returned ~ network2 + (1 | session/personid),
                        data = barrera_tw)


summary(tw_barrera_lmer)

tw_barrera_lmer_bf <- bain(fixef(tw_barrera_lmer),
                           hypothesis = "network2Emb > 0",
                           Sigma = as.matrix(vcov(tw_barrera_lmer)),
                           n = length(tw_barrera_lmer@resp$y))


# ------------------------------- SAMPLE SIZE ----------------------------------

nrow(barrera_tf) + nrow(barrera_tw) # number of actions
length(unique(c(barrera_tf$subjectid, barrera_tw$subjectid))) # number of participants

# ------------------------------- SAVE RESULTS ---------------------------------

save.image("R/results/barrera.RData")




