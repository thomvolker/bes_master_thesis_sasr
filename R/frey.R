
################################################################################
## Data by Frey, Corten & Buskens, 2018
################################################################################


# ------------------------------- LOAD PACKAGES --------------------------------

library(dplyr)
library(ggplot2)
library(magrittr)
library(lme4)
library(bain)
library(lmtest)
library(sandwich)

# --------------------------------- LOAD DATA ----------------------------------

frey <- readstata13::read.dta13("data/frey_corten_buskens_2018/SON_trust.dta",
                                generate.factors = TRUE)

# -------------- DATA MANIPULATION TO REPRODUCE ORIGINAL RESULTS ---------------


frey %<>%
  mutate(eSubj = session * 100 + subject) %>%
  arrange(session, triad_id, B, A, round) %>%
  group_by(session, triad_id, Aactive) %>%
  mutate(obs_abused = (A == Aactive | info == 1) * (1 - ifelse(is.na(honor), 1, honor)),
         round_obs  = ifelse(obs_abused == 1, round, 7),
         first_round_obs = min(round_obs),
         observed_abuse = ifelse(round > first_round_obs, 1, 0)) %>%
  ungroup() %>%
  group_by(session, triad_id, A) %>%
  mutate(first_round_obs = min(round_obs),
         observed_abuse = ifelse(round > first_round_obs & info == 1, 1, observed_abuse),
         playPeriod = case_when(round < 3 ~ 1,
                               round > 2 & round < 5 ~ 2,
                               round > 4 ~ 3),
         tg2 = ifelse(round %% 2 == 0, 1, 0),
         info = factor(info),
         pi = factor(round(pi, 3), levels = c(0.2, 0.05, 0.4)),
         g = case_when(gamma < 3 ~ 1,
                       gamma == 3 ~ 2,
                       gamma == 4 ~ 3),
         g = factor(g, levels = c(1,2,3), labels = c("Exo", "EndoR", "EndoE")))

# ---------------------- REPRODUCE ORIGINAL FINDINGS ---------------------------


ptable <- function(x) prop.table(table(x))

## proportion of observations without abuse of trust withnessed
frey %$% ptable(observed_abuse)

## Proportion of interactions in which trust was placed (if no abuse of trust 
## has been observed yet)
frey %>%
  filter(observed_abuse == 0,
         A == Aactive) %$%
  ptable(trust)

## Proportion of observations in which trust was honored (if no abuse of trust
## has been observed yet, and the trustee is of the opportunistic type)
frey %>%
  filter(bad == 1,
         observed_abuse == 0,
         A == Aactive) %$%
  ptable(honor)

## Reproduce appendix A from paper
## Note that the parameterization is slightly different in R than in Stata

## Regular way in R
frey %>%
  filter(A == Aactive,
         observed_abuse == 0) %$%
  lme4::glmer(trust ~ info*pi*g +
                info*playPeriod +
                info*tg2 +
                (1 | eSubj),
              family = binomial) %>%
  summary

## Stata way, but in R
stata_like_rep <-
  data.frame(model.matrix(trust ~ info*pi*g + info*playPeriod + info*tg2, frey)[,-1],
           trust = frey$trust,
           honor = frey$honor,
           B = frey$B,
           bad = frey$bad,
           eSubj = frey$eSubj,
           A = frey$A,
           Aactive = frey$Aactive,
           observed_abuse = frey$observed_abuse) %>%
  mutate(playPeriod = ifelse(info1 == 1, 0, playPeriod),
         tg2 = ifelse(info1 == 1, 0, tg2))

glmer(trust ~ info1 + pi0.05 + pi0.4 + gEndoR + gEndoE + playPeriod + tg2 + 
              info1.pi0.05 + info1.pi0.4 + info1.gEndoR + info1.gEndoE +
              pi0.05.gEndoR + pi0.4.gEndoR + pi0.05.gEndoE + pi0.4.gEndoE +
              info1.playPeriod + info1.pi0.05.gEndoR + info1.pi0.4.gEndoR +
              info1.pi0.05.gEndoE + info1.pi0.4.gEndoE + (1 | eSubj),
        data = stata_like_rep %>% filter(A == Aactive, observed_abuse == 0) ,
        family = binomial) %>%
  summary()

## No convergence has been reached, but the estimates are quite similar.
## See the next section to obtain identical results via stata.

## Regular way in R (results in similar model fit, but somewhat different
## estimates due to different parameterization)
honor_glmer <- frey %>%
  filter(B == 1,
         bad == 1,
         observed_abuse == 0) %$%
  glmer(honor ~ info*pi*g +
                info*playPeriod + 
                info*tg2 +
                (1 | eSubj),
        family = binomial)

## Stata parameterization, but ran in R
glmer(honor ~ info1 + pi0.05 + pi0.4 + gEndoR + gEndoE + playPeriod + tg2 + 
              info1.pi0.05 + info1.pi0.4 + info1.gEndoR + info1.gEndoE +
              pi0.05.gEndoR + pi0.4.gEndoR + pi0.05.gEndoE + pi0.4.gEndoE +
              info1.playPeriod + info1.pi0.05.gEndoR + info1.pi0.4.gEndoR +
              info1.pi0.05.gEndoE + info1.pi0.4.gEndoE + (1 | eSubj),
      data = stata_like_rep %>% filter(B == 1, bad == 1, observed_abuse == 0),
      family = binomial) %>%
  summary


# REPLICATION IN STATA

### RUN IN R TO OBTAIN DATA ###
rep_d <- data.frame(trust = frey$trust, ## DV
                    honor = frey$honor,
                    A = frey$A,
                    Aactive = frey$Aactive,
                    observed_abuse = frey$observed_abuse,
                    B = frey$B,
                    bad = frey$bad,
                    model.matrix(trust ~ info*pi*g +       ## numeric IV's
                                         info*playPeriod + ## without
                                         info*tg2 +        ## intercept
                                         eSubj,            #column
                                  frey)[,-1])

colnames(rep_d) <- gsub("\\.", "_", colnames(rep_d))
readstata13::save.dta13(rep_d, file = "data/frey_corten_buskens_2018/frey_rep_stata.dta")

### Run in Stata TO OBTAIN RESULTS ###

# // R parameterization

# // use "path_to_file\frey_rep_stata.dta"
# 
# xtmelogit trust info* pi* g* playPeriod tg2 || eSubj: if A == Aactive & observed_abuse == 0, var
# xtmelogit honor info* pi* g* playPeriod tg2 || eSubj: if B == 1 & bad == 1 & observed_abuse == 0, var
# 
# // Stata parameterization
# 
# 
# replace playPeriod = 0 if info1_playPeriod != 0
# replace tg2 = 0 if info1_tg2 != 0
# 
# xtmelogit trust info* pi* g* playPeriod tg2 || eSubj: if A == Aactive & observed_abuse == 0, var
# xtmelogit honor info* pi* g* playPeriod tg2 || eSubj: if B == 1 & bad == 1 & observed_abuse == 0, var
# 
# // Yields results identical to the published results




## Recreate figures

## Trustfulness
frey %>%
  filter(A == Aactive,
         observed_abuse == 0) %>%
  mutate(pi = factor(pi, levels = c(0.05, 0.2, 0.4)),
         info = factor(info, levels = c(0,1), labels = c("NoEmb", "Emb"))) %>%
  group_by(g, pi, info, round) %>%
  summarize(trust = mean(trust)) %>%
  ggplot(aes(x = round, 
             y = trust, 
             col = info,
             shape = info,
             linetype = info)) +
  geom_line() +
  geom_point() +
  facet_wrap(g ~ pi) +
  scale_color_brewer(palette = "Set1") +
  ylim(0, 1) +
  theme_minimal()

## Trustworthiness
frey %>%
  filter(B == 1,
         bad == 1,
         observed_abuse == 0) %>%
  mutate(pi = factor(pi, levels = c(0.05, 0.2, 0.4)),
         info = factor(info, levels = c(1,0), labels = c("Emb", "NoEmb"))) %>%
  group_by(g, pi, info, round) %>%
  summarize(honor = mean(honor, na.rm = TRUE)) %>%
  ggplot(aes(x = round, 
             y = honor, 
             col = info,
             shape = info,
             linetype = info)) +
  geom_line() +
  facet_wrap(g ~ pi) +
  scale_color_brewer(palette = "Set1") +
  ylim(0, 1) +
  theme_minimal()


# ---------------------- ANALYSES VOLKER, BUSKENS, RAUB ------------------------


frey_tf <- # Trustfulness data
  frey %>%
  filter(A == Aactive,
         round == 1,
         gamma < 3)

tf_frey_prop <- frey_tf %>% # proportion trustfulness
  group_by(info) %>%
  summarize(tf = mean(trust))

tf_frey_glm <- glm(trust ~ info + pi, # logistic regression model trustfulness
                   family = binomial,
                   data = frey_tf)

tf_frey_bf <- 
  bain(x = coef(tf_frey_glm), # bayes factor trustfulness
       hypothesis = "info1 > 0",
       Sigma = vcovCL(tf_frey_glm, cluster = ~session + eSubj),
       n = length(tf_frey_glm$y))

frey_tw <- # trustworthiness data
  frey %>%
  filter(B == 1,
         bad == 1,
         round == 1,
         gamma < 3,
         trust == 1)

tw_frey_prop <- frey_tw %>% # proportion trustworthiness
  group_by(info) %>%
  summarize(tw = mean(honor))
  

tw_frey_glm <- glm(honor ~ info + pi, # logistic regression model trustworthiness
                  family = binomial,
                  data = frey_tw)

tw_frey_bf <-
  bain(x = coef(tw_frey_glm), # trustworthiness bayes factors
       hypothesis = "info1 > 0",
       Sigma = vcovCL(tw_frey_glm, cluster = ~session + eSubj),
       n = length(tw_frey_glm$y))


# ----------- MULTILEVEL ROBUSTNESS ANALYSES VOLKER, BUSKENS, RAUB -------------


tf_frey_glmer <- glmer(trust ~ info + pi + (1 | eSubj),
                       family = binomial,
                       data = frey_tf,
                       nAGQ = 50)

tf_frey_glmer_bf <- bain(fixef(tf_frey_glmer),
                         hypothesis = "info1 > 0",
                         Sigma = as.matrix(vcov(tf_frey_glmer)),
                         n = length(tf_frey_glmer@resp$n))

tw_frey_glmer <- glmer(honor ~ info + pi + (1 | eSubj),
                       family = binomial,
                       data = frey_tw,
                       nAGQ = 50)

tw_frey_glmer_bf <- bain(fixef(tw_frey_glmer),
                         hypothesis = "info1 > 0",
                         Sigma = as.matrix(vcov(tw_frey_glmer)),
                         n = length(tw_frey_glmer@resp$n))

# -------------------------------- SAMPLE SIZE ---------------------------------

nrow(frey_tf) + nrow(frey_tw) ## number of actions
length(unique(c(frey_tf$eSubj, frey_tw$eSubj))) ## number of participants

# ------------------------------- SAVE RESULTS ---------------------------------

save.image("R/results/frey.RData")
