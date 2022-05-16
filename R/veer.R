
# ------------------------------- LOAD PACKAGES --------------------------------

library(dplyr)
library(ggplot2)
library(magrittr)
library(lme4)
library(bain)
library(sandwich)

# --------------------------------- LOAD DATA ----------------------------------


veer <- readstata13::read.dta13("data/buskens_raub_veer_2010/trusttriadsSN.dta",
                                generate.factors = TRUE)

# ------------ DATA MANIPULATION FOR REPRODUCING ORIGINAL ANALYSES -------------


veer %<>%
  filter(treatment > 1) %>%
  group_by(session, treatment, subgroup) %>%
  mutate(triad = cur_group_id()) %>%
  ungroup() %>%
  mutate(network  = ifelse(network == 6, 1, 0),
         choiceb  = case_when(trustor == 1 ~ choiceb1,
                              trustor == 2 ~ choiceb2),
         choiceb  = ifelse(choiceb == 2, 0, choiceb),
         decision = period * 2 - (2 - trustor),
         trustor2 = ifelse(trustor == 2, 1, 0),
         round    = ifelse(period > 13, paste0("Round", period), 0),
         subjectidtr1 = case_when(treatment == 2                 ~ subjectid - 2,
                                  treatment == 3 & subject > 2   ~ subjectid - 2,
                                  treatment == 3 & subject <= 2  ~ subjectid + 18 - 2,
                                  treatment == 4 & subject > 5   ~ subjectid - 5,
                                  treatment == 4 & subject <= 5  ~ subjectid - 5 + 18),
         subjectidtr2 = case_when(treatment == 2                 ~ subjectid - 1,
                                  treatment == 3 & subject < 17  ~ subjectid + 2,
                                  treatment == 3 & subject >= 17 ~ subjectid + 2 - 18,
                                  treatment == 4 & subject > 10  ~ subjectid - 10,
                                  treatment == 4 & subject <= 10 ~ subjectid - 10 + 18),
         subjectidtr = case_when(trustor == 1 ~ subjectidtr1,
                                 trustor == 2 ~ subjectidtr2),
         subjectidtro = case_when(trustor == 2 ~ subjectidtr1,
                                  trustor == 1 ~ subjectidtr2),
         choicea = case_when(trustor == 1 ~ choicea1,
                             trustor == 2 ~ choicea2),
         choiceao = case_when(trustor == 1 ~ choicea2,
                              trustor == 2 ~ choicea1)) %>%
  arrange(session, treatment, subgroup, period, trustor) %>%
  mutate(choicebo = case_when(trustor == 1 ~ lead(choiceb),
                              trustor == 2 ~ lag(choiceb)),
         future = 15 - period,
         lastround = ifelse(future == 0, 1, 0),
         firstround1 = ifelse(future == 14 & treatment == 2, 1, 0),
         firstround2 = ifelse(future == 14 & treatment == 3, 1, 0),
         firstround3 = ifelse(future == 14 & treatment == 4, 1, 0),
         treatment2  = ifelse(treatment == 3, 1, 0),
         treatment3  = ifelse(treatment == 4, 1, 0),
         round_net   = ifelse(period > 13 & network == 1, paste0("Round", period, "net"), "No"),
         round_tr    = ifelse(period > 13 & trustor > 1, paste0("Round", period, "Tr", trustor), "No"),
         round_tr_net = ifelse(period > 13 & trustor > 1 & network == 1, paste0("Round", period, "Tr", trustor, "net"), "No"),
         futurenet4  = future*network,
         tweede_treat = ifelse(treatment == 3, 1, 0),
         derde_treat = ifelse(treatment == 4, 1, 0),
         hadbeentrustee = ifelse(treatment == 4 | (treatment == 3 & (decision %% 2) == 2), 1, 0)) %>%
  arrange(session, treatment, subgroup, trustor, period)


veer$aH1 <- veer$aA1 <- veer$aH2 <- veer$aA2 <- 0

for (i in 2:15) {
  veer$aH1[veer$period == i] <- 0.5 * lag(veer$aH1)[veer$period == i] + lag(veer$choicea * veer$choiceb)[veer$period == i]
  veer$aA1[veer$period == i] <- 0.5 * lag(veer$aA1)[veer$period == i] + lag(veer$choicea * (1 - veer$choiceb))[veer$period == i]
  veer$aH2[veer$period == i & veer$trustor == 1] <- 0.5 * lag(veer$aH2)[(veer$period == i & veer$trustor == 1)] + lag(veer$choiceao * veer$choicebo)[veer$period == i & veer$trustor == 1]
  veer$aA2[veer$period == i & veer$trustor == 1] <- 0.5 * lag(veer$aA2)[veer$period == i & veer$trustor == 1] + lag(veer$choiceao * (1 - veer$choicebo))[veer$period == i & veer$trustor == 1]
  veer$aH2[veer$period == i & veer$trustor == 2] <- 0.5 * lag(veer$aH2)[(veer$period == i & veer$trustor == 2)] + (veer$choiceao * veer$choicebo)[veer$period == i & veer$trustor == 2]
  veer$aA2[veer$period == i & veer$trustor == 2] <- 0.5 * lag(veer$aA2)[veer$period == i & veer$trustor == 2] + (veer$choiceao * (1 - veer$choicebo))[veer$period == i & veer$trustor == 2]
}


veer$aH2 <- veer$aH2 * veer$network
veer$aA2 <- veer$aA2 * veer$network


# ------------------------ REPLICATE ORIGINAL RESULTS --------------------------


## Replication in R yields different results due to a different algorithm, but 
## the results are not too far off

lme4::glmer(choicea ~ network + treatment2 + treatment3 + trustor2 + aH1 + 
                      aA1 + aH2 + aA2 + future + futurenet4 + round + round_net + 
                      round_tr + round_tr_net + firstround1 + firstround2 +
                      firstround3 + (1 | triad/subjectidtr), 
            data = veer[veer$trustor > 0 & veer$treatment > 1,],
            family = "binomial") %>%
  summary()

## To replicate the exact results, run the following code.
options(na.action = "na.pass")
model.matrix(choicea ~ network + treatment2 + treatment3 + trustor2 + aH1 + 
               aA1 + aH2 + aA2 + future + futurenet4 + round + round_net + 
               round_tr + round_tr_net + firstround1 + firstround2 +
               firstround3 + triad + subjectidtr,
             veer) %>%
  as.data.frame() %>%
  mutate(choicea = veer$choicea,
         trustor = veer$trustor,
         period = veer$period,
         treatment = veer$treatment) %>%
  as.data.frame %>%
  readstata13::save.dta13("~/Downloads/veer_from_r.dta")

## Then, in stata

# use veer_from_r.dta
# xtmelogit choicea network treatment2 treatment3 trustor2 aH1 aA1 aH2 aA2 future ///
#                   futurenet4 roundRound14 roundRound15 round_netRound14net      ///
#                   round_netRound15net round_trRound14Tr2 round_trRound15Tr2     ///
#                   round_tr_netRound14Tr2net round_tr_netRound15Tr2net           ///
#                   firstround1 firstround2 firstround3                           ///
#                   if (trustor > 0 & period > 0)                                 ///
#                   || triad: || subjectidtr: , var

## Hence, data handling succeeded.

## Trustfulness plot
veer_tf_plot <- 
  veer %>%
  filter(trustor > 0 & period > 0 & treatment > 1) %>%
  mutate(network = as.factor(network),
         trustor = as.factor(trustor)) %>%
  group_by(network, period, trustor) %>%
  summarize(trustful = mean(choicea == 1, na.rm = TRUE)) %>%
  ggplot(aes(x = period, y = trustful, col = network, linetype = trustor)) +
  geom_line() +
  geom_point() +
  ylim(0.2, 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Trustfulness Buskens, Raub & Veer (2010)")

veer_tw_plot <- 
  veer %>%
  filter(trustor > 0 & period > 0 & treatment > 1 & choicea == 1) %>%
  mutate(network = as.factor(network),
         trustor = as.factor(trustor)) %>%
  group_by(network, period, trustor) %>%
  summarize(trustful = mean(choiceb == 1, na.rm = TRUE)) %>%
  ggplot(aes(x = period, y = trustful, col = network, linetype = trustor)) +
  geom_line() +
  geom_point() +
  ylim(0.2, 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Trustworthiness Buskens, Raub & Veer (2010)")

library(patchwork)

veer_tf_plot + veer_tw_plot


# --------------------- ANALYSES VOLKER, BUSKENS, RAUB -------------------------

## Filter first rounds for tf
veer_tf <- veer %>%
  filter(trustor == 1,
         period == 1,
         treatment > 1)

## Proportion trustfulness
tf_veer_prop <- veer_tf %>%
  group_by(network) %>%
  summarize(tf = mean(choicea))

## Regress trustfulness on network condition
tf_veer_glm <- glm(choicea ~ network, family = binomial, veer_tf)

vcov(tf_veer_glm)
vcovCL(tf_veer_glm, cluster = ~ session, fix = TRUE)
# Not so good idea, probably due to having only two sessions per
# network condition.

## BAYES FACTOR
tf_veer_bf <- bain(x = coef(tf_veer_glm),
                   hypothesis = "network > 0",
                   Sigma = vcov(tf_veer_glm),
                   n = length(tf_veer_glm$y))

# First round trustworthiness
veer_tw <- veer %>%
  filter(trustor == 1 &
           period == 1 &
           treatment > 1 &
           choicea == 1)

# proportion trustworthiness
tw_veer_prop <- veer_tw %>%
  group_by(network) %>%
  summarize(tw = mean(choiceb))

# logistic regression trustworthiness
tw_veer_glm <- glm(choiceb ~ network, family = binomial, data = veer_tw)

vcov(tw_veer_glm)
vcovCL(tw_veer_glm, cluster = ~ session)
# Not so good idea, probably due to having only two sessions per
# network condition.

# bayes factor trustworthiness
tw_veer_bf <- bain(x = coef(tw_veer_glm),
                   hypothesis = "network > 0",
                   Sigma = vcov(tw_veer_glm),
                   n = length(tw_veer_glm$y))


# ------------------------------- SAMPLE SIZE ----------------------------------

nrow(veer_tf) + nrow(veer_tw) ## Number of actions and number of participants

# ------------------------------ SAVE RESULTS ----------------------------------

save.image("R/results/veer.RData")
