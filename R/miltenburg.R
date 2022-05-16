
# ------------------------------- LOAD PACKAGES --------------------------------

library(magrittr)
library(dplyr)
library(purrr)
library(ggplot2)
library(bain)
library(lme4)
library(sandwich)
library(lmtest)

# --------------------------------- LOAD DATA ----------------------------------

miltenburg <- 
  readstata13::read.dta13("data/miltenburg_buskens_raub_2012/expall.dta",
                          generate.factors = TRUE)

# ------------ DATA MANIPULATION FOR REPRODUCING ORIGINAL RESULTS --------------

miltenburg %<>%
  arrange(session, treatment, Period, Subject) %>%
  mutate(id = row_number(),
         session = case_when(0 < id & id < 1819 ~ 1,
                             1818 < id & id < 3424 ~ 2,
                             3423 < id & id < 5029 ~ 3,
                             5028 < id & id < 6955 ~ 4,
                             6954 < id & id < 8881 ~ 5,
                             8880 < id & id < 10807 ~ 6,
                             10806 < id & id < 12733 ~ 7,
                             12732 < id & id < 14658 ~ 8),
         person_id = case_when(session == 1 ~ Subject + 0,
                               session == 2 ~ Subject + 18,
                               session == 3 ~ Subject + 33,
                               session == 4 ~ Subject + 48,
                               session == 5 ~ Subject + 66,
                               session == 6 ~ Subject + 84,
                               session == 7 ~ Subject + 102,
                               session == 8 ~ Subject + 120),
         group_id = 100 * session + 10 * treatment + Group,
         condition = recode_factor(session, 
                                   `1` = 1, `2` = 3, `3` = 3, `4` = 1,
                                   `5` = 2, `6` = 2, `7` = 4, `8` = 4),
         condition = factor(condition, labels = c("MZ", "ZM", "ZZ", "MM"))) %>%
  filter(treatment > 1 & treatment < 8) %>% 
  mutate(owntrust = case_when(Tr == 1 ~ ChoiceA1,
                              Tr == 2 ~ ChoiceA2),
         othertrust = case_when(Tr == 1 ~ ChoiceA2,
                                Tr == 2 ~ ChoiceA1),
         ownhonor = case_when(Tr == 1 ~ ChoiceB1,
                              Tr == 2 ~ ChoiceB2),
         otherhonor = case_when(Tr == 1 ~ ChoiceB2,
                                Tr == 2 ~ ChoiceB1)) %>%
  bind_rows(filter(.data = ., Tr == 0)) %>%
  group_by(id) %>%
  mutate(countB = row_number(id),
         owntrust = ifelse(countB == 1 & Tr == 0, ChoiceA1, owntrust),
         owntrust = ifelse(countB == 2 & Tr == 0, ChoiceA2, owntrust),
         ownhonor = ifelse(countB == 1 & Tr == 0, ChoiceB1, ownhonor),
         ownhonor = ifelse(countB == 2 & Tr == 0, ChoiceB2, ownhonor),
         ownhonor = ifelse(ownhonor == 2, NA, ownhonor),
         otherhonor = ifelse(otherhonor == 2, NA, otherhonor),
         network = ifelse(Network == 1, 0, 1),
         future = 15 - Period,
         cfuture = future - 7,
         future_full = cfuture * network,
         round14 = ifelse(Period == 14, 1, 0),
         round15 = ifelse(Period == 15, 1, 0),
         round14full = round14*network,
         round15full = round15*network,
         Tr2 = ifelse(Tr == 2, 1, 0),
         round14tr2 = round14*Tr2,
         round15tr2 = round15*Tr2,
         round14tr2full = round14full * Tr2,
         round15tr2full = round15full * Tr2,
         game1round1 = ifelse(Period == 1 & treatment == 2, 1, 0),
         game2round1 = ifelse(Period == 1 & treatment == 3, 1, 0),
         game3round1 = ifelse(Period == 1 & treatment == 4, 1, 0),
         game4round1 = ifelse(Period == 1 & treatment == 5, 1, 0),
         game5round1 = ifelse(Period == 1 & treatment == 6, 1, 0),
         game6round1 = ifelse(Period == 1 & treatment == 7, 1, 0),
         treatment = treatment - 1,
         Tr2game = case_when(Tr == 0 & countB == 1 ~ 0,
                             Tr == 0 & countB == 2 ~ 1),
         treat2 = treatment,
         treat2network = (treat2 - 3.5) * network,
         tr2network = ifelse(Tr == 0, Tr2game*network, Tr2*network),
         tr2treat2network = ifelse(Tr == 0, Tr2game * treat2network, Tr2 * treat2network),
         tr2treat2 = ifelse(Tr == 0, Tr2game * (treat2 - 3.5), Tr2 * (treat2 - 3.5)))


# --------------------- REPLICATION ORIGINAL RESULTS ---------------------------



miltenburg_tf_plot <- miltenburg %>%
  filter(Tr > 0) %>%
  group_by(network, Period, Tr) %>%
  mutate(network = as.factor(network),
         Tr = as.factor(Tr)) %>%
  summarize(trustfulness = mean(owntrust)) %>%
  ggplot(aes(x = Period, y = trustfulness, col = network, linetype = Tr)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Trustfulness Van Miltenburg, Buskens, Raub (2012)")

miltenburg_tf_plot_treatments <- miltenburg %>% # not in paper, just to check trustfulness per supergame
  filter(Tr > 0) %>%
  group_by(treatment, network, Period, Tr) %>%
  mutate(network = as.factor(network),
         Tr = as.factor(Tr)) %>%
  summarize(trustfulness = mean(owntrust)) %>%
  ggplot(aes(x = Period, y = trustfulness, col = network, linetype = Tr)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  facet_wrap(~ treatment, nrow = 2) +
  labs(title = "Trustfulness Van Miltenburg, Buskens, Raub (2012) per treatment")



miltenburg_tw_plot <- miltenburg %>% # not in paper, just to check trustfulness per supergame
  filter(Tr > 0 & owntrust == 1) %>%
  group_by(network, Period, Tr) %>%
  mutate(network = as.factor(network),
         Tr = as.factor(Tr)) %>%
  summarize(trustworthiness = mean(ownhonor)) %>%
  ggplot(aes(x = Period, y = trustworthiness, col = network, linetype = Tr)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Trustworthiness Van Miltenburg, Buskens, Raub (2012)")

miltenburg_tw_plot_treatments <- miltenburg %>%
  filter(Tr > 0 & owntrust == 1) %>%
  group_by(treatment, network, Period, Tr) %>%
  mutate(network = as.factor(network),
         Tr = as.factor(Tr)) %>%
  summarize(trustworthiness = mean(ownhonor)) %>%
  ggplot(aes(x = Period, y = trustworthiness, col = network, linetype = Tr)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  facet_wrap(~ treatment, nrow = 2) +
  labs(title = "Trustworthiness Van Miltenburg, Buskens, Raub (2012) per treatment")


fitlist_tf <- map(1:15, ~glm(owntrust ~ treat2 + network + treat2network,
                             data = miltenburg[miltenburg$Tr > 0 &
                                                 miltenburg$Period == .x, ],
                             family = binomial))

fitlist_tw <- map(1:15, ~glm(ownhonor ~ treat2 + network + treat2network,
                             data = miltenburg[miltenburg$Tr == 0 &
                                                 miltenburg$Period == .x &
                                                 miltenburg$owntrust == 1, ],
                             family = binomial))

map(fitlist_tf, ~coeftest(.x, vcov. = vcovCL, cluster = ~person_id + group_id))
map(fitlist_tw, ~coeftest(.x, vcov. = vcovCL, cluster = ~person_id + group_id))

## Only marginally different SE's, but same coefficients,
## so replication succeeded.

# ---------------------- ANALYSES VOLKER, BUSKENS, RAUB ------------------------

## Trustfulness

miltenburg_tf <- miltenburg %>%
  filter(Period == 1 & Tr == 1)

tf_miltenburg_prop <- miltenburg_tf %>%
  group_by(network) %>%
  summarize(tf = mean(owntrust))

tf_miltenburg_glm <- glm(owntrust ~ network,
                         family = binomial,
                         data = miltenburg_tf)

tf_miltenburg_bf <- bain(coef(tf_miltenburg_glm),
                         hypothesis = "network > 0",
                         Sigma = vcovCL(tf_miltenburg_glm, cluster = ~person_id + session),
                         n = length(tf_miltenburg_glm$y))

## Trustworthiness analysis

miltenburg_tw <- miltenburg %>%
  filter(Period == 1,
         Tr == 1,
         owntrust == 1)

tw_miltenburg_prop <- miltenburg_tw %>%
  group_by(network) %>%
  summarize(tw = mean(ownhonor))

tw_miltenburg_glm <- glm(ownhonor ~ network,
                         family = binomial,
                         data = miltenburg_tw)

tw_miltenburg_bf <- bain(x = coef(tw_miltenburg_glm),
                         hypothesis = "network > 0",
                         Sigma = vcovCL(tw_miltenburg_glm, cluster = ~person_id + session),
                         n = length(tw_miltenburg_glm$y))


# ------------ MULTILEVEL ROBUSTNESS ANALYSES VOLKER, BUSKENS, RAUB ------------


tf_miltenburg_glmer <- glmer(owntrust ~ network + (1 | session/person_id),
                             family = binomial,
                             data = miltenburg_tf)
summary(tf_miltenburg_glmer)
## Singular fit, so we drop the session level (and then we can increase 
## nAGQ to 50, to get more accurate estimates).

tf_miltenburg_glmer <- glmer(owntrust ~ network + (1 | person_id),
                             family = binomial,
                             data = miltenburg_tf,
                             nAGQ = 50)

tf_miltenburg_glmer_bf <- bain(x = fixef(tf_miltenburg_glmer), 
                               hypothesis = "network > 0",
                               Sigma = as.matrix(vcov(tf_miltenburg_glmer)), 
                               n = length(tf_miltenburg_glmer@resp$n))

tw_miltenburg_glmer <- glmer(ownhonor ~ network + (1 | session/person_id),
                             family = binomial,
                             data = miltenburg_tw)
## Also singular fit, so we drop the session level, again.
tw_miltenburg_glmer <- glmer(ownhonor ~ network + (1 | person_id),
                             family = binomial,
                             data = miltenburg_tw)
summary(tw_miltenburg_glmer)
tw_miltenburg_glmer_bf <- bain(x = fixef(tw_miltenburg_glmer), 
                               hypothesis = "network > 0",
                               Sigma = as.matrix(vcov(tw_miltenburg_glmer)), 
                               n = length(tw_miltenburg_glmer@resp$n))

# -------------------------------- SAMPLE SIZE ---------------------------------

nrow(miltenburg_tf) + nrow(miltenburg_tw) ## Number of observations
length(unique(c(miltenburg_tf$person_id, miltenburg_tw$person_id))) ## number of participants

# -------------------------------- SAVE RESULTS --------------------------------

save.image("R/results/miltenburg.RData")

