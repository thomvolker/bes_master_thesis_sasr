
# ------------------------------ LOAD PACKAGES ---------------------------------

library(dplyr)
library(ggplot2)
library(sandwich)
library(lme4)
library(bain)

# -------------------------------- LOAD DATA -----------------------------------

corten <- readstata13::read.dta13("data/corten_etal_2016/repexp_cortenetal_2016_V02.dta")
corten <- filter(corten, sampled == 1)

# ------------------------------- REPLICATIONS ---------------------------------

bind_rows(
  `1.Rounds 1-35` =
    corten %>%
    filter(treatment == "main",
           Status != "no choice",
           Period < 36) %>%
    group_by(Reputation, berkeley) %>%
    summarize(Cooperation = mean(Status == "Cooperate")),
  `2.First interactions` = 
    corten %>%
    filter(treatment =="main", Status != "no choice") %>%
    arrange(session, id_subj1, id_subj2, Period) %>%
    group_by(session, id_subj1, id_subj2) %>%
    mutate(first_int = row_number() == 1) %>%
    filter(first_int) %>%
    group_by(Reputation, berkeley) %>%
    summarize(Cooperation = mean(Status == "Cooperate")),
  .id = "Rounds"
) %>%
  ggplot(aes(x = Reputation, y = Cooperation, fill = Rounds)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~berkeley) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

## Seems pretty similar, but hard to determine whether the values are exactly 
## the same.

corten %>%
  filter(treatment == "main",
         Status != "no choice",
         Period < 36,
         berkeley == "Experiment 1") %>%
  group_by(Reputation, id_group) %>%
  summarize(Cooperation = mean(Status == "Cooperate")) %>%
  ungroup() %>%
  mutate(rank = rank(Cooperation)) %>% 
  group_by(Reputation) %>%
  wilcox.test(Cooperation ~ Reputation, ., exact = F, correct = F)

# Exactly equal as numbers reported by 

corten %>%
  filter(treatment == "main",
         Status != "no choice") %>%
  arrange(session, id_subj1, id_subj2, Period) %>%
  group_by(session, id_subj1, id_subj2) %>%
  mutate(first_int = row_number() == 1) %>%
  filter(first_int,
         berkeley == "Experiment 1") %>%
  group_by(Reputation, id_group) %>%
  summarize(Cooperation = mean(Status == "Cooperate")) %>%
  wilcox.test(Cooperation ~ Reputation, data = ., exact = F, correct = F)

## Exactly same numbers as reported.

corten %>%
  filter(treatment == "main",
         Status != "no choice",
         berkeley == "Experiment 2",
         Period < 36) %>%
  group_by(Reputation, id_group) %>%
  summarize(Cooperation = mean(Status == "Cooperate")) %>%
  ungroup() %>%
  mutate(rank = rank(Cooperation)) %>%
  group_by(Reputation) %>%
  wilcox.test(Cooperation ~ Reputation, ., exact = F, correct = F)

## P value equals the reported p-value

corten %>%
  filter(treatment == "main", 
         Status != "no choice") %>%
  arrange(session, id_subj1, id_subj2, Period) %>%
  group_by(session, id_subj1, id_subj2) %>%
  mutate(first_int = row_number() == 1) %>%
  filter(first_int,
         berkeley == "Experiment 2") %>%
  group_by(Reputation, id_group) %>%
  summarize(Cooperation = mean(Status == "Cooperate")) %>%
  wilcox.test(Cooperation ~ Reputation, ., exact = F, correct = F)

## Exactly the same p-value

corten %>%
  filter(treatment == "main",
         Status != "no choice",
         Period < 36) %>%
  group_by(Reputation, id_group) %>%
  summarize(Cooperation = mean(Status == "Cooperate")) %>%
  ungroup() %>%
  mutate(rank = rank(Cooperation)) %>%
  group_by(Reputation) %>%
  wilcox.test(Cooperation ~ Reputation, ., exact = F, correct = F)

## P value equals the reported p-value

corten %>%
  filter(treatment == "main", 
         Status != "no choice") %>%
  arrange(session, id_subj1, id_subj2, Period) %>%
  group_by(session, id_subj1, id_subj2) %>%
  mutate(first_int = row_number() == 1) %>%
  filter(first_int) %>%
  group_by(Reputation, id_group) %>%
  summarize(Cooperation = mean(Status == "Cooperate")) %>%
  wilcox.test(Cooperation ~ Reputation, ., exact = F, correct = F)

## p-value equals the reported p-value. 

corten %>%
  filter(treatment == "main", 
         Status != "no choice",
         Period > 29 & Period < 36) %>%
  group_by(Reputation, berkeley, id_group) %>%
  summarize(Cooperation = mean(Status == "Cooperate")) %>%
  group_by(Reputation, berkeley) %>%
  summarize(Cooperation_Mu = mean(Cooperation),
            Cooperation_SD = sd(Cooperation))

# Embeddedness conditions are probably swapped in the paper.  

# ---------------------- ANALYSES VOLKER, BUSKENS, RAUB ------------------------

corten_coop <- corten %>% # first round cooperation data
  filter(Period == 1, 
         treatment == "main",
         Status != "no choice") %>%
  mutate(Cooperate = ifelse(Status == "Cooperate", 1, 0))
  
coop_corten_prop <- corten_coop %>% # proportion cooperation
  group_by(Reputation) %>%
  summarize(coop = mean(Cooperate))

coop_corten_glm <- glm(Cooperate ~ Reputation, # fit logistic regression model 
                       family = binomial,
                       data = corten_coop)

summary(coop_corten_glm)

coop_corten_bf <- bain(coef(coop_corten_glm), # bayes factors
                       hypothesis = "Reputationyes > 0",
                       Sigma = vcovCL(coop_corten_glm, cluster = ~id_subj1),
                       n = length(coop_corten_glm$y))

# ---------------- ROBUSTNESS ANALYSIS VOLKER, BUSKENS, RAUB -------------------

coop_corten_glmer <- glmer(Cooperate ~ Reputation + (1 | id_subj1),
                           data = corten_coop,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 50)

summary(coop_corten_glmer)

coop_corten_glmer_bf <- bain(fixef(coop_corten_glmer),
                             hypothesis = "Reputationyes > 0",
                             Sigma = as.matrix(vcov(coop_corten_glmer)),
                             n = length(coop_corten_glmer@resp$n))

coop_corten_glmer_bf

# ------------------------------- SAMPLE SIZE ----------------------------------

nrow(corten_coop) ## number of actions
length(unique(corten_coop$id_subj1)) ## number of participants

# ------------------------------ SAVE RESULTS ----------------------------------

save.image("R/results/corten.RData")
  