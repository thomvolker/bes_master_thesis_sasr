

# --------------------------- LOAD REQUIRED PACKAGES ---------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(bain)

# ------------------------------- LOAD DATA SETS -------------------------------

datanames <- paste0("data/seinen_schram_2006/sessie", 1:6, ".RDS")

seinen <- map_dfr(datanames, ~ readRDS(.x), .id = "Session")

# ----------------------------- DATA MANIPULATION ------------------------------
#
# Check for non-overlapping groups and create group ID within session,
# also create the treatment ID's
# ______________________________________________________________________________


purrr::map(paste0(1:6), ~ seinen %>% filter(Session == .x) %$% table(DonorID, RecipientID))

seinen <- seinen %>%
  mutate(GroupID = case_when(Session == 1 & DonorID < 15 ~ 1,
                             Session == 1 & DonorID > 14 ~ 2,
                             Session == 2 & DonorID < 15 ~ 1,
                             Session == 2 & DonorID > 14 ~ 2,
                             Session == 3 & DonorID < 15 ~ 1,
                             Session == 3 & DonorID > 14 ~ 2,
                             Session == 4 & DonorID < 15 ~ 1,
                             Session == 4 & DonorID > 14 ~ 2,
                             Session == 5 & DonorID < 15 ~ 1,
                             Session == 5 & DonorID > 14 ~ 2,
                             Session == 6 & DonorID < 11 ~ 1,
                             Session == 6 & DonorID > 10 ~ 2),
         Treatment = case_when(Session %in% c(1,2) ~ "LCI",
                               Session %in% c(3,4) ~ "HCI",
                               Session %in% c(5,6) ~ "HCN"),
         Session = as.numeric(Session),
         GroupInTreatment = case_when(Session %in% c(1,2) ~ paste0((Session * 10) + GroupID),
                                      Session %in% c(3,4) ~ paste0(((Session - 2) * 10) + GroupID),
                                      Session %in% c(5,6) ~ paste0(((Session - 4) * 10) + GroupID)))

# -------------------------- REPRODUCE ORIGINAL RESULTS ------------------------

## Recreate figures that show cooperation over time

seinen %>%
  group_by(Treatment, Round) %>%
  summarize(Helpful = mean(-(Choice - 2))) %>%
  ungroup() %>%
  ggplot(aes(x = Round, y = Helpful, col = Treatment, linetype = Treatment)) +
  geom_line() +
  theme_minimal() +
  xlim(0, 90)

seinen %>%
  arrange(Treatment, GroupInTreatment, Round) %>%
  group_by(Treatment, GroupInTreatment, Round) %>%
  summarize(total_help_period = sum(-(Choice - 2)),
            nobs = n()) %>%
  group_by(Treatment, GroupInTreatment) %>%
  mutate(cum_mean = RcppRoll::roll_sum(total_help_period, n = 7, fill = 0, align = "right") / 
           RcppRoll::roll_sum(nobs, n = 7, fill = 0, align = "right")) %>%
  ggplot(aes(x = Round, 
             y = cum_mean, 
             col = GroupInTreatment, 
             linetype = GroupInTreatment)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~Treatment) +
  xlim(0, 90)


## Recreate tables with fraction helpful choices per group per treatment

seinen %>%
  filter(Round < 91) %>%
  group_by(Treatment, GroupInTreatment) %>%
  summarize(Helpful = mean(-(Choice - 2))) %>%
  tidyr::pivot_wider(names_from = GroupInTreatment,
                     values_from = Helpful) %>%
  left_join(., 
            seinen %>% 
              filter(Round < 91) %>% 
              group_by(Treatment) %>%
              summarize(Total = mean(-(Choice - 2))))

## Recreate data for figure with helpful choices dependent on the recipients status

seinen <- seinen %>%
  arrange(Treatment, GroupInTreatment, DonorID, Round) %>%
  group_by(Treatment, GroupInTreatment, DonorID) %>%
  mutate(pastAA = lag(ChoicesAA),
         pastAB = lag(ChoicesAB),
         pastBA = ChoicesBA,
         pastBB = ChoicesBB)

seinen %>%
  filter(Round < 91,
         (pastAA + pastAB) > 5,
         (pastBA + pastBB) > 5,
         Treatment == "HCI") %$%
  table(-(Choice - 2), pastBA) %>%
  addmargins(c(1,2))  # Exact same numbers



## Mann-Whitney tests reported in paper (not able to check more than just the
## significance levels)

seinen %>%
  filter(Treatment != "HCI") %>%
  group_by(Treatment, Session, GroupID) %>%
  summarize(Helpful = mean(-(Choice - 2))) %>%
  wilcox.test(Helpful ~ Treatment, data = .)

seinen %>%
  filter(Treatment != "LCI") %>%
  group_by(Treatment, Session, GroupID) %>%
  summarize(Helpful = mean(-(Choice - 2))) %>%
  wilcox.test(Helpful ~ Treatment, data = .)

seinen %>%
  filter(Treatment != "HCN") %>%
  group_by(Treatment, Session, GroupID) %>%
  summarize(Helpful = mean(-(Choice - 2))) %>%
  wilcox.test(Helpful ~ Treatment, data = .)

# All the same



## Recreate numbers for Figure 4

seinen %>%
  mutate(pastAA = ifelse(Treatment == "HCN", ChoicesAA, pastAA),
         pastAB = ifelse(Treatment == "HCN", ChoicesAB, pastAB)) %>%
  filter(Round < 91, 
         (pastAA + pastAB) == 6,
         (pastBA + pastBB) == 6,
         Treatment != "LCI") %$%
  table(-(Choice - 2), pastAA, Treatment) %>%
  addmargins(c(1,2))

seinen %>%
  mutate(pastAA = ifelse(Treatment == "HCN", ChoicesAA, pastAA),
         pastAB = ifelse(Treatment == "HCN", ChoicesAB, pastAB)) %>%
  filter(Round < 91, 
         (pastAA + pastAB) == 6,
         (pastBA + pastBB) == 6,
         Treatment != "LCI") %$%
  table(-(Choice - 2), pastAA, Treatment) %>%
  prop.table(margin = c(2, 3))

## I have a strong feeling that the authors forgot to create a lagged variable
## for the pastAA and pastAB conditions in the HCN treatment, although, of
## course, I cannot proof this. Additionally, this hardly affects the results of
## the paper (i.e., note that pastAA and pastAB are the lagged versions of the 
## variables ChoicesAA and ChoicesAB).

## Then, the following would be correct

seinen %>%
  filter(Round < 91, 
         (pastAA + pastAB) == 6,
         (pastBA + pastBB) == 6,
         Treatment != "LCI") %$%
  table(-(Choice - 2), pastAA, Treatment) %>%
  addmargins(c(1,2))

seinen %>%
  filter(Round < 91, 
         (pastAA + pastAB) == 6,
         (pastBA + pastBB) == 6,
         Treatment != "LCI") %$%
  table(-(Choice - 2), pastAA, Treatment) %>%
  prop.table(margin = c(2, 3))




# ----------------------- ANALYSES VOLKER, BUSKENS, RAUB -----------------------


seinen_coop <- seinen %>%   # EXTRACT DATA COOPERATION
  filter(Round == 1, Treatment %in% c("HCI", "HCN")) %>%
  mutate(Help = -(Choice - 2),
         Treatment = factor(Treatment, 
                            levels = c("HCN", "HCI")))

coop_seinen_prop <- seinen_coop %>% # PROPORTION HELPFUL CHOICES PER GROUP
  group_by(Treatment) %>%
  summarize(coop = mean(Help))

coop_seinen_glm <- glm(Help ~ Treatment, # FIT LOGISTIC REGRESSION MODEL
                       family = binomial,
                       data = seinen_coop)

summary(coop_seinen_glm) # LOGISTIC REGRESSION ESTIMATES



coop_seinen_bf <- bain(coef(coop_seinen_glm),  # CALCULATE BAYES FACTORS
                       hypothesis = "TreatmentHCI > 0",
                       Sigma = vcov(coop_seinen_glm),
                       n = length(coop_seinen_glm$y))


# ADDITIONAL ANALYSIS NOT DISCUSSED IN PAPER.

seinen_coop2 <- seinen %>%                                 # ADDITIONAL ANALYSIS
  arrange(Treatment, GroupInTreatment, DonorID, Round) %>% # NOT IN PAPER  
  group_by(Treatment, GroupInTreatment, DonorID) %>%       # OPERATIONALIZE NOT
  mutate(ChoiceNum = row_number()) %>%                     # AS FIRST ROUND BUT
  filter(Round == ChoiceNum,                               # AS ROUNDS IN WHICH
         Treatment %in% c("HCI", "HCN")) %>%               # RECIPIENT HAS NOT 
  mutate(Help = -(Choice - 2),                             # BEEN A DONOR, AND 
         Treatment = factor(Treatment,                     # THUS COULD NOT HAVE
                            levels = c("HCN", "HCI")),     # LEARNED SOMETHING
         UnqDonorID = Session * 100 + DonorID)

coop_seinen_prop2 <- seinen_coop2 %>%
  group_by(Treatment) %>%
  summarize(coop = mean(Help))

coop_seinen_glm2 <- glm(Help ~ Treatment, 
                        family = binomial,
                        data = seinen_coop2)

summary(coop_seinen_glm2)


coop_seinen_bf2 <- bain(coef(coop_seinen_glm2),
                       hypothesis = "TreatmentHCI > 0",
                       Sigma = sandwich::vcovCL(coop_seinen_glm2, cluster = ~ UnqDonorID),
                       n = length(coop_seinen_glm2$y))


# -------------------------------- SAMPLE SIZE ---------------------------------
nrow(seinen_coop) ## number of actions and number of participants considered.

# ------------------------------- SAVE RESULTS ---------------------------------

save.image("R/results/seinen.RData")
