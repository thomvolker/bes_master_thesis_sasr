
# ------------------------------- LOAD PACKAGES --------------------------------

library(dplyr)
library(tidyr)
library(lme4)
library(sandwich)
library(bain)
library(magrittr)
library(trend)
library(ggplot2)

# --------------------------------- LOAD DATA ----------------------------------


NoMin <- readxl::read_excel("data/duffy_xie_lee_2013/AllData.xlsx",
                            sheet = "NoMin")

MinNo <- readxl::read_excel("data/duffy_xie_lee_2013/AllData.xlsx",
                            sheet = "MinNo")

NoInfoCost <- readxl::read_excel("data/duffy_xie_lee_2013/AllData.xlsx",
                                 sheet = "NoInfoCost")

InfoNoCost <- readxl::read_excel("data/duffy_xie_lee_2013/AllData.xlsx",
                                 sheet = "InfoNoCost")


duffy1 <- bind_rows(NoMin, MinNo, .id = "Order")
duffy2 <- bind_rows(NoInfoCost, InfoNoCost, .id = "Order")


# ------------------------ REPRODUCE ORIGINAL RESULTS --------------------------


duffy1 %>%
  group_by(Session, Treatment) %>%
  summarize(`nSupergames` = length(unique(Sequence)),
            `nPeriods` = length(unique(Sequence * 100 + Round))) %>%
  pivot_wider(names_from = Treatment,
              values_from = c(nSupergames, nPeriods)) %>%
  mutate(nSupergames = paste0(nSupergames_1, "/", nSupergames_2),
         nPeriods = paste0(nPeriods_1, "/", nPeriods_2)) %>%
  select(-contains("_"))

## Order of numbers is NoInfo/MinInfo

duffy2 %>%
  group_by(Session, Treatment) %>%
  summarize(`nSupergames` = length(unique(Sequence)),
            `nPeriods` = length(unique(Sequence * 100 + Round))) %>%
  pivot_wider(names_from = Treatment,
              values_from = c(nSupergames, nPeriods)) %>%
  mutate(nSupergames = paste0(nSupergames_1, "/", nSupergames_2, "/", nSupergames_3),
         nPeriods = paste0(nPeriods_1, "/", nPeriods_2, "/", nPeriods_3)) %>%
  select(-contains("_"))

## Order of numbers is NoInfo / Info / Cost


duffy1 %>% # Replicate table 2 (in wide format)
  filter(Type == 1) %>%
  group_by(Order, Treatment, Session) %>%
  summarize(Inv = mean(Invest), 
            Ret = sum(Return, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %>%
  group_by(Order, Treatment) %>%
  summarize(Invest = mean(Inv),
            Return = mean(Ret),
            InvRet = mean(InvRet)) %>%
  mutate(order2 = ifelse((as.numeric(Order) - Treatment) == 0, 1, 0)) %>%
  select(-c(Order, Treatment)) %>%
  pivot_wider(names_from = order2, values_from = c(Invest, Return, InvRet))



# Replicate results p. 688 NoMin treatment and MinNo treatment
# Hypothesis that there is less trust and trustworthiness without information 
# provision than with minimal information provision in the NoMin treatment

duffy1 %>%
  filter(grepl("NoMin", Session), Type == 1) %>%
  group_by(Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {wilcox.test(Inv ~ Treatment, ., paired = T, alternative = "l") %>% print()} %T>%
  {wilcox.test(Ret ~ Treatment, ., paired = T, alternative = "l") %>% print()} %T>%
  {wilcox.test(InvRet ~ Treatment, ., paired = T, alternative = "l") %>% print()}

# Replication succeeded



# Hypothesis that there is less trust and trustworthiness without information 
# provision than with minimal information provision in the MinNo treatment

duffy1 %>%
  filter(grepl("MinNo", Session), Type == 1) %>%
  group_by(Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {wilcox.test(Inv ~ Treatment, ., paired = T, alternative = "l") %>% print()} %T>%
  {wilcox.test(Ret ~ Treatment, ., paired = T, alternative = "l") %>% print()} %T>%
  {wilcox.test(InvRet ~ Treatment, ., paired = T, alternative = "l") %>% print()}

# Also replicated (although the exact p-values and orderings are not) reported
# here.



# Test whether there is more trust under minimum information than under no
# information when both conditions are the first conditions played in the session

duffy1 %>%
  filter((Order == "1" & Treatment == 1)|(Order == "2" & Treatment == 2),
         Type == 1) %>%
  group_by(Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Treatment, ., alternative = "t") %>% print()} %T>%
  {rrod.test(Ret ~ Treatment, ., alternative = "t") %>% print()} %T>%
  {rrod.test(InvRet ~ Treatment, ., alternative = "t") %>% print()}

# All are non-significant, just as in the paper.



# Test whether there is more trust under minimum information than under no
# information when both conditions are the second conditions played in the session

duffy1 %>%
  filter((Order == "1" & Treatment == 2)|(Order == "2" & Treatment == 1),
         Type == 1) %>%
  group_by(Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Treatment, ., alternative = "t") %>% print()} %T>%
  {rrod.test(Ret ~ Treatment, ., alternative = "t") %>% print()} %T>%
  {rrod.test(InvRet ~ Treatment, ., alternative = "t") %>% print()}

# I find p = .11 for the second test, whereas duffy, xie & lee report .10. This
# might be due to differences in software, but it might also be due to a typo,
# or on some data handling error on my side.



# Test whether there are differences between invest, return-given-invest and 
# invest-and-return for no information between the two sets of sessions

duffy1 %>%
  filter(Treatment == 1, Type == 1) %>%
  group_by(Order, Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(Ret ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(InvRet ~ Order, ., alternative = "t") %>% print()}

# Seems correct



# Test whether there are differences between invest, return-given-invest and
# invest-and-return for minimum information between the two sessions.

duffy1 %>%
  filter(Treatment == 2, Type == 1) %>%
  group_by(Order, Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(Ret ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(InvRet ~ Order, ., alternative = "t") %>% print()}

# Also seems correct



## Replicate table 4, without the payoffs, note that i ignore section 5.2, 
## on finite horizons with no information

duffy2 %>%
  filter(Type == 1) %>%
  mutate(order2 = case_when(grepl("NoInfo", Session, fixed=F) & Treatment == 1 ~ 1,
                            grepl("NoInfo", Session, fixed=F) & Treatment == 2 ~ 2,
                            grepl("InfoNo", Session, fixed=F) & Treatment == 2 ~ 1,
                            grepl("InfoNo", Session, fixed=F) & Treatment == 1 ~ 2,
                            grepl("Cost", Session, fixed=F) ~ 3)) %>%
  group_by(Order, Treatment, Session, order2) %>%
  summarize(Inv = mean(Invest), 
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %>%
  group_by(order2, Order, Treatment) %>%
  summarize(Invest = mean(Inv),
            Return = mean(Ret),
            InvRet = mean(InvRet)) %>%
  select(-c(Order, Treatment)) %>%
  pivot_wider(names_from = order2, values_from = c(Invest, Return, InvRet)) %>%
  pivot_longer(cols = contains("_"), 
               names_to = c("var", ".value"),
               names_sep = "_")




## Replicate results p. 690 NoInfo treatment and InfoNo treatment. The cost
## treatment is deliberately ignored.

duffy2 %>%
  filter(Treatment < 3, Type == 1) %>%
  group_by(Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {wilcox.test(Inv ~ Treatment, ., paired = T, alternative = "l") %>% print()} %T>%
  {wilcox.test(Ret ~ Treatment, ., paired = T, alternative = "l") %>% print()} %T>%
  {wilcox.test(InvRet ~ Treatment, ., paired = T, alternative = "l") %>% print()}

duffy2 %>%
  filter((Order == "1" & Treatment == 1)|(Order == "2" & Treatment == 2),
         Type == 1) %>%
  group_by(Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Treatment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(Ret ~ Treatment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(InvRet ~ Treatment, ., alternative = "l") %>% print()}

duffy2 %>%
  filter((Order == "1" & Treatment == 2)|(Order == "2" & Treatment == 1),
         Type == 1) %>%
  group_by(Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Treatment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(Ret ~ Treatment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(InvRet ~ Treatment, ., alternative = "l") %>% print()}


duffy2 %>%
  filter((Order == "1" & Treatment == 1)|(Order == "2" & Treatment == 1),
         Type == 1) %>%
  group_by(Order, Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(Ret ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(InvRet ~ Order, ., alternative = "t") %>% print()}


duffy2 %>%
  filter((Order == "1" & Treatment == 2)|(Order == "2" & Treatment == 2),
         Type == 1) %>%
  group_by(Order, Session, Treatment) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(Ret ~ Order, ., alternative = "t") %>% print()} %T>%
  {rrod.test(InvRet ~ Order, ., alternative = "t") %>% print()}




## Replicate results p. 691-692 NoInfo treatment and InfoNo treatment. The cost
## treatment is deliberately ignored.

duffy12 <- bind_rows(duffy1 %>% select(Order, Session, Period, Subject, Group, 
                                       Round, Sequence, Treatment, Invest, 
                                       Return, Profit, Type),
                     duffy2 %>% select(Order, Session, Period, Subject, Group, 
                                       Round, Sequence, Treatment, Invest, 
                                       Return, Profit, Type),
                     .id = "Experiment")

duffy12 %>%
  filter(grepl("InfoNo", Session, fixed = TRUE) | 
           grepl("MinNo", Session, fixed = TRUE),
         Treatment == 2, Type == 1) %>%
  group_by(Experiment, Session) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Experiment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(Ret ~ Experiment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(InvRet ~ Experiment, ., alternative = "l") %>% print()}

duffy12 %>%
  filter(grepl("NoInfo", Session, fixed = TRUE) | 
           grepl("NoMin", Session, fixed = TRUE),
         Treatment == 2, Type == 1) %>%
  group_by(Experiment, Session) %>%
  summarize(Inv = mean(Invest),
            Ret = sum(Return == 1, na.rm=T) / sum(Invest),
            InvRet = mean(Invest * sapply(Return == 1, isTRUE))) %T>%
  {rrod.test(Inv ~ Experiment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(Ret ~ Experiment, ., alternative = "l") %>% print()} %T>%
  {rrod.test(InvRet ~ Experiment, ., alternative = "l") %>% print()}




## Replicate Fig. 5 & Section 5.5.1

duffy1 %>%
  filter(Message > -1, Type == 1) %>%
  mutate(Group = case_when(grepl("NoMin", Session, fixed=T) & Treatment == 2 ~ 2,
                           grepl("MinNo", Session, fixed=T) & Treatment == 2 ~ 3,
                           TRUE ~ 1)) %>%
  group_by(Treatment, Session, Message) %>%
  summarize(Invest = mean(Invest)) %>% 
  filter(Treatment == 1) %>%
  wilcox.test(Invest ~ Message, ., exact = F, correct = F, paired = T)

duffy1 %>%
  filter(Message > -1, Order == "1", Type == 1) %>%
  mutate(Group = case_when(grepl("NoMin", Session, fixed=T) & Treatment == 2 ~ 2,
                           grepl("MinNo", Session, fixed=T) & Treatment == 2 ~ 3,
                           TRUE ~ 1)) %>%
  group_by(Treatment, Session, Message) %>%
  summarize(Invest = mean(Invest)) %>% 
  filter(Treatment == 2) %>%
  wilcox.test(Invest ~ Message, ., exact = F, correct = F, paired = T)

duffy1 %>%
  filter(Message > -1, Order == "2", Type == 2) %>%
  mutate(Group = case_when(grepl("NoMin", Session, fixed=T) & Treatment == 2 ~ 2,
                           grepl("MinNo", Session, fixed=T) & Treatment == 2 ~ 3,
                           TRUE ~ 1)) %>%
  group_by(Treatment, Session, Message) %>%
  summarize(Invest = mean(Invest)) %>% 
  filter(Treatment == 2) %>%
  wilcox.test(Invest ~ Message, ., exact = F, correct = F, paired = T)

duffy1 %>%
  filter(Message > -1) %>%
  mutate(Group = case_when(grepl("NoMin", Session, fixed=T) & Treatment == 2 ~ 2,
                           grepl("MinNo", Session, fixed=T) & Treatment == 2 ~ 3,
                           TRUE ~ 1)) %>%
  group_by(Group, Message) %>%
  summarize(Invest = mean(Invest)) %>%
  ggplot(aes(x = Group, y = Invest, fill = Message)) +
  geom_bar(stat = "identity", position = position_dodge2())

## Seems similar, but impossible to determine the exact numbers.



## Replicate figure 6 and section 5.5.2

plot6data1 <- duffy1 %>%
  filter(Type == 1, Round > 1, Treatment == 2) %>%
  mutate(all_return = Message == 1, 
         all_keep   = Message == 0)

plot6data1 %>%
  filter(all_keep | all_return) %>%
  pivot_longer(c(all_keep, all_return)) %>%
  filter(value) %>%
  group_by(name) %>%
  summarize(Invest = mean(Invest)) %>%
  ggplot(aes(x = name, y = Invest, fill = name)) +
  geom_hline(yintercept = c(0:10)/10, alpha = 0.3) +
  geom_col() +
  ylim(0, 1) + 
  theme_classic()

## Looks very much the same.
  
plot6data2 <- duffy2 %>%
  filter(Treatment == 2, Type == 1) %>%
  mutate(all_return = (MessageNumReturn / MessageNumInvest) == 1,
         all_keep   = (MessageNumReturn / MessageNumInvest) == 0)


plot6data2 %>% 
  filter(all_keep | all_return) %>%
  pivot_longer(c(all_keep, all_return)) %>%
  filter(value) %>%
  group_by(name) %>%
  summarize(Invest = mean(Invest)) %>%
  ggplot(aes(x = name, y = Invest, fill = name)) +
  geom_hline(yintercept = c(0:10)/10, alpha = 0.3) +
  geom_col() +
  ylim(0, 1) + 
  theme_classic()

# Also looks the same.

bind_rows(plot6data1 %>% filter(all_keep | all_return),
          plot6data2 %>% filter(all_keep | all_return), 
          .id = "Treatment") %>%
  pivot_longer(c(all_keep, all_return)) %>%
  filter(value) %>%
  group_by(name, Treatment, Session) %>%
  summarize(Invest = mean(Invest)) %>%
  filter(name == "all_return") %>%
  rrod.test(Invest ~ Treatment, .)

bind_rows(plot6data1 %>% filter(all_keep | all_return),
          plot6data2 %>% filter(all_keep | all_return), 
          .id = "Treatment") %>%
  pivot_longer(c(all_keep, all_return)) %>%
  filter(value) %>%
  group_by(name, Treatment, Session) %>%
  summarize(Invest = mean(Invest)) %>%
  filter(name == "all_keep") %>%
  rrod.test(Invest ~ Treatment, .)


## In agreement with reported results


## Replicate minimal information model from table 8

duffy_tab8_min_dat <- duffy1 %>%
  filter(Type == 1, Treatment == 2) %>%
  arrange(Session, Subject, Sequence, Round) %>%
  group_by(Session, Subject, Sequence) %>%
  mutate(Return = ifelse(is.na(Return), -1, Return),
         Order = as.numeric(Order) - 1,
         seen_abuse = ifelse(Return == 0, 1, 0),
         grim = cummax(lag(seen_abuse, default = 0)),
         tft1 = ifelse(lag(Return, default = 1) == 0, 1, 0),
         tft2 = ifelse(lag(Return, n = 2, default = 1) == 0, 1, 0),
         last_return = ifelse(Message == 1, 1, 0),
         grim_last_return = grim*last_return,
         person_id = as.numeric(factor(Session)) * 100 + Subject) %>%
  filter(Round > 1)

## Replication

duffy_tab8_min_rep <- glmer(Invest ~ Round + Sequence + Order + grim + tft1 + tft2 +
                              last_return + grim_last_return + (1 | Session/Subject),
                            family = binomial(link = "probit"),
                            data = duffy_tab8_min_dat,
                            control = glmerControl(optimizer = "bobyqa"))

summary(duffy_tab8_min_rep)

## Numbers are not exactly the same, but very close and lead to the same similar
## conclusions

## To get an exact replication, run the following line to obtain the dta file

duffy_tab8_min_dat %>%
  mutate(Session = factor(Session)) %>%
  readstata13::save.dta13("data/duffy_xie_lee_2013/duffy1_rep_tab8_min.dta")

## And run the following, outcommented, lines in stata

# use "data\duffy_xie_lee_2013\duffy1_rep_tab8_min.dta", clear
# 
# gllamm Invest Round Sequence Order grim tft1 tft2 last_return ///
#   grim_last_return, family(bin) link(pro) i(Subject Session)




## Replicate full information model from table 9

duffy_tab9_full_dat <- duffy2 %>%
  filter(Type == 1, Treatment == 2) %>%
  arrange(Session, Subject, Sequence, Round) %>%
  group_by(Session, Subject, Sequence) %>%
  mutate(Return = ifelse(is.na(Return), -1, Return),
         Order = as.numeric(Order) - 1,
         seen_abuse = ifelse(Return == 0, 1, 0),
         grim = cummax(lag(seen_abuse, default = 0)),
         tft1 = ifelse(lag(Return, default = 1) == 0, 1, 0),
         tft2 = ifelse(lag(Return, n = 2, default = 1) == 0, 1, 0),
         last_return = ifelse(history1 == 1, 1, 0),
         grim_last_return = grim*last_return,
         person_id = as.numeric(factor(Session)) * 100 + Subject) %>%
  mutate(all_return    = sapply((MessageNumReturn / MessageNumInvest) == 1, isTRUE),
         all_no_return = sapply((MessageNumReturn / MessageNumInvest) == 0, isTRUE)) %>%
  ungroup() %>% 
  filter(Round > 1)

duffy_tab9_full_rep <- glmer(Invest ~ Round + Sequence + Order + grim + tft1 + tft2 +
                               last_return + all_return + all_no_return + grim_last_return + 
                               (1 | Session/Subject),
                             family = binomial(link = "probit"),
                             data = duffy_tab9_full_dat,
                             control = glmerControl("bobyqa"))


summary(duffy_tab9_full_rep)

## Numbers are not exactly the same, but quite close

duffy_tab9_full_dat %>%
  mutate(Session = factor(Session)) %>%
  readstata13::save.dta13("data/duffy_xie_lee_2013/duffy2_rep_tab9_full.dta")

## And run the following, outcommented, lines in stata

# use "data\duffy_xie_lee_2013\duffy2_rep_tab9_full.dta", clear
# 
# gllamm Invest Round Sequence Order grim tft1 tft2 all_return all_no_return ///
#   last_return grim_last_return, ///
#   family(bin) link(pro) i(Subject Session)




# ----------------------- ANALYSES VOLKER, BUSKENS, RAUB -----------------------

# Trustfulness

duffy_min_tf <- duffy1 %>% # NO VERSUS MINIMUM INFORMATION TRUSTFULNESS DATA
  filter(Round == 1, 
         Type == 1) %>%
  mutate(IDinSession = as.numeric(factor(Session)) * 100 + Subject,
         Treatment = factor(Treatment))

duffy_full_tf <- duffy2 %>% # NO VERSUS FULL INFORMATION TRUSTFULNESS DATA
  filter(Round == 1, 
         Type == 1,
         Treatment < 3) %>%
  mutate(IDinSession = as.numeric(factor(Session)) * 100 + Subject,
         Treatment = factor(Treatment))

tf_duffy_min_prop <- duffy_min_tf %>% # NO VERSUS MINIMUM PROPORTION TRUSTFULNESS
  group_by(Treatment) %>%
  summarize(tf = mean(Invest))

tf_duffy_full_prop <- duffy_full_tf %>% # NO VERSUS FULL PROPORTION TRUSTFULNESS
  group_by(Treatment) %>%
  summarize(tf = mean(Invest))

tf_duffy_min_glm <- glm(Invest ~ Treatment, # NO_MIN LOGISTIC REGRESSION MODEL
                        family = binomial,
                        data = duffy_min_tf)

tf_duffy_full_glm <- glm(Invest ~ Treatment, # NO_FULL LOGISTIC REGRESSION MODEL
                         family = binomial,
                         data = duffy_full_tf)

summary(tf_duffy_min_glm)
summary(tf_duffy_full_glm)


tf_duffy_min_bf <- bain(x = coef(tf_duffy_min_glm), # NO_MIN BAYES FACTOR
                        hypothesis = "Treatment2 > 0",
                        Sigma = vcovCL(tf_duffy_min_glm, ~ Session + IDinSession),
                        n = length(tf_duffy_min_glm$y))

tf_duffy_full_bf <- bain(x = coef(tf_duffy_full_glm), # NO_FULL BAYES FACTOR
                         hypothesis = "Treatment2 > 0",
                         Sigma = vcovCL(tf_duffy_full_glm, ~ Session + IDinSession),
                         n = length(tf_duffy_full_glm$y))




# Trustworthiness

duffy_min_tw <- duffy1 %>% # NO_MIN DATA TRUSTWORTHINESS
  filter(Round == 1, 
         Type == 2,
         Invest == 1) %>%
  mutate(IDinSession = as.numeric(factor(Session)) * 100 + Subject,
         Treatment = factor(Treatment))

duffy_full_tw <- duffy2 %>% # NO_FULL DATA TRUSTWORTHINESS
  filter(Round == 1, 
         Type == 2,
         Treatment < 3,
         Invest == 1) %>%
  mutate(IDinSession = as.numeric(factor(Session)) * 100 + Subject,
         Treatment = factor(Treatment))

tw_duffy_min_prop <- duffy_min_tw %>% # NO_MIN PROPORTION TRUSTWORTHINESS
  group_by(Treatment) %>%
  summarize(tw = mean(Return))

tw_duffy_full_prop <- duffy_full_tw %>% #NO_FULL PROPORTION TRUSTWORTINESS
  group_by(Treatment) %>%
  summarize(tw = mean(Return))

tw_duffy_min_glm <- glm(Return ~ Treatment, # NO_MIN LOGISTIC REGRESSION MODEL
                        family = binomial,
                        data = duffy_min_tw)

tw_duffy_full_glm <- glm(Return ~ Treatment, # NO_FULL LOGISTIC REGRESSION MODEL
                         family = binomial,
                         data = duffy_full_tw)

summary(tw_duffy_min_glm)
summary(tw_duffy_full_glm)


tw_duffy_min_bf <- bain(x = coef(tw_duffy_min_glm), # NO_MIN BAYES FACTOR TRUSTWORTHINESS
                        hypothesis = "Treatment2 > 0",
                        Sigma = vcovCL(tw_duffy_min_glm, ~ Session + IDinSession),
                        n = length(tw_duffy_min_glm$y))

tw_duffy_full_bf <- bain(x = coef(tw_duffy_full_glm), # NO_FULL BAYES FACTOR TRUSTWORTHINESS
                         hypothesis = "Treatment2 > 0",
                         Sigma = vcovCL(tw_duffy_full_glm, ~ Session + IDinSession),
                         n = length(tw_duffy_full_glm$y))


# -------- MULTILEVEL MODEL ROBUSTNESS ANALYSES VOLKER, BUSKENS, RAUB ----------

tf_duffy_min_glmer <- glmer(Invest ~ Treatment + (1 | Session/IDinSession), 
                            family = binomial,
                            data = duffy_min_tf)

tf_duffy_min_glmer_bf <- bain(fixef(tf_duffy_min_glmer),
                              hypothesis = "Treatment2 > 0",
                              Sigma = as.matrix(vcov(tf_duffy_min_glmer)),
                              n = length(tf_duffy_min_glmer@resp$n))
# Note that the choice of n is not important here, as it only affects the prior 
# variance. Since we consider a one parameter hypothesis centered at the 
# boundary of the hypothesis space, the complexity is always 0.5, regardless of 
# the prior variance.

tf_duffy_full_glmer <- glmer(Invest ~ Treatment + (1 | Session/Subject),
                             family = binomial,
                             data = duffy_full_tf)
summary(tf_duffy_full_glmer)
## Failed to converge, due to lack of variance at session level.
tf_duffy_full_glmer <- glmer(Invest ~ Treatment + (1 | IDinSession),
                             family = binomial,
                             data = duffy_full_tf)


tf_duffy_full_glmer_bf <- bain(x = fixef(tf_duffy_full_glmer),
                               hypothesis = "Treatment2 > 0",
                               Sigma = as.matrix(vcov(tf_duffy_full_glmer)),
                               n = length(tf_duffy_full_glmer@resp$n))



tw_duffy_min_glmer <- glmer(Return ~ Treatment + (1 | Session/Subject),
                            family = binomial,
                            data = duffy_min_tw)


tw_duffy_full_glmer <- glmer(Return ~ Treatment + (1 | Session / Subject),
                             family = binomial,
                             data = duffy_full_tw)

tw_duffy_min_glmer_bf <- bain(x = fixef(tw_duffy_min_glmer),
                              hypothesis = "Treatment2 > 0",
                              Sigma = as.matrix(vcov(tw_duffy_min_glmer)),
                              n = length(tf_duffy_min_glmer@resp$n))

tw_duffy_full_glmer_bf <- bain(x = fixef(tw_duffy_full_glmer),
                               hypothesis = "Treatment2 > 0",
                               Sigma = as.matrix(vcov(tw_duffy_full_glmer)),
                               n = length(tf_duffy_full_glmer@resp$n))


# -------------------------------- SAMPLE SIZE ---------------------------------

nrow(duffy_min_tf) + nrow(duffy_min_tw) + nrow(duffy_full_tf) + nrow(duffy_full_tw)

## Number of participants considered
length(unique(c(duffy_min_tf$IDinSession, duffy_min_tw$IDinSession))) +
  length(unique(c(duffy_full_tf$IDinSession, duffy_full_tw$IDinSession)))

# ------------------------------- SAVE RESULTS ---------------------------------

save.image("R/results/duffy_xie_lee.RData")


