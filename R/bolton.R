
# ------------------------------ LOAD PACKAGES ---------------------------------

library(magrittr)
library(ggplot2)
library(plm)
library(lme4)
library(dplyr)
library(bain)


# -------------------------------- LOAD DATA -----------------------------------

bolton <- readxl::read_excel("data/bolton_katok_ockenfels_2004/BoltonKatokOckenfelsMS04Data.xls")

# --------------------- DATA MANIPULATION FOR REPLICATION ----------------------

bolton <- bolton %>%
  mutate(Treat = factor(Treat, 
                        levels = c(0, 1, 2), 
                        labels = c("Strangers", "Feedback", "Partners"))) %>%
  arrange(Sess, Seller, Rnd) %>%
  group_by(Sess, Seller) %>%
  mutate(TOTALSHIPfeedback   = cumsum(sapply(lag(Ship, 2) > 0, isTRUE)), #ifelse(Treat == "Feedback", cumsum(sapply(lag(Ship == 1), isTRUE)), 0), # Remove Treat statement eventually
         TOTALNOSHIPfeedback = cumsum(sapply(lag(Ship, 2) < 0, isTRUE)), #ifelse(Treat == "Feedback", cumsum(sapply(lag(Ship == -1), isTRUE)), 0),
         LASTBUYSHIPPED = case_when(is.na(lag(Buy)) ~ 0,
                                    lag(Buy == 1) ~ ifelse(lag(Ship > 0), 1, 0)),
         LASTBUYNSHIPPED = case_when(is.na(lag(Buy)) ~ 0,
                                     lag(Buy == 1) ~ ifelse(lag(Ship < 0), 1, 0)),
         SHIPLASTstrangers  = ifelse(Treat == "Strangers" & sapply(lag(Ship) > 0, isTRUE), 1, 0),
         SHIPLASTfeedback   = ifelse(Treat == "Feedback"  & sapply(lag(Ship) > 0, isTRUE), 1, 0),
         SHIPLASTpartners   = ifelse(Treat == "Partners"  & sapply(lag(Ship) > 0, isTRUE), 1, 0),
         NSHIPLASTstrangers = ifelse(Treat == "Strangers" & sapply(lag(Ship) < 0, isTRUE), 1, 0),
         NSHIPLASTfeedback  = ifelse(Treat == "Feedback"  & sapply(lag(Ship) < 0, isTRUE), 1, 0),
         NSHIPLASTpartners  = ifelse(Treat == "Partners"  & sapply(lag(Ship) < 0, isTRUE), 1, 0)) %>% #,
  tidyr::fill(c(LASTBUYSHIPPED, LASTBUYNSHIPPED)) %>%
  ungroup() %>%
  arrange(Sess, Buyer, Rnd) %>% 
  group_by(Sess, Buyer) %>%
  mutate(SHIPLAST2strangers = ifelse(Treat == "Strangers", LASTBUYSHIPPED, 0),
         SHIPLAST2feedback  = ifelse(Treat == "Feedback", LASTBUYSHIPPED, 0),
         SHIPLAST2partners  = ifelse(Treat == "Partners", LASTBUYSHIPPED, 0),
         NSHIPLAST2feedback = ifelse(Treat == "Feedback", LASTBUYNSHIPPED, 0),
         NSHIPLAST2partners = ifelse(Treat == "Partners", LASTBUYNSHIPPED, 0),
         CBSHIP   = cumsum(sapply(lag(Ship) > 0, isTRUE)),
         CBNOSHIP = cumsum(sapply(lag(Ship) < 0, isTRUE)),
         CBSHIP2  = ifelse(Treat == "Partners", cumsum(sapply(lag(Ship, 2) > 0, isTRUE)), CBSHIP),
         CBNOSHIP2 = ifelse(Treat == "Partners", cumsum(sapply(lag(Ship, 2) < 0, isTRUE)), CBNOSHIP)) %>%
  ungroup() %>% 
  mutate(ROUNDstrangers = Rnd * (Treat == "Strangers"),
         ROUNDfeedback = Rnd * (Treat == "Feedback"),
         ROUNDpartners = Rnd * (Treat == "Partners"),
         LAST2ROUNDstrangers = ifelse(Treat == "Strangers" & Rnd > 28, 1, 0),
         LAST2ROUNDfeedback = ifelse(Treat == "Feedback" & Rnd > 28, 1, 0),
         LAST2ROUNDpartners = ifelse(Treat == "Partners" & Rnd > 28, 1, 0),
         IDBuyer = Sess * 100 + Buyer,
         IDSeller = Sess * 100 + Seller,
         IDRnd = Sess * 10000 + Buyer * 100 + Rnd) %>%
  select(-c(...9, ...10))


# ---------------------------- REPLICATE FIGURES -------------------------------


bolton %>%
  group_by(Treat, Rnd) %>%
  summarize(tf = mean(Buy)) %>%
  ggplot(aes(x = Rnd, y = tf, col = Treat, linetype = Treat, shape = Treat)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ylim(0, 1)

bolton %>%
  filter(Buy == 1) %>%
  group_by(Treat, Rnd) %>%
  summarize(tw = mean(Jship)) %>%
  ggplot(aes(x = Rnd, y = tw, col = Treat, linetype = Treat, shape = Treat)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ylim(0, 1)

bolton %>%
  group_by(Treat, Rnd) %>%
  summarize(eff = mean(Jship)) %>%
  ggplot(aes(x = Rnd, y = eff, col = Treat, linetype = Treat, shape = Treat)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ylim(0, 1)

# --------------------------- REPLICATE ANALYSES -------------------------------

## Table 1 - Model 1

tab1mod1 <- glmer(Buy ~ Treat + ROUNDstrangers + ROUNDfeedback + ROUNDpartners +
                    LAST2ROUNDstrangers + LAST2ROUNDfeedback + LAST2ROUNDpartners +
                    (1 | IDBuyer),
                  family = binomial(link = "probit"),
                  data = bolton,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 20)
## Model did not converge... Estimates are considerably close (e.g., log-lik
## and coefficients).
summary(tab1mod1)

tab1mod2 <- glmer(Buy ~ Treat + CBSHIP + CBNOSHIP + LAST2ROUNDstrangers + 
                    LAST2ROUNDfeedback + LAST2ROUNDpartners + 
                    (1 | IDBuyer),
                  family = binomial(link = "probit"),
                  data = bolton,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 20)
## Model also didn't converge... Estimates are considerably close though
summary(tab1mod2)

tab1mod3 <- glmer(Buy ~ (Treat == "Partners") + TOTALSHIPfeedback + TOTALNOSHIPfeedback +
                    SHIPLAST2feedback + NSHIPLAST2feedback + SHIPLAST2partners + NSHIPLAST2partners +
                    CBSHIP2 + CBNOSHIP2 + LAST2ROUNDstrangers + LAST2ROUNDfeedback + 
                    LAST2ROUNDpartners +
                    (1 | IDBuyer),
                  family = binomial(link = "probit"),
                  data = bolton,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 50)

summary(tab1mod3)

bolton %>% 
  mutate(SHIPLAST = case_when(Treat == "Strangers" ~ SHIPLAST2strangers,
                              Treat == "Feedback"  ~ SHIPLAST2feedback,
                              Treat == "Partners"  ~ SHIPLAST2partners)) %>% 
  group_by(Treat) %>%
  mutate(Mean_Treat = mean(Buy)) %>%
  group_by(Treat, SHIPLAST) %>%
  summarize(Buy = mean(Buy - Mean_Treat))

bolton %>%
  arrange(Sess, Buyer, Rnd) %>%
  group_by(Sess, Buyer) %>%
  mutate(LASTBUYSHIPPED = case_when(is.na(lag(Buy)) ~ 0,
                                    lag(Buy == 1) ~ ifelse(lag(Ship > 0), 1, 0)),
         SHIPLAST = case_when(Treat == "Strangers" ~ SHIPLAST2strangers,
                              Treat == "Feedback"  ~ SHIPLAST2feedback,
                              Treat == "Partners"  ~ SHIPLAST2partners)) %>%
  tidyr::fill(LASTBUYSHIPPED) %>%
  group_by(Treat) %>%
  mutate(Mean_Treat = mean(Buy)) %>%
  group_by(Treat, SHIPLAST, LASTBUYSHIPPED) %>%
  summarize(Buy = mean(Buy - Mean_Treat))

tab2mod <- glmer(Jship ~ Treat + ROUNDstrangers + ROUNDfeedback + ROUNDpartners +
                   SHIPLAST2strangers + SHIPLAST2feedback + SHIPLAST2partners +
                   LAST2ROUNDstrangers + LAST2ROUNDfeedback + LAST2ROUNDpartners +
                   (1 | IDSeller),
                 family = binomial(link = "probit"),
                 data = bolton %>% filter(Buy == 1),
                 control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 20)


tab2mod2 <- glmer(Jship ~ Treat + ROUNDstrangers + ROUNDfeedback + ROUNDpartners +
                    SHIPLASTstrangers + SHIPLASTfeedback + SHIPLASTpartners +
                    LAST2ROUNDstrangers + LAST2ROUNDfeedback + LAST2ROUNDpartners +
                    (1 | IDSeller),
                  family = binomial(link = "probit"),
                  data = bolton %>% filter(Buy == 1),
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 20)
summary(tab2mod)
summary(tab2mod2)




plm(Buy ~ Treat + ROUNDstrangers + ROUNDfeedback + ROUNDpartners + LAST2ROUNDstrangers +
      LAST2ROUNDfeedback + LAST2ROUNDpartners,
    data = bolton,
    index = c("IDBuyer"),
    model = "within") %>%
  summary()


plm(Buy ~ ifelse(Treat == "Feedback", 1, 0) + ifelse(Treat == "Partners", 1, 0) + CBSHIP + CBNOSHIP + LAST2ROUNDstrangers + LAST2ROUNDfeedback +
      LAST2ROUNDpartners,
    data = bolton,
    index = c("IDBuyer"),
    model = "within") %>%
  summary()

plm(Buy ~ Treat + TOTALSHIPfeedback + TOTALNOSHIPfeedback + SHIPLAST2feedback +
      NSHIPLAST2feedback + SHIPLAST2partners + NSHIPLAST2partners + CBSHIP2 + 
      CBNOSHIP2 + LAST2ROUNDstrangers + LAST2ROUNDfeedback + LAST2ROUNDpartners,
    data = bolton,
    index = c("IDBuyer"),
    model = "within") %>% 
  summary


tabB2mod <- glmer(Buy ~ 
                    (Treat == "Partners") + 
                    I(TOTALSHIPfeedback * (Treat == "Feedback")) + 
                    I(TOTALNOSHIPfeedback * (Treat == "Feedback")) +
                    I(SHIPLAST2feedback * (Treat == "Feedback")) + 
                    I(NSHIPLAST2feedback * (Treat == "Feedback")) + 
                    I(SHIPLAST2partners * (Treat == "Partners")) + 
                    I(NSHIPLAST2partners * (Treat == "Partners")) +
                    I(CBSHIP * (Treat == "Strangers")) +
                    I(CBSHIP * (Treat == "Feedback")) +
                    I(CBSHIP * (Treat == "Partners")) + 
                    I(CBNOSHIP * (Treat == "Strangers")) +
                    I(CBNOSHIP * (Treat == "Feedback")) +
                    I(CBNOSHIP * (Treat == "Partners")) + 
                    LAST2ROUNDstrangers + LAST2ROUNDfeedback + 
                    LAST2ROUNDpartners +
                    (1 | IDBuyer),
                  family = binomial(link = "probit"),
                  data = bolton,
                  nAGQ = 100) %>% summary


## Correct replication was not entirely possible based on the information in the
## article and potential differences between software programs. Nevertheless, most
## coefficients estimated in the replications were highly similar to the coefficients
## reported. 

## Yet, there is probably a typo in Table B1, effect of ROUNDstrangers in model 1,
## where I suspect the authors omitted a 0 between -0. and 184.


# ---------------------- ANALYSES VOLKER, BUSKENS, RAUB ------------------------



bolton_tf <- bolton %>% # trustfulness data
  filter(Rnd == 1,
         Treat %in% c("Strangers", "Feedback"))

tf_bolton_prop <- bolton_tf %>% # proportion trustfulness per network condition
  group_by(Treat) %>%
  summarize(tf = mean(Buy))

tf_bolton_glm <- glm(Buy ~ Treat, # fit logistic regression model
                     family = binomial,
                     bolton_tf)
summary(tf_bolton_glm)

tf_bolton_bf <- bain(coef(tf_bolton_glm), # get bayes factors
                     hypothesis = "TreatFeedback > 0",
                     Sigma = vcov(tf_bolton_glm),
                     n = length(tf_bolton_glm$y))

bolton_tw <- bolton %>% # trustworthiness data
  filter(Buy == 1,
         Rnd == 1,
         Treat %in% c("Strangers", "Feedback")) %>%
  mutate(Ship = ifelse(Ship > 0, 1, 0))

tw_bolton_prop <- bolton_tw %>% # proportion trustworthiness per network condition
  group_by(Treat) %>%
  summarize(tw = mean(Ship))

tw_bolton_glm <- glm(Ship ~ Treat, # logistic regression model trustworthiness
                     family = binomial,
                     bolton_tw)

summary(tw_bolton_glm)

tw_bolton_bf <- bain(coef(tw_bolton_glm), # bayes factors trustworthiness
                     hypothesis = "TreatFeedback > 0",
                     Sigma = vcov(tw_bolton_glm),
                     n = length(tw_bolton_glm$y))

# ------------------------------- SAMPLE SIZE ----------------------------------

nrow(bolton_tf) + nrow(bolton_tw)

# ------------------------------ SAVE RESULTS ----------------------------------

save.image("R/results/bolton.RData")

