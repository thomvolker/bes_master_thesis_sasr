

setwd("~/Documents/Master_Thesis/SaSR/data/seinen_schram_2006")

dat1 <- read.table("~/Downloads/sessie1.txt",
                   col.names = c("periode", "playerA", "playerB", "keuzesA1", 
                                 "keuzesA2", "keuzesB1", "keuzesB2", "keuze"))
dat2 <- read.table("~/Downloads/sessie2.txt", 
                   col.names = c("periode", "playerA", "playerB", "keuzesA1", 
                                 "keuzesA2", "keuzesB1", "keuzesB2", "keuze"))

View(dat1)

dat1 <- readxl::read_xls("sessie1.xls", sheet = 1, skip = 10)
dat2 <- readxl::read_xls("sessie2.xls", sheet = 2)
dat3 <- readxl::read_xls("sessie3.xls", sheet = 1)
dat4 <- readxl::read_xls("sessie4.xls", sheet = 4)

dat1 <- dat1 %>%
  select(-c(`geel/groep`, `...12`))

dat2 <- dat2 %>%
  select(-c(`geel/groep`))

dat3 <- 
  dat3 %>% 
  select(-c(`geel/groep`, nr))

dat4 <- 
  dat4 %>%
  select(periode = `0`, termnrA, termnrB, groep, keuzesAA, 
         keuzesAB, keuzesBA, keuzesBB, keuze, geel)

dat <- bind_rows(dat1,
                 dat2,
                 dat3, 
                 dat4,
                 .id = "sessie")

table(dat$geel, dat$keuze)

dat %>%
  mutate(group = as.numeric(sessie)*10 + groep) %>%
  filter(periode < 91) %>%
  group_by(group) %>%
  summarize(cooperate = mean(2-keuze)) %>%
  group_by(group) %>%
  arrange(cooperate)

dat %>%
  mutate(group = as.numeric(sessie)*10 + groep) %>%
  filter(periode < 91) %>%
  group_by(group = group < 25, periode) %>%
  summarize(keuze = mean(2 - keuze, na.rm=T)) %>%
  ggplot(aes(x = periode, y = keuze, col = group)) +
  geom_line() +
  ylim(0,1)

length(table(dat$periode))

table(dat$keuze, dat$periode)

dat2 %>%
  mutate(keuze = 3 - keuze) %>%
  bind_rows(dat1,
            .id = "sessie") %>%
  mutate(groep = ifelse(playerA < 15, 1, 2),
         group = as.numeric(sessie)*10 + groep) %>%
  filter(periode < 91) %>%
  group_by(group) %>%
  summarize(cooperate = mean(keuze))
