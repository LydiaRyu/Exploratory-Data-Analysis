
library(Lahman)
library(dplyr)


dat <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB+BB,
         singles = (H-X2B-X3B-HR )/pa, bb= BB/pa) %>%
  filter(pa>=100) %>%
  select(playerID, singles, bb)

head(dat,10)


avg <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB+BB,
         singles = (H-X2B-X3B-HR )/pa, bb= BB/pa) %>%
  filter(pa>=100) %>%
  select(playerID, singles, bb) %>%
  group_by(playerID) %>%
  summarize(avg_singles = mean(singles), avg_bb = mean(bb) )
  

head(avg,10)

dat2 <- inner_join(dat, avg, by="playerID")

head(dat2,10)

cors <- dat2 %>%
  summarize(cor_singles = cor(singles, avg_singles), 
                          cor_bb = cor(bb, avg_bb) ) 
  

cors

----------------------------------------------------------------------------------

# Linear Regression with 'GaltonFamilies' dataset

library(HistData)
library(tidyverse)
library(broom)
library(tidyr)


data("GaltonFamilies")

head(GaltonFamilies,10)

set.seed(1)
galton_heights <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup()

head(galton_heights,10)


cors <- galton_heights %>%
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender=="female", "daughter", "son")) %>%
  unite(pair, c("parent","child")) %>%
  group_by(pair) %>% 
  summarize(cor = cor(parentHeight, childHeight))

cors


father_daughter <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(daughter = childHeight)

head(father_daughter,10)


father_son <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

head(father_son,10)


mother_daughter <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)


head(mother_daughter,10)


mother_son <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(son = childHeight)

head(mother_son,10)


fit1 <-lm(daughter ~ father, data = father_daughter)
fit2 <-lm(son ~ father, data = father_son)
fit3 <-lm(daughter ~ mother, data = mother_daughter)
fit4 <-lm(son ~ mother, data = mother_son)
tidy(fit1)
tidy(fit2)
tidy(fit3)
tidy(fit4)


tidy(fit1, conf.int=TRUE)
tidy(fit2, conf.int=TRUE)
tidy(fit3, conf.int=TRUE)
tidy(fit4, conf.int=TRUE)


co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>%
  setNames(1:12) %>% 
  mutate(year = as.character(1959:1997))

head(co2_wide,10)


co2_tidy <- co2_wide %>%
  gather(month, co2, 1:12)

head(co2_tidy,10)

  
  
  