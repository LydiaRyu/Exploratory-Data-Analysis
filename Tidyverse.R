
#install.packages("tidyverse")
library(tidyverse)


dat<-tibble(ID=1:4,
            grp=rep(c("A","B"), each=2),
            sex=rep(c("F","M"),2),
            meanL=c(0.22,0.47,0.33,0.55),
            sdL=c(0.11,0.33,0.11,0.31),
            meanR=c(0.34,0.57,0.40,0.65),
            sdR=c(0.08,0.33,0.07,0.27)
)
dat

dat_F<-dat %>%
  filter(sex =="F") %>%
  gather(key, value, meanL, sdL, meanR, sdR)  %>%
  unite(col = col_name, sex, key, sep = "." ) %>%
  spread(col_name, value) %>%
  select(-ID)

dat_M<-dat %>%
  filter(sex =="M") %>%
  gather(key, value, meanL, sdL, meanR, sdR)  %>%
  unite(col = col_name, sex, key, sep = "." ) %>%
  spread(col_name, value) %>%
  select(-ID)

dat_join<-left_join(dat_F, dat_M, by = "grp") %>%
  select(-grp) 

cbind(ID=1:2, dat_join)
