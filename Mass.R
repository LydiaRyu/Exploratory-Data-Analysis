#install.packages("MASS")
library(MASS)
head(as_tibble(Cars93))


Cars93 %>% ggplot() + 
  geom_point(aes(x= MPG.highway, y=Price, size=Horsepower))

Cars93 %>% ggplot() + 
  geom_point(aes(x= MPG.highway, y=Price, size=Horsepower, col=Origin))

Cars93 %>% ggplot(aes(x= MPG.highway, y=Price, size=Horsepower, col=Origin)) + 
  stat_smooth()     


Cars93 %>% ggplot() + 
  geom_point(aes(x= MPG.highway, y=Price, size=Horsepower, col=Origin)) +
  facet_grid(. ~ Origin)

Cars93 %>% ggplot(aes(x= MPG.highway, y=Price, size=Horsepower, col=Origin)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  facet_grid(. ~ Origin)
