
library(dplyr)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>%
  setNames(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>%
  mutate(year = 1959:1997)
co2_tidy <- co2_wide %>% gather(month, co2, -year, convert = TRUE)

co2_wide
co2_tidy

yearly_avg<- co2_tidy %>%  
  group_by(year) %>%
  summarize(yearly_avg = mean(co2))

yearly_avg


co2_join<-left_join(co2_wide, yearly_avg, by = "year")

head(co2_join)

left_join(co2_tidy, yearly_avg, by = "year") %>%
  mutate(residual = co2 - yearly_avg) %>%
  ggplot(aes(year, residual, color = month)) +
  geom_point() +
  geom_line()
