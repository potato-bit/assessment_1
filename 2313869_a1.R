library('tidyverse')
library('emmeans')
library('afex')
library('cowplot')


# PART 1
d1 <- read_csv('phones.csv')
head(d1)
d1 <- d1 %>% mutate(ranklow = ifelse(rank=='low',1,0),
              rankmedium = ifelse(rank=='medium',1,0),
              rankhigh = ifelse(rank=='high',1,0),
              first = ifelse(valuer=='first',1,0),
              absolute = ifelse(type=='absolute',1,0))
m1 <- lm(wtp ~ absolute*ranklow + absolute*rankhigh + first,d1)
summary(m1)

a1 <- aov_car(wtp ~ type*rank + valuer + Error(id), d1)
summary(a1)
p1 <- afex_plot(a1,'type')
p2 <- afex_plot(a1,'rank')
plot_grid(p1,p2)
### There seems to be no significant difference between the two types of ranking, and ultimately it is the ranking itself
### that has the most effect.

e1 <- emmeans(m1,'absolute')
summary(e1)



# PART 2
d2 <- read_csv('ride.csv')
head(d2)

d2 <- d2 %>% mutate(thrill=ifelse(Task=='Thrill',1,0),
              fself=ifelse(Order=='Self First',1,0))
m2 <- lm(Other ~ Self*thrill + fself, d2)
summary(m2)

e2 <- emmeans(m2,'thrill')
summary(e2)
