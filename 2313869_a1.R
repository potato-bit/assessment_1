library('tidyverse')
library('emmeans')
library('afex')
library('cowplot')

d1 <- read_csv('phones.csv')
head(d1)
d1 <- d1 %>% mutate(ranklow = ifelse(rank=='low',1,0),
              rankmedium = ifelse(rank=='medium',1,0),
              rankhigh = ifelse(rank=='high',1,0),
              first = ifelse(valuer=='first',1,0),
              absolute = ifelse(type=='absolute',1,0))
m1 <- lm(wtp ~ ranklow+rankhigh+first+absolute,d1)
summary(m1)

a1 <- aov_car(wtp ~ rank*type + valuer + Error(id), d1)
summary(a1)
