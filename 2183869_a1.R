library('tidyverse')
library('emmeans')
library('afex')
library('cowplot')


# PART 1
##data exploration
d1 <- read_csv('phones.csv')
head(d1)
d1 <- d1 %>% mutate(valuer=factor(valuer, levels=c('first','replacement')),
                    rank=factor(rank, levels=c('low','medium','high')),
                    type=factor(type, levels=c('absolute','relative')))
nrow(d1)
d1 %>% ggplot(aes(x=wtp)) + geom_histogram()
d1 %>% group_by(valuer) %>% summarise(n=n()) 
d1 %>% group_by(rank) %>% summarise(n=n())
d1 %>% group_by(type) %>% summarise(n=n())
d1 %>% group_by(rank,type) %>% summarise(n=n())
d1 %>% group_by(valuer,rank) %>%  summarise(n=n())
d1 %>% group_by(valuer,type) %>%  summarise(n=n())

##regression analysis

#### setting treatment contrasts for now
afex::set_treatment_contrasts()

### hypothesis: there exist interaction effects between type of rank and rank
### value in eliciting WTP. Further, the type of valuer may also affect WTP if 
### new phone is compared to old one (more interaction effects?)
l1 <- lm(wtp ~ type*rank + valuer*rank, d1)
summary(l1)
### the statistically significant effects are `rankhigh` the high ranked devices
### elicit 34 points higher WTP; and the interaction effect between high ranked 
### devices and valuers who are looking for a replacement device i.e, those
### looking for a replacement valued high ranked devices 46.6 points higher than
### first-time buyers implying that replacement buyers would accept higher prices.
### The statistically insignificant values follow the expected trends, except 
### that replacement buyers have 12 points lower of WTP than first time buyers 
### on average.

##visualization
##[insert animated graph showing relationships between variables]

##ANOVA testing
#### setting sum contrasts
afex::set_sum_contrasts()

a1 <- aov_car(wtp ~ type*rank + valuer*rank + Error(id), d1)
summary(a1)
### the ANOVA test shows us there is a significant difference between the rank 
### groups and the interaction affect between the type of valuer and rank; and 
### there is no statistically significant difference between the other groups

##visualization
p1 <- afex_plot(a1,'type','rank')
p2 <- afex_plot(a1,'rank','type')
pg1 <- plot_grid(p1,p2)
p3 <- afex_plot(a1,'valuer','rank')
p4 <- afex_plot(a1,'rank','valuer')
pg2 <- plot_grid(p3,p4)
pg1
pg2
### As the regression analysis showed, there is only a significant difference in
### in means between replacement valuers and high rank buyers. Otherwise, the 
### main determining factor when buying a new phone is the rank itself 
### regardless of type or valuer.

##follow-up tests
e1 <- emmeans(a1,c('type','rank'))
summary(e1)
### the highest mean comes from the high relative rank, but not that there is 
### not a large difference (not more than 10) between absolute and relative 
### ranks for each rank.

### test contrasts
con1 <- list(A_v_R = c(1/3,-1/3,1/3,-1/3,1/3,-1/3),
             L_v_E = c(1/2,1/2,rep(-1/4,4)),
             M_v_E = c(-1/4,-1/4,1/2,1/2,-1/4,-1/4),
             H_v_E = c(rep(-1/4,4),1/2,1/2))
contrast(e1,con1,adjust='holm')
### as expected only rank effects are statistically significant

e2 <- emmeans(a1,c('valuer','rank'))
summary(e2)
### like the ANOVA test suggests, only the replacement:high pair has a
### significant difference in means. Interestingly for low ranked devices, 
### replacement buyers value them less than first-time buyers. This suggests a 
### greater spread in WTP for replacement buyers than first-time buyers.
### [insert graph showing this]

### test contrasts
con2 <- list(F_v_R = c(1/3,-1/3,1/3,-1/3,1/3,-1/3),
             L_v_E = c(1/2,1/2,rep(-1/4,4)),
             M_v_E = c(-1/4,-1/4,1/2,1/2,-1/4,-1/4),
             H_v_E = c(rep(-1/4,4),1/2,1/2),
             Rh_v_Fh = c(rep(0,4),-1/2,1/2),
             Rh_v_E = c(rep(-1/5,5),1),
             Fl_v_Rl = c(1/2,-1/2,rep(0,4)))
contrast(e2,con2,adjust='holm')
### from the ANOVA visualization earlier, it looked like there might be a 
### difference between low ranked devices and type of valuers, however the 
### contrasts test shows us this is not statistically  significant.The contrasts
### test confirms that replacement buyers buying high rank devices report the 
### highest WTPs, 60.8 points greater than everybody else on average.
             


# PART 2
##data exploration
d2 <- read_csv('ride.csv')
head(d2)
d2 <- d2 %>% mutate(Task=factor(Task, levels=c('Thrill','WTP')),
                    Order=factor(Order, levels=c('Self First','Other First')))
d2 %>% filter(is.na(Task))
### there are missing values under Task, for Pid=(4,14,122,190,246) upon closer 
### examination, this is due to a misspelling of the word "Thrill" to "Thill"

### editing misspelled values
for (x in c(4,14,122,190,246)) {
  d2[d2$Pid==x,'Task'] <- as_factor('Thrill')
}
d2 %>% filter(is.na(Task))
### renaming columns because capitalization is annoying
d2 <- rename_with(d2, tolower)

nrow(d2)
d2 %>% group_by(task) %>% summarise(n=n())
d2 %>% group_by(order) %>% summarise(n=n())
d2 %>% group_by(task,order) %>% summarise(n=n())
d2 %>% ggplot(aes(x=self)) + geom_histogram()
d2 %>% ggplot(aes(x=other)) + geom_histogram()
d2 %>% ggplot(aes(x=self,y=other)) + geom_point(position='jitter') + geom_smooth(method='lm',se=F)
d2 %>% ggplot(aes(x=self,y=other,color=task)) + geom_point(position='jitter') 

##regression analysis
#### setting treatment contrasts for now
afex::set_treatment_contrasts()

d2 %>% select(self,other) %>% GGally::ggscatmat()
### note the bimodal distribution of both `self` and `other` ratings, 
### participants tend to avoid giving median scores for either
l2 <- lm(other ~ self,d2)
summary(l2)
l3 <- lm(other ~ task*self + order,d2)
summary(l3)
l4 <- lm(other ~ self*order + task*self,d2)
summary(l4)
### There is almost a 1:1 relationship between how much a participant rates 
### their own experience and how they expect others to enjoy the same experience.
### Further, those who rate other's experience first rate it 5 points higher than
### when they rate themselves' first on average. Further, there seems to be
### statistically significant interaction effects between `taskWTP` and `self`.

##follow-up tests
e3 <- emtrends(l3,'task','self')
summary(e3)
### we can see that the slope of `self` decreases by 0.4 when participants are 
### in the WTP group.

e4 <- emmeans(l3,'task',self=mean(d2$self))
summary(e4)
### the WTP group also has a lower intercept than the Thrill group

##ANOVA Testing
#### set sum contrasts
afex::set_sum_contrasts()
a2 <- aov_car(other ~ task*order + Error(pid), d2)
summary(a2)
p5 <- afex_plot(a2,'task','order')
p6 <- afex_plot(a2,'order','task')
plot_grid(p5,p6)

##follow-up tests
e5 <- emmeans(a2,c('task','order'))
summary(e5)

con3 <- list(T_v_W=c(1/2,-1/2,1/2,-1/2),
             S_v_O=c(1/2,1/2,-1/2,-1/2),
             OT_v_ST=c(-1,0,1,0))
contrast(e5,con3,adjust='holm')

