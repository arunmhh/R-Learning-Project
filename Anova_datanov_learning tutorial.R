library(datarium)
library(tidyverse)
library(rstatix)
library(datarium)
deep <- depression
## convert data matrix from wider to longer##
deep %>% 
  pivot_longer(cols = c("t0","t1","t2","t3"),
               names_to = "period",
                      values_to = "time")-> deep1
deep2 <- deep1 ## duplicate data
deep2$period <- as.factor(deep2$period) # convert as factor
deep2
levels(deep2$period) ## factor level in period
## one way Anova##
oneavo <- aov(time ~ period,data = deep2)
summary(oneavo)
TukeyHSD(oneavo)

##boxplot##
ggplot(deep2, aes(x = period, y = time, fill = period))+
  geom_boxplot()+
  geom_jitter(shape = 20,
              colour = "red")+
  theme_classic()

## two way Anova##
twoaov <- aov(time ~ period + treatment, data = deep2)
summary(twoaov)
TukeyHSD(twoaov)


?lm
harzinf <- heartattack
library(datarium)
## three way Anova##
threewayaov3 <- aov(cholesterol~ (gender+risk+drug)^2, data = harzinf)
sapply(harzinf,class)
colnames(harzinf)
summary(threewayaov3)
harzinf %>% 
  ggplot(aes(x =drug, y = cholesterol, colours = drug))+
  stat_boxplot(geom = "errorbar",width = 0.5)+
  geom_boxplot(fill = c("pink","brown","red"),outlier.size = 2,outlier.shape = 8)
##############
plant <- PlantGrowth
levels(plant$group)
boxplot(plant)
plant %>% select(group)->plant1
plant %>%
  ggplot(aes(x = group, y = weight, colour = group))+
  stat_boxplot(geom = "errorbar", width = 0.3)+
  geom_boxplot(fill= c("red","green","blue"), outlier.shape = 8, outlier.size = 2)+
  labs(title = "Weight Distribution")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 15))+ theme_classic()
               
#############################
threewayaov4 <- aov(cholesterol~ gender*risk*drug, data = harzinf)
sapply(harzinf,class)
colnames(harzinf)
summary(threewayaov4)
  
remove(ship)

