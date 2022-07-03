library(tidyverse)
library(patchwork)
library(gapminder)
library(forcats)
library(ggpubr)
data("gapminder")
view(gapminder)
colnames(gapmind)
#### single sample t-test###
ttest <- gapmind %>% 
  filter(continent == "Europe") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50)
ttest§p.value

attributes(ttest)
ttest$p.value
ttest$statistic
#### Two sided sample t-test for difference###
gapmind %>% 
  filter(continent %in% c("Africa","Europe")) %>% 
  t.test(lifeExp~ continent,data = .,
         alternative = "two.side")
#### One sided sample t-test for difference###
gapmind %>% 
  filter(country %in% c("Ireland","Switzerland")) %>% 
  t.test(lifeExp ~ country, data = .,
         aternative = "less", conf.level = 0.95)

####### Paired ttest####
gapmind %>% 
  filter(year %in% c(1957,2007) &
  continent == "Africa") %>% 
  mutate(year = factor(year, levels = c(2007, 1957))) %>% 
  t.test(lifeExp ~ year, data = ., paired = TRUE)
  t.test()
  
  ########## Analysis of varience ######## Annova##
  taov <- gapmind %>% 
    filter(year == 2007 & continent %in% c("Americas", "Europe","Asia")) %>% 
    select(continent,lifeExp)
  view(taov)
  
  taov %>% 
    group_by(continent) %>% 
    summarise(Mean_life = mean(lifeExp)) %>% 
    arrange(Mean_life)
  
  taov %>% 
    aov(lifeExp ~ continent,data = .) %>% 
    summary()
  aov_model <- taov %>% 
    aov(lifeExp ~ continent,data = .)
  
  
  taov %>% 
    aov(lifeExp ~ continent,data = .) %>% 
    TukeyHSD() 
  
  
  taov %>% 
    aov(lifeExp ~ continent,data = .) %>% 
    TukeyHSD() %>% 
    plot()
  ##### ND##
  boxplot(taov$lifeExp)
ggqqplot(taov$lifeExp)
shapiro.test(taov$lifeExp)

SS <- rnorm(150)
ks.test(SS)
?ks.test
shapiro.test(gapminder$pop)
plot(density(gapmind$pop))
getwd()
studentdata <- read.csv("How-To-R-master/82_Data_File.csv",header = TRUE, sep = ",")
view(studentdata)
attach(studentdata)
colnames(studentdata)
attach(LungCapData2)
Lung <- t.test(Height ~ Smoke, mu = 0, alt = "two.sided", conf=0.95, ver.eq = F, paired = F)
Lung
attributes(Lung)

