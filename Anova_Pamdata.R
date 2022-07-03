library(rstatix)
library(tidyverse)
rawpam <- read.csv("Raw Pam.txt",sep = "\t",header = TRUE)
pam <- read.csv("Pamdata.txt",sep = "\t",header = TRUE)

pam %>% select(Sample.Names) %>% summarise(N = n())
pam %>% select(ID,treatment = Groups,sample = Sample.Names,Reads)-> pam1
## one way annova
pamavo <- aov(Reads ~ sample,data = pam1)
summary(pamavo)
TukeyHSD(pamavo)
##two way annova##
pamtwoaov <- aov(Reads ~ sample + treatment, data = pam1)
summary(pamtwoaov)
TukeyHSD(pamtwoaov)
## one way annova to treatment
pamavo <- aov(Reads ~ treatment,data = pam1)
summary(pamavo)
TukeyHSD(pamavo)


#####

rawpam %>% 
  pivot_longer(cols = c(4:24),
               names_to = "sample",
               values_to = "reads")-> rawpam1
rawpam1 %>% mutate(treatment = case_when(sample == N ~ "control"))->nnn
#####

nm2 %>% 
  select(sample) %>% 
  mutate(group = str_replace(sample, pattern = "\\w\\d*",
                             replacement = "")) %>% select(group) %>% 
  mutate(nm2,group= recode(.x= group, "N" = "Control", "T"= "Treatment"))->nm3

## add group column based on sample names, N= control, T = Treatment## 
rawpam1 %>% select(sample) %>% 
  mutate(group = str_replace(sample, pattern = "\\w\\d*",
                             replacement = "")) %>% select(group) %>% 
  mutate(rawpam1,group= recode(.x= group,"N" = "Control", "T" = "Treatment"))->rawpam2

########
remove(x)
