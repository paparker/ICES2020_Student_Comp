library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
source('R/funcs.R')

sbo <- read_rds('Data/tidy_sbo.rds')


sbo <- sbo %>% select(PRMINC, RECEIPTS_NOISY, EMPLOYMENT_NOISY, SEX,
                      ETH,RACE,VET,SECTOR,FRANCHISE,TABWGT) %>% 
  na.omit()
sbo$RACE[sbo$RACE %in% c("B   S", "B  P", "I  S", "I P", "IA", "PS")] <- "Other"


sbo <- mutate(sbo, SEX=as.factor(SEX)) %>% 
  mutate(PRMINC=abs(PRMINC-2)) %>%
  mutate(ETH=as.factor(ETH)) %>%
  mutate(RACE=as.factor(RACE)) %>%
  mutate(VET=as.factor(VET)) %>%
  mutate(SECTOR=as.factor(SECTOR)) %>%
  mutate(FRANCHISE=as.factor(FRANCHISE)) 

heatDF <- sbo %>% group_by(SECTOR, RACE) %>% summarize(`Direct Est.`=sum(PRMINC*TABWGT)/sum(TABWGT), Count=n())

ggplot(heatDF)+
  geom_tile(aes(x=SECTOR, y=RACE, fill=`Direct Est.`))+
  scale_fill_viridis_c()+
  theme_classic()
ggsave('Plots/heatmap.png')

hexDF <- sbo %>% filter(RECEIPTS_NOISY < quantile(RECEIPTS_NOISY, 0.75) 
                        & EMPLOYMENT_NOISY < quantile(EMPLOYMENT_NOISY, 0.75)) %>%
  mutate(REC=round(7*RECEIPTS_NOISY/max(RECEIPTS_NOISY))/7*max(RECEIPTS_NOISY),
         EMP=round(7*EMPLOYMENT_NOISY/max(EMPLOYMENT_NOISY))/7*max(EMPLOYMENT_NOISY)) %>%
  group_by(REC,EMP) %>% summarize(Est=sum(PRMINC*TABWGT)/sum(TABWGT), Count=n())
  
ggplot(hexDF)+
  geom_tile(aes(x=REC, y=EMP, fill=Est))+
  scale_fill_viridis_c(name="Direct Est.")+
  theme_classic()+
  xlab("RECEIPTS")+
  ylab("EMPLOYMENT")
ggsave('Plots/heatmap2.png')

