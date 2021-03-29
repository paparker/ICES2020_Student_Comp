library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
source('R/funcs.R')

sbo <- read_rds('Data/tidy_sbo.rds')


sbo <- sbo %>% select(PRMINC, RECEIPTS_NOISY, EMPLOYMENT_NOISY, SEX,
                      RACE,ETH,VET,SECTOR,FRANCHISE,TABWGT) %>% 
  na.omit()

sbo <- sbo %>% mutate(TABWGT=TABWGT*n()/sum(TABWGT)) %>%
  mutate(PRMINC=abs(PRMINC-2))

sbo <- mutate(sbo, SEX=as.factor(SEX)) %>% 
  mutate(VET=as.factor(VET)) %>%
  mutate(SECTOR=as.factor(SECTOR)) %>%
  mutate(FRANCHISE=as.factor(FRANCHISE)) 

## Combine Race and Eth into one factor
sbo <- sbo %>% 
  mutate(RACE_ETH=paste(RACE," (",ETH,")",sep=""))

sbo <- sbo %>%
  group_by(RACE_ETH) %>% 
  add_count(name="count") %>%
  mutate(RACE_ETH=factor(ifelse(count < 250, "Other", RACE_ETH))) %>%
  select(-RACE,-ETH)

sbo$RACE_ETH<-as.factor(sbo$RACE_ETH)

heatDF <- sbo %>% group_by(SECTOR, RACE_ETH) %>% summarize(`Direct Est.`=sum(PRMINC*TABWGT)/sum(TABWGT), Count=n())

ggplot(heatDF)+
  geom_tile(aes(x=SECTOR, y=RACE_ETH, fill=`Direct Est.`))+
  scale_fill_viridis_c()+
  theme_classic()+
  scale_x_discrete(label=abbreviate)+
  theme(axis.text.x = element_text(size = 12, angle = 90))
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

