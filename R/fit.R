library(readr)
library(dplyr)
library(tidyr)
source('R/funcs.R')

sbo <- read_rds('Data/tidy_sbo.rds')


sbo <- sbo %>% select(PRMINC, RECEIPTS_NOISY, EMPLOYMENT_NOISY, SEX,
              ETH,RACE,VET,SECTOR,FRANCHISE,TABWGT) %>% 
  na.omit()

sbo <- sbo %>% mutate(TABWGT=TABWGT*n()/sum(TABWGT)) %>%
  mutate(PRMINC=abs(PRMINC-2))

#PRMINC response with SEX, ETH, RACE, VET, SECTOR, FRANCHISE as covariates
Xmat <- model.matrix(formula(~ (RECEIPTS_NOISY + EMPLOYMENT_NOISY + factor(SEX)+factor(ETH)+factor(RACE)+factor(VET)+factor(SECTOR)+factor(FRANCHISE))-1), data=sbo)
nh <- 100 ## number of hidden nodes
A <- matrix(rnorm(nh*(ncol(Xmat)+1)), ncol=nh) ## random weights
Xnew <- plogis(cbind(1,Xmat)%*%A) ## transformed input data


mod <- pgVB(X=Xnew, Y=sbo$PRMINC, eps=0.1, wgt=sbo$TABWGT) ## fit model using VB

beta <- rmvnorm(1000, mean=mod$BU$mean, sigma=as.matrix(mod$BU$Cov)) ## posterior samples

### Get predictions
Xpred <- plogis(cbind(1,Xmat)%*%A)
preds <- plogis(Xpred%*%t(beta)) ### Need to save A and beta for prediction in shiny app

