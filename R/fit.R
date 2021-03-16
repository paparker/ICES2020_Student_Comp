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

sbo <- mutate(sbo, SEX=as.factor(SEX)) %>% 
	mutate(ETH=as.factor(ETH)) %>%
	mutate(RACE=as.factor(RACE)) %>%
	mutate(VET=as.factor(VET)) %>%
	mutate(SECTOR=as.factor(SECTOR)) %>%
	mutate(FRANCHISE=as.factor(FRANCHISE)) 

#PRMINC response with SEX, ETH, RACE, VET, SECTOR, FRANCHISE as covariates
Xmat <- model.matrix(formula(~ (RECEIPTS_NOISY + EMPLOYMENT_NOISY + SEX +
				ETH+RACE+VET+
				SECTOR+FRANCHISE)-1), 
		     data=sbo)

#extracting factor levels per https://stackoverflow.com/a/44008147
factor_levels <- lapply(sbo[,sapply(sbo, is.factor), drop = F],
		       	function(j){levels(j)}
			)

nh <- 100 ## number of hidden nodes
A <- matrix(rnorm(nh*(ncol(Xmat)+1)), ncol=nh) ## random weights
Xnew <- plogis(cbind(1,Xmat)%*%A) ## transformed input data


mod <- pgVB(X=Xnew, Y=sbo$PRMINC, eps=0.1, wgt=sbo$TABWGT) ## fit model using VB

beta <- rmvnorm(1000, mean=mod$BU$mean, sigma=as.matrix(mod$BU$Cov)) ## posterior samples

### Get predictions
Xpred <- plogis(cbind(1,Xmat)%*%A)
preds <- plogis(Xpred%*%t(beta)) ### Need to save A and beta for prediction in shiny app

write_rds(list(A=A, beta=beta, factor_levels=factor_levels), file = "Data/elm.Rds")
