library(readr)
library(dplyr)
library(tidyr)
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

#PRMINC response with SEX, ETH, RACE, VET, SECTOR, FRANCHISE as covariates
Xmat <- model.matrix(formula(~ (log(RECEIPTS_NOISY+1) + log(EMPLOYMENT_NOISY+1) + SEX +
				RACE_ETH+VET+
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
#preds <- plogis(Xpred%*%t(beta)) ### Need to save A and beta for prediction in shiny app
preds <- rep(NA, nrow(Xpred))
pb <- txtProgressBar(min = 0, max = nrow(Xpred), style = 3)
for(i in 1:nrow(Xpred)){
  preds[i] <- mean(Xpred[1,]%*%t(beta))
  setTxtProgressBar(pb, i)
}

write_rds(list(A=A, beta=beta, factor_levels=factor_levels), path = "Data/elm.rds")

wll1 <- sum(-sbo$TABWGT*(sbo$PRMINC*log((preds)) + (1-sbo$PRMINC)*(1-(preds))))

## Compare to linear model
mod2 <- pgVB(X=Xmat, Y=sbo$PRMINC, eps=0.1, wgt=sbo$TABWGT) ## fit model using VB
beta2 <- rmvnorm(1000, mean=mod2$BU$mean, sigma=as.matrix(mod2$BU$Cov)) ## posterior samples

### Get predictions
#preds2 <- plogis(Xmat%*%t(beta2)) ### Need to save A and beta for prediction in shiny app
preds2 <- rep(NA, nrow(Xmat))
pb <- txtProgressBar(min = 0, max = nrow(Xmat), style = 3)
for(i in 1:nrow(Xmat)){
  preds2[i] <- mean(Xmat[1,]%*%t(beta2))
  setTxtProgressBar(pb, i)
}
wll2 <- sum(-sbo$TABWGT*(sbo$PRMINC*log((preds2)) + (1-sbo$PRMINC)*(1-(preds2))))
