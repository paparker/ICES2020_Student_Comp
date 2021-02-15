library(readr)
library(dplyr)
library(rstan)

sbo <- read_rds('Data/tidy_sbo.rds')

n <- dim(sbo)[1]
wgt_sum <- sum(sbo$TABWGT)
#rescale weights to sample size
sbo <- mutate(sbo, TABWGT=TABWGT*n/wgt_sum)
sbo <- select(sbo, PRMINC, RECEIPTS_NOISY, EMPLOYMENT_NOISY, SEX,
                   ETH,RACE,VET,SECTOR,FRANCHISE,TABWGT) %>% na.omit
sbo <- sample_frac(sbo, .01)
#PRMINC response with SEX, ETH, RACE, VET, SECTOR, FRANCHISE as covariates
Xmat <- model.matrix(formula(~ (RECEIPTS_NOISY + EMPLOYMENT_NOISY + factor(SEX)+factor(ETH)+factor(RACE)+factor(VET)+factor(SECTOR)+factor(FRANCHISE))-1), data=sbo)

n_iter <- 4000
N <- dim(Xmat)[1]
P <- dim(Xmat)[2]
Y <- abs(sbo$PRMINC-2) #map from 1=Yes, 2=No to 1=Yes, 0=No
wgt <- sbo$TABWGT


pll_dat <- list(N=N, P=P, Y=Y, wgt=wgt, X=Xmat, chains=1, iter=1)

m<-stan_model(file="R/PLL_logistic.stan")
vb(m, data=pll_dat)

pll_fit0 <- stan("R/PLL_logistic.stan", data=pll_dat, chains=1, iter=1)

pll_fit <- stan(fit=pll_fit0, data=pll_dat, cores=1, chains=1, warmup=1000,
                iter=n_iter, open_progress=FALSE)

save(pll_fit, file="pll_fit.Rdata")
