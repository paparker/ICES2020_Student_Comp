library(BayesLogit)
library(Matrix)
library(mvtnorm)

pgVB <- function(X, Y, Au=1, Bu=1, eps=0.01, wgt=NULL){
  n <- length(Y)
  if(is.null(wgt)) wgt <- rep(1, n)
  p <- ncol(X)
  Bsig2u <- 1
  MUbu <- rep(1,p) #glm.fit(C, cbind(Y,1-Y), family=binomial(link='logit'))$coefficients
  SIGbu <- Diagonal(p)
  checkOld <- Inf
  checkVec <- c()
  iter <- 1
  repeat{
    ## Latent Variables
    xi <- as.numeric(sqrt(colSums(t(X)*(SIGbu%*%t(X))) + (X%*%MUbu)^2))
    
    ## Regression coefficients
    Zbar <- Diagonal(n,(wgt*0.5/xi)*tanh(0.5*xi))
    SIGbu <- solve(as.numeric((Au+p/2)/(Bsig2u))*Diagonal(p) +
                     t(X)%*%Zbar%*%X)
    MUbu <-  SIGbu %*% t(X) %*% as.numeric(wgt*(Y - 0.5))
    
    
    
    ## RE Variance
    Bsig2u <- Bu + 0.5*(t(MUbu)%*%(MUbu) + sum(diag(SIGbu)))
    
    PPrec <- as.numeric((Au+p/2)/(Bsig2u))*Diagonal(p)
    
    ## Check for convergence
    #checkNew <- log(det(SIGbu)) - (t(MUbu[c(1:p)])%*%(MUbu[c(1:p)]) + sum(diag(SIGbu[c(1:p),c(1:p)]))) - log(Bsig2u)
    checkNew  <- 0.5*(p) + 0.5*determinant(SIGbu, logarithm = T)$modulus  - 
      0.5*t(MUbu)%*%PPrec%*%(MUbu) + sum(wgt*(Y-0.5)*as.numeric(X%*%MUbu) + wgt*log(plogis(xi)) - 0.5*wgt*xi) - 
      0.5*sum(diag(PPrec %*% SIGbu)) - log(Bsig2u)
    checkVec <- c(checkVec, as.numeric(checkNew))
    
    
    if (as.logical(abs(checkOld - checkNew) < eps)) break
    iter <- iter + 1
    checkOld <- checkNew
    print(paste0("Iteration: ",iter, " ELBO: ", checkNew))
  }
  return(list(iter=iter, BU=list(type="Normal", mean=MUbu, Cov=SIGbu, p=p),
              Bsig2u=list(type="Inv. Gamma", A=Au + p/2, B=Bsig2u), Elbo=checkVec))
}

PGELMmcmc <- function(X, Y, nh=20, iter=1000, burn=500, wgt=NULL){
  ## This function fits the PG-ELM model via Gibbs sampling
  # X is the input covariate matrix, one row per sample unit
  # Y is a vector of binary survey responses
  # nh is the number of hidden nodes
  # iter is the number of iterations
  # burn is the length of burn in
  # wgt is the vector of survey weights

  if(is.null(wgt)) wgt <- rep(1,n)
  Xs <- scale(X)
  centers <- attr(Xs, "scaled:center")
  scales <- attr(Xs, "scaled:scale")
  X <- cbind(1,Xs)
  p <- ncol(X)
  n <- length(Y)

  
  A <- matrix(rnorm(nh*p), ncol=nh)
  Xnew <- plogis(X%*%A)
  p <- ncol(Xnew)
  kappa <- wgt*(Y - 0.5)
  w <- rep(1, n)
  beta <- rep(0, p)
  sig2E <- 1
  betaOut <- matrix(NA, nrow=iter, ncol=p)
  sig2Eout <- rep(NA, iter)
  print(paste0("Starting ",iter, " iterations."))
  pb <- txtProgressBar(min=0, max=iter, style=3)
  for(i in 1:iter){
    Einv <- (1/sig2E)*Diagonal(p)
    
    ## Sample fixed effects
    precBeta <- solve(t(Xnew)%*%Diagonal(length(w),w)%*%Xnew + Einv)
    meanBeta <- t(Xnew)%*%Diagonal(length(w),w)%*%(kappa/w)
    beta <- betaOut[i,]  <- as.numeric(rmvnorm(1, mean=precBeta%*%meanBeta, sigma=as.matrix(precBeta)))
    
    
    
    ## Sample ridge penalty
    sig2E <- sig2Eout[i] <- 1/rgamma(1,
                                     shape=1 + p/2,
                                     (1 + 0.5*t(beta)%*%(beta)))
    
    ## Sample latent PG variables
    w <- rpg(n, wgt, as.numeric(Xnew%*%beta))
    setTxtProgressBar(pb, i)
  }
  return(list(Beta=betaOut[-c(1:burn),], sig2E=sig2Eout[-c(1:burn)], ELM=A, Centers=centers, Scales=scales))
}
