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
