#############################################################################
#Function for modeling method################################################
##Input: 
###train: Training set [x,s], x does not include intercept column
###iter: Maximum number of major (outer) iterations, iter=1000
###tol: Relative tolerance on feasibility and optimality, tol=1e-8
###v: Decision threshold for estimating accuracy measures, v=c(0.2,0.3,0.4,0.5)
###vv: Intervals for estimating case numbers, vv=seq(0,1,0.1)
#############################################################################
library(Rsolnp)

LZ_c_solnp_r <- function(train, iter, tol, v, vv){

  ########################################
  #Estimate parameters#############
  ########################################
  
  ##add a column of 1 to training set
  ntrain=data.frame(rep(1,nrow(train)),train)
  tx=as.matrix(ntrain[,-ncol(ntrain)])
  nn=ncol(ntrain)-1
  
  ##generate initial value for parameter estimation
  m <- glm(train$s ~ .,data=train,family=binomial())
  betahat <- m$coefficients
  chat <- sum(train$s)/length(train$s)
  chat1=log(chat/(1-chat))
  par0 <- c(betahat, chat1)
 
 ##likelihood function
  lik1 <- function(par) {
    beta <- par[1:nn]
    lc<- par[nn+1]
    sum1 <- -sum(train$s*log(plogis(tx %*% beta)*plogis(lc))+(1-train$s)*log(1-plogis(lc)*plogis(tx %*% beta)))
    sum1
  }
  
  ctrl <- list(outer.iter=iter,tol=tol,trace=0)
  
  ##estimate parameters by optimizing likelihood function
  fit <-solnp(par0, fun=lik1, control=ctrl)
 
  theta=fit$pars[1:(ncol(ntrain)-1)]
  cc=plogis(fit$pars[ncol(ntrain)])
  
  if (fit$convergence==0) {conv=1} else {conv=0} # conv=1 if converged
  
  hess <- fit$hessian
    cov=solve(hess)
    se=sqrt(diag(cov)[1:nn])
    delta=cc*(1-cc)
    cse=sqrt(diag(cov)[(nn+1)])*delta #se for c
    ##calculate z value and p value to assess significance of each covariate
    z=theta/se
    p=2*(1-pnorm(abs(z)))
    bcov=cov[1:length(theta),1:length(theta)]

  ##estimate phenotype prevalence, q 
  train$phat=plogis(tx %*% theta)
  qhat=mean(train$phat)
  
  ##compute ase of q
  betay=theta
  aa=as.vector(plogis(tx %*% betay)*(1-plogis(tx %*% betay)))
  gg=matrix(0,nrow=nrow(tx),ncol=ncol(tx))
  for (i in 1:nrow(tx)){
    gg[i,]=aa[i]*tx[i,]
  }
  gbeta=apply(gg,2,mean)
  gbeta=as.matrix(gbeta,nrow=length(gbeta))
  qcov_b=t(gbeta)%*%bcov%*%gbeta
  
  fvec=plogis(tx %*% betay)-mean(plogis(tx %*% betay))
  qcov_f=var(fvec)/length(fvec)

  qcov=qcov_b+qcov_f
  qse=sqrt(qcov)
  
  ########################################
  #Estimate accuracy measures#############
  ########################################
  
  #calculate pp=p(y=1|s=0)
  q1=qhat
  cc1=cc
  pp=(q1-mean(train$s))/(1-mean(train$s))
  
  tpr=tpr_b(train$s,train$phat, v,pp)
  fpr=fpr_b(train$s,train$phat, v,pp)
  ppv=ppv_b(train$s,train$phat, v,pp)
  npv=npv_b(train$s,train$phat, v,pp)
  auc1=auc_self(fpr_b(train$s, train$phat, cutoff,pp),tpr_b(train$s, train$phat, cutoff,pp))
  auc2=rep(auc1,length(v))
  accu=data.frame(cbind(v,tpr,ppv,fpr,npv,auc2))
  
  ########################################
  #Model calibration######################
  ########################################
  paracase0=para_ab(train$s, train$phat, vv, pp,cc1)
  nonparacase0=nonpara_ab(train$s, train$phat, vv, pp,cc1)
  cases=data.frame(cbind(vv,paracase0,nonparacase0))
  
  return(list(theta=theta,se=se,z=z,p=p,conv=conv,accu=accu,
              cc=cc,cse=cse,qhat=qhat,qse=qse,train=train,cases=cases))
}


################################################
#Sourced functions##############################
################################################

#Function of nonparametrically estimating number of cases
nonpara_ab<-function(s, predy, v, pp,cc1){
  yhat <- predy
  value <- numeric(length(v))
  for (j in 1:(length(v)-1)){ 
    ind1 <- (yhat>v[j]&yhat<v[j+1])
    ind2 <- (s==0)
    ind3 <- (s==1)
    prob=(sum(ind1*ind3)/sum(ind3)*pp)/(sum(ind1*ind2)/sum(ind2))
    totaln=sum(ind1*ind2)
    value[j]=prob*totaln
  }
  return(value) 
}

#Function of model-predicted number of cases
para_ab<-function(s, predy, v, pp,cc1){
  yhat <- predy
  value <- numeric(length(v))
  for (j in 1:(length(v)-1)){ 
    ind1 <- (yhat>v[j]&yhat<=v[j+1])
    ind2 <- (s==0)
    num=sum(yhat*(1-cc1)*ind1)
    denom=sum((1-cc1*yhat)*ind1)
    totaln=sum(ind1*ind2)
    value[j]=num/denom*totaln
  }
  return(value) 
}

tpr_b <- function(s, predy, v, pp){
  yhat <- predy
  tpr <- numeric(length(v))
  num_tpr <- numeric(length(v))
  denom_tpr <- numeric(length(v))
  for (j in 1:length(v)){ 
    ind1 <- (yhat>=v[j])
    ind2 <- (s==1)
    num_tpr <- sum(ind1*ind2)
    denom_tpr <-sum(ind2)
    tpr[j]=num_tpr/denom_tpr
  }
  return(tpr)
}

fpr_b <- function(s, predy, v, pp){
  yhat <- predy
  fpr <- numeric(length(v))
  for (j in 1:length(v)){ 
    ind1 <- (yhat>=v[j])
    ind2 <- (s==0)
    ind3 <- (s==1)
    fpr[j] <- (sum(ind1*ind2)/sum(ind2)-sum(ind1*ind3)/sum(ind3)*pp)/(1-pp)
  }
  return(fpr)
}

ppv_b <- function(s, predy, v,pp){
  yhat <- predy
  ppv <- numeric(length(v))
  num_ppv <- numeric(length(v))
  denom_ppv <- numeric(length(v))
  for (j in 1:length(v)){ 
    ind1 <- (yhat>=v[j])
    ind2 <- (s==0)
    ind3 <- (s==1)
    ppv[j]=(sum(ind1*ind3)/sum(ind3)*pp)/(sum(ind1*ind2)/sum(ind2))
  }
  return(ppv)
}

npv_b <- function(s, predy, v,pp){
  yhat <- predy
  npv <- numeric(length(v))
  num_npv <- numeric(length(v))
  denom_npv <- numeric(length(v))
  for (j in 1:length(v)){
    ind1 <- (yhat<=v[j])
    ind2 <- (s==0)
    ind3 <- (s==1)
    npv[j]=1-sum(ind1*ind3)/sum(ind3)*pp/(sum(ind1*ind2)/sum(ind2))
  }
  return(npv)
}

auc_self=function(x,y){
  
  FPR=sort(x)
  TPR=sort(y)
  
  simple_auc <- function(TPR, FPR){
    # inputs already sorted, best scores first 
    dFPR <- c(diff(FPR), 0)
    dTPR <- c(diff(TPR), 0)
    sum(TPR * dFPR) + sum(dTPR * dFPR)/2
  }
  simple_auc(TPR, FPR)
}

