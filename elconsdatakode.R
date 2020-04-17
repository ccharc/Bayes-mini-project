data =  data.frame(read.csv("elconsdata.csv",sep=";"))

y = as.matrix(data[,1])
X = as.matrix(data[,2:3])
a=0.99


fit.mod = profile.likelihood(a,y,X)

afit=optimize(profile.likelihood,interval=c(-1,1),y=y,X=X,maximum=T)

logL1 = profile.likelihood(afit$maximum,y,X,maximize=F)


beta = coef(logL1[[2]])

tau = summary(logL1[[2]])$sigma


theta =c(a,phi)


mod = neg.log.profile.likelihood(theta,y,X,minimize=T)


thetastart=c(log(a/(1-a)),log(1))
thetafit=optim(thetastart,neg.log.profile.likelihood,y=y,X=X)
thetafit

ahat=2*exp(thetafit$par[1])/(1+exp(thetafit$par[1]))-1
phihat=exp(thetafit$par[2])


a=(ahat+1)/2
thetastart=c(log(a/(1-a)),log(phihat))
neg.log.profile.likelihood(thetastart,y,X,minimize=F)

#Bootstrap 1
k = 0
go = TRUE
like.for.model.bootstrapping <- NULL
while(go){
  y.sim = as.matrix(cbind(rep(1,nrow(X)), X)) %*% (coef(fit.mod))
  + sample(resid(fit.mod), length(Y), replace = T)
  afit2 = optimize(profile.likelihood,interval=c(-1,1),y=y.sim,X=X,maximum=T)
  like.for.model.bootstrapping = c(like.for.model.bootstrapping, afit2$objective)
  k = k+1
  go = k<1e3
}

#Bootstrapping 2
k = 0
go = TRUE
like.for.model2.bootstrapping <- NULL
while(go){
  y.sim = as.matrix(X) %*% (beta) + sample(residual, length(y), replace = T)
  afit2 = optim(thetastart, neg.log.profile.likelihood, y = y.sim, X = X)
  model2 = neg.log.profile.likelihood(afit2$par, y=y.sim, X = X,
                                       pos = F, maximize = T)
  like.for.model2.bootstrapping = c(like.for.model2.bootstrapping, model2[[1]])
  k = k+1
  go = k<1e3
}


  