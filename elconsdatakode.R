data =  data.frame(read.csv("elconsdata.csv",sep=";"))

y = as.matrix(data[,1])
X = as.matrix(data[,2:3])
a=0.99


fit1 = profile.likelihood(a,y,X)

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
  
  
  
  