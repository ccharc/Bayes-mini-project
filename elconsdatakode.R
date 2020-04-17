data =  data.frame(read.csv("elconsdata.csv",sep=";"))

y = as.matrix(data[,1])
X = as.matrix(data[,2:3])
a=0.5


fit1 = profile.likelihood(a,y,X)

afit=optimize(profile.likelihood,interval=c(-1,1),y=y,X=X,maximum=T)

logL1 = profile.likelihood(afit$maximum,y,X,maximize=F)


beta = coef(logL1[[2]])
beta
sigma = summary(logL[[2]])$sigma
sigma

mod = neg.log.profile.likelihood(theta,y,X,minimize=T)
  
  
  
  
  
  