library(Matrix)

############################first part of selfstudy#############################



#skeleton for profile likelihood procedure
profile.likelihood=function(a,y,X,maximize=T) {
  
  n=length(y)

  
  B1 = sparseMatrix(i=c(1:n,2:n), j=c(1:n,2:n-1), x=c(rep(1,n),rep(-a, n-1)), dims=c(n,n), triangular = TRUE)

  sqDinv = sqrt(sparseMatrix(i=c(1:n), j=c(1:n), x=c((1-a^2),rep(1,n-1))))


  S = sqDinv %*% B1
  
  ytilde=as.numeric(S%*%y)
  Xtilde=as.matrix(S%*%cbind(rep(1,n),X))
  
  fit=lm(ytilde~-1+Xtilde)
  
  detS=det(S)
  if (maximize){
    return(logLik(fit)*detS)
    }else{ 
    loglikelihood = logLik(fit)*detS
  return(list(loglikelihood,fit))
    }         
          
}

#simulate data
veca = c(0,0.5,0.99)
vecn = c(20,1000)

simu = 100 

for (k in 1:length(vecn)) {
  
for (j in 1: length(veca)){
beta1 = c()
beta2 = c()
tau1 = c()

for (i in 1:simu) {
n = vecn[k]
a = veca[j]

tau2=1
B1 = sparseMatrix(i=c(1:n,2:n), j=c(1:n,2:n-1), x=c(rep(1,n),rep(-a, n-1)), dims=c(n,n), triangular = TRUE)
Droot = sqrt(sparseMatrix(i=c(1:n),j=c(1:n),x=c(1/(1-a^2),rep(1,(n-1)))))
nu=sqrt(tau2)*Droot%*%rnorm(n)

U=solve(B1,nu)

x=rnorm(n)
X=matrix(x,ncol=1)
y=3+2*x+U

afit=optimize(profile.likelihood,interval=c(-1,1),y=y,X=X,maximum=T)

logL = profile.likelihood(afit$maximum,y,X,maximize=F)
beta = coef(logL[[2]])
beta1[i] = beta[[1]]
beta2[i] = beta[[2]]
tau1[i] = summary(logL[[2]])$sigma

}
par(mfrow=c(1,3))
qqnorm(beta1)
qqline(beta1)
qqnorm(beta2)
qqline(beta2)
qqnorm(tau1)
qqline(tau1)

filename=paste("plot",format(k),format(j),".pdf",sep="")
               dev.copy2pdf(file=filename)
}
}


#plot(U,type="l")
#mean(U)
#var(as.numeric(U))
#acf(as.numeric(U))
