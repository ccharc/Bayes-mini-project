library(Matrix)

############################first part of selfstudy#############################


#skeleton for profile likelihood procedure
profile.likelihood=function(a,y,X,maximize=T) {
  
  n=length(y)

  
  B1 = sparseMatrix(i=c(1:n,2:n), j=c(1:n,2:n-1), x=c(rep(1,n),rep(-a, n-1)), dims=c(n,n), triangular = TRUE)
  B1
  sqDinv = sqrt(sparseMatrix(i=c(1:n), j=c(1:n), x=c(rep(1-a^2))))
  sqDinv

  S = sqDinv %*% B1
  S
  
  ytilde=as.numeric(S%*%y)
  Xtilde=as.matrix(S%*%cbind(rep(1,n),X))
  
  fit=lm(ytilde~-1+Xtilde)
  
  
  
  detS=det(S)
  if (maximize){
    return(logLik(fit))
    }else{ 
    loglikelihood = logLik(fit)
  return(list(loglikelihood,fit))
    }         
          
}

#simulate data
n=10000
a=0.5
tau2=1
Droot = solve(sqDinv)
nu=sqrt(tau2)*Droot%*%rnorm(n)

U=solve(B1,nu)


plot(U,type="l")
mean(U)
var(as.numeric(U))
acf(as.numeric(U))

x=rnorm(n)
X=matrix(x,ncol=1)
y=3+2*x+U

afit=optimize(profile.likelihood,interval=c(-1,1),y=y,X=X,maximum=T)

profile.likelihood(afit$maximum,y,X,maximize=F)




