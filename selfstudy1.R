#reparametrization so we are sure to have positive phi and |a|<1
#theta=(logit((a+1)/2),log(phi))

#if A=t(R)%*%R then we can compute z=A^{-1}x by solving Az=x wrt. z. That is
#first temp=solve(t(R),x) and secondly z=solve(R,temp)

neg.log.profile.likelihood=function(theta,y,X,minimize=T){
  
  a=2*exp(theta[1])/(1+exp(theta[1]))-1
  phi=exp(theta[2])
  print(c(a,phi))
  
  n=length(y)
  #construct B and sqrt(D) as sparse matrices
  B1 = sparseMatrix(i=c(1:n,2:n), j=c(1:n,2:n-1), x=c(rep(1,n),rep(-a, n-1)), dims=c(n,n), triangular = TRUE)
  
  sqDinv = sqrt(sparseMatrix(i=c(1:n), j=c(1:n), x=c((1-a^2),rep(1,n-1))))
 
  #Compute Q and Qtilde (again as sparse matrices)
  Q  = t(B1)%*%sqDinv%*%B1
  Qtilde=sparseMatrix(i=c(1:n),j=c(1:n),x=rep(phi,n))+Q
  
  #compute Cholesky factorization
  CholQtilde=chol(Qtilde)
  
  
  #compute \hat \beta(\theta)
  #(X^T W^{-1} X)^{-1} X^\T W^{-1} y
  #using Cholesky factor CholQtilde
  w = Q%*%y
  x = solve(t(CholQtilde),w)
  WinvY= solve(CholQtilde,x) 
  
  w = Q%*%X
  x = solve(t(CholQtilde),w)
  WinvX= solve(CholQtilde,x) 
  
  betahat = solve(t(X) %*% WinvX) %*% t(X) %*%  WinvY
  
  
  #check when n small
  #Winv=solve(Qtilde)%*%Q
  #betahat=solve(t(X)%*%Winv%*%X)%*%t(X)%*%Winv%*%y#OK
  
  #compute \hat \sigma^2(\theta)
  residual=y-X%*%betahat
  w = Q%*%residual
  x = solve(t(CholQtilde),w)
  WinvRes= solve(CholQtilde,x) 
  
  sigma2hat=sum(residual*WinvRes)/n
  
  #check
  #z3=Winv%*%residual
  #sigma2hat=sum(residual*z3)/n
  #sigma2hat
  
  
  #compute log likelihood. Note: determinant returns log determinant of original matrix.
  #check (only for small n)
  #V=sigma2hat*(diag(rep(1,n))+solve(Q)*phi)
  #-log(det(V))/2#-1.92
  #Vinv=solve(Qtilde)%*%Q/sigma2hat#OK
  
  #detVinv=det(Q)/(det(Qtilde)*sigma2hat^n)
  #logdetVinvhalf=log(det(Q))/2-log(det(Qtilde))/2-n*log(sigma2hat)/2
  
  detQ=det(Q)
  
  loglikelihood=-n*log(sigma2hat)/2-determinant(CholQtilde)$modulus+log(detQ)/2
  #return negative log likelihood for later use with optim() which minimizes as default.
  if (minimize)
    return(-loglikelihood)
  else
    return(list(-loglikelihood,betahat,sigma2hat))
}

#simulate data
n=10000
a=0.5
tau2=0.25
sigma2=1
ii=c(1:n,2:n)
jj=c(1:n,1:(n-1))
Binvij=c(rep(1,n),rep(-a,n-1))
Binv=sparseMatrix(i=ii,j=jj,x=Binvij,dims=c(n,n))
Droot=sparseMatrix(i=c(1:n),j=c(1:n),x=sqrt(c(1/(1-a^2),rep(1,n-1))))
nu=sqrt(tau2)*Droot%*%rnorm(n)

U=solve(Binv,nu)

plot(U,type="l")
mean(U)
var(as.numeric(U))
acf(as.numeric(U))

x=rnorm(n)
X=cbind(rep(1,n),x)
y=3+2*x+U+rnorm(n,sd=sqrt(sigma2))

tempa=(a+1)/2
thetastart=c(log(tempa/(1-tempa)),log(tau2/sigma2))
neg.log.profile.likelihood(thetastart,y,X,minimize=F)

tempa=(0.3+1)/2
thetastart=c(log(tempa/(1-tempa)),log(1))
thetafit=optim(thetastart,neg.log.profile.likelihood,y=y,X=X)
thetafit

ahat=2*exp(thetafit$par[1])/(1+exp(thetafit$par[1]))-1
phihat=exp(thetafit$par[2])


tempa=(ahat+1)/2
thetastart=c(log(tempa/(1-tempa)),log(phihat))
neg.log.profile.likelihood(thetastart,y,X,minimize=F)




