library(Matrix)

############################first part of selfstudy#############################

#how to define sparse 5 x 5 matrix A with 1 and 2 at entries [2,3] and [5,5]
A=sparseMatrix(i=c(2,5),j=c(3,5),x=c(1,2),dims=c(5,5))
#take a look at A
A
n=5
a=4

#skeleton for profile likelihood procedure
profile.likelihood=function(a,y,X,maximize=T){
  
  n=length(y)
  #construct B^{-1} and sqrt(D^{-1}) - both as sparse matrices
  
  
  B1 = sparseMatrix(i=c(1:n,2:n), j=c(1:n,2:n-1), x=c(rep(1,n),rep(a, n-1)), dims=c(n,n), triangular = TRUE)
  B1
  
#Compute S
  ......
  
  ytilde=as.numeric(S%*%y)#some conversions of formats needed so that lm() is happy (wants data to be of type numeric and design matrix Xtilde to be of ordinary matrix type)
  Xtilde=as.matrix(S%*%cbind(rep(1,n),X))#why add column of ones ?
  
  fit=lm(ytilde~-1+Xtilde)
  
  #compute determinant of S
  detS=......
  
  if (maximize)
    #return likelihood of data y given a (NB log likelihood for ytilde can be extracted using logLik(fit))
    else
      #return likelihood of data y given a as well as fitted coeffiecients and variance
}

#simulate data
n=10000
a=0.5
tau2=1
nu=sqrt(tau2)*Droot%*%rnorm(n)#Droot inverse of Dinvroot and Binv defined as above

U=solve(Binv,nu)#This corresponds to computing U=B eps

#Ucheck=solve(Binv)%*%eps  don't do this if n is large ! B is not sparse !!

plot(U,type="l")#take a look at simulated errors
mean(U)
var(as.numeric(U))
acf(as.numeric(U))

x=rnorm(n)
X=matrix(x,ncol=1)
y=3+2*x+U

afit=optimize(profile.likelihood,interval=c(-1,1),y=y,X=X,maximum=T)

profile.likelihood(afit$maximum,y,X,maximize=F)
