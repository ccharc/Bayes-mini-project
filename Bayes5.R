y = c(4,6,6,7,3,5,3,11,10,5)

y. = sum(y)

mean = 6 

var = 10 

alpha = 36/10

beta = sqrt(10/alpha)


alpha1 = y. + alpha

beta1 = beta / (1+length(y)*beta)

mean1 = alpha1*beta1

var1 = alpha1*beta1^2    


alphaconf = 0.05

qlower = qgamma(alphaconf / 2, alpha1, scale = beta1)

qupper = qgamma(1- alphaconf/2, alpha1, scale = beta1)

postinterv = c(qlower,qupper)
postinterv
lower = mean1 - 2 * sqrt(var1)
upper = mean1 + 2* sqrt(var1)

c(lower,upper)

x = seq(0,10, len = 100)

plot(x, dgamma(x = seq(0,10, len = 100), alpha1, scale = beta1))
