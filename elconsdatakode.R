data =  data.frame(read.csv("elconsdata.csv",sep=";"))

y = as.matrix(data[,1])
X = as.matrix(data[,2:3])
a=0.5

fit1 = profile.likelihood(a,y,X)
summary(fit1)
