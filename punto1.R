n<-40
meanExp<-1/0.2
nsim<-1000
mat<- matrix(NA,nsim,n)
meanExpData<-vector("numeric",nsim)
sdExpData<-vector("numeric",nsim)
varExpData<-vector("numeric",nsim)
## obtengo mis datos
for(i in 1:nsim){
  mat[i,]<-rexp(40,0.2)
  meanExpData[i]<- mean(mat[i,])
  sdExpData[i]<-sd(mat[i,])  
  varExpData[i]<- sum((mat[i,]-meanExpData[i])^2)/(n-1)
}

## histograma para la media
mean(meanExpData)
hist(meanExpData)
abline(v=mean(meanExp),col="red",lwd=3)

## histogra para la varianza
hist(varExpData)
sqrt(mean(varExpData))
