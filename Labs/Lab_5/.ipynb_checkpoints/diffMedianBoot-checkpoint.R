diffMedianBoot <- function(x,y,R=1000,lower=.025,upper=.975) {
  boo = c(1:R)
  nx = length(x)
  ny = length(y)
  for (i in c(1:R)) {
    rx = sample(x,nx,replace=T)
    ry = sample(y,ny,replace=T)
    boo[i] = median(rx) - median(ry)
  }
  par(mfrow=c(3,1))
  hist(boo,prob=T)
  boxplot(boo, horizontal = T)
  qqnorm(boo)
  qqline(boo)
  return(list(median(x) - median(y), sd(boo), quantile(boo,c(lower,upper))))
}