#' rec.mean.var
#' @description recover mean and variance of the data with ceiling/floor effects
#' @param y a (non-empty) numeric vector of data with ceiling/floor effects
#' @return percentage of ceiling/floor and recovered mean and variance
#' @export rec.mean.var

rec.mean.var<-function(y){
  floor.perc=sum(y==min(y))/length(y)*(sum(y==min(y))>=1)
  ceiling.perc=sum(y==max(y))/length(y)*(sum(y==max(y))>=1)
  a=qnorm(floor.perc)
  b=qnorm(1-ceiling.perc)
  y.t=y[y!=max(y)&y!=min(y)]
  mean=mean(y.t)
  var=var(y.t)
  if (a==-Inf & b==Inf)
  {
    return(c(mean(y),var(y)))
  }
  else
  {
    if (a==-Inf & b!=Inf)
    {
      term1=b*dnorm(b)/(pnorm(b))
    }
    if (b==Inf & a!=-Inf)
    {
      term1=a*dnorm(a)/(pnorm(a))
    }
    if (a!=-Inf & b!=Inf)
    {
      term1=(b*dnorm(b)-a*dnorm(a))/(pnorm(b)-pnorm(a))
    }
    term2=(dnorm(b)-dnorm(a))/(pnorm(b)-pnorm(a))
    var.o=var/(1-term1-term2^2)
    mean.o=mean+sqrt(var.o)*term2
  }
  return(list(`ceiling percentage`=ceiling.perc,`floor percentage`=floor.perc,` original mean`=mean.o,`original variance`=var.o))
}
