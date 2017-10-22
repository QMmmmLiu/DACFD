#' induce.cfe
#' @description inducing ceiling/floor effects in data
#' @param floor.perc a (non-empty) numeric value from 0 to 1 denoting the desired percentage of floor effects
#' @param ceiling.perc a (non-empty) numeric value from 0 to 1 denoting the desired percentage of ceiling effects
#' @param y a (non-empty) numeric vector of data
#' @return y scores with induced ceiling/floor effects
#' @export induce.cfe



induce.cfe<-function(floor.perc,ceiling.perc,y)
{
  m=mean(y)
  s=sd(y)
  a=qnorm(floor.perc,m,s)
  b=qnorm(1-ceiling.perc,m,s)
  y[y>=b]=b
  y[y<=a]=a
  return(y)
}
