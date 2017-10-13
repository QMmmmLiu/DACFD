#' f.star.test
#' @description conduct a Browne-Forsythe F star test
#' @param means a (non-empty) numeric vector of the group means
#' @param variances a (non-empty) numeric vector of the group variances
#' @param ns a (non-empty) numeric vector of sample size per group
#' @return F star value, p value, and f.squared
#' @export f.star.test

f.star.test<-function(means,variances,ns){
    MSB.s=sum(ns*(means-mean(means))^2)
    MSW.s=sum((1-ns/sum(ns))*variances)
    F.s=MSB.s/MSW.s
    gs=((1-ns/sum(ns))*variances)/MSW.s
    f=1/(sum(gs^2/(ns-1)))
    p.value=1-pf(F.s,length(means)-1,f)
    f.squared=(length(means)-1)*F.s/sum(ns)
    return(list("F star value"=F.s,"p value"=p.value,"cohen's f squared estimate"=f.squared))
}
