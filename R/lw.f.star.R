#' lw.f.star
#' @description conduct an F star with for data with ceiling/floor effects
#' @param data a dataframe of data with ceiling/floor effects and corresponding group variables in wide format
#' @param formula a formula denoting the dependent and independent variable, e.g., y~group
#' @export lw.f.star

lw.f.star<-function(data,formula){
  mf <- model.frame(formula=formula, data=data)
  iv <- model.matrix(attr(mf, "terms"), data=mf)[,2]
  dv <- model.response(mf)
  samp_per_group=as.numeric(table(iv))
  mvs=matrix(unlist(by(dv,iv,rec.mean.var)),ncol=length(samp_per_group))
  samp_means_per_group=mvs[3,]
  samp_var_per_group=mvs[4,]
  f.star.test(samp_means_per_group,samp_var_per_group,samp_per_group)
}

