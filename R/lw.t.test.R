lw.t.test<-function(x1,x2,method_type){
  mv1=rec.mean.var(x1)
  mv2=rec.mean.var(x2)
  m1=mv1[[3]]
  v1=mv1[[4]]
  m2=mv2[[3]]
  v2=mv2[[4]]
  n=length(x.1)
  if (method_type=="a"){
    n1=n-sum(x.1==max(x.1)|x1==min(x1))*(sum(x.1==max(x.1)|x1==min(x1))>2)
    n2=n-sum(x.2==max(x.2)|x1==min(x2))*(sum(x.2==max(x.2)|x1==min(x2))>2)
  }
  if (method_type=="b"){n1=n;n2=n}
  if (method_type!="a"&method_type!="b"){stop("method type not recognized")}
  t=(m1-m2)/sqrt(v1/n1+v2/n2)
  stderrx <- sqrt(v1/n1)
  stderry <- sqrt(v2/n2)
  stderr <- sqrt(stderrx^2 + stderry^2)
  df <- stderr^4/(stderrx^4/(n1 - 1) + stderry^4/(n2 - 1))/2-1
  df=ifelse(df>0,df,1)
  p=2*pt(-abs(t),df)
  d=(m2-m1)/sqrt((n1*v1+n2*v2)/(n1+n2))
  ll=(m1-m2)-qt(.975,df)*stderr
  ul=(m1-m2)+qt(.975,df)*stderr
  return(list('adjusted t statistic'=t,'p value'=p,'adjusted d estimate'=d,'95% confidence interval'=c(ll,ul)))
}
