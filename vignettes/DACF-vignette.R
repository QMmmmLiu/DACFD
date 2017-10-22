## ------------------------------------------------------------------------
library(DACF)
# Simulate healthy data for two groups
x.1=rnorm(300,2,4)
x.2=rnorm(300,3,5)
# check mean and variance for simulated healthy data
mean(x.1);var(x.1)
mean(x.2);var(x.2)
# induce ceiling effects of 20% in group 1
x.1.cf=induce.cfe(.2,0,x.1)
# induce floor effects of 10% in group 2
x.2.cf=induce.cfe(0,.1,x.2)
# recover the mean and variance for ceiling/floor data
rec.mean.var(x.1.cf)
rec.mean.var(x.2.cf)
# conduct a t test on healthy data
t.test(x.1,x.2)
t.test(x.1.cf,x.2.cf)
# conduct an adjusted t test on ceiling/floor data
lw.t.test(x.1.cf,x.2.cf,"a")
lw.t.test(x.1.cf,x.2.cf,"b")
# generate a dataframe for ANOVA demo
testdat=threeganova.sim(10000,.0625,1)
# induce ceiling/floor effects in the data
testdat.cf=testdat
testdat.cf[testdat.cf$group==2,]$y=induce.cfe(.2,0,testdat.cf[testdat.cf$group==2,]$y)
# conduct an adjusted F star test on ceiling/floor data
lw.f.star(testdat.cf,y~group,"a")
lw.f.star(testdat.cf,y~group,"b")

