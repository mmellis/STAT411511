## ----setup, include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      cache.path='cache/A2Sol_',
                      fig.path='figure/A2Sol_')


## ----2---------------------------------------------------------------------------------
Novice<-c(30, 35, 26, 40, 36, 20, 45, 31, 33, 29, 21, 48)
Experienced<-c(31, 15, 25, 19, 28, 17, 19, 18, 24, 10, 20, 21)


## ----2c, cache=T-----------------------------------------------------------------------
obs<-c(Novice, Experienced)
n2<-length(Novice); n1<-length(Experienced)
ra.mx<-replicate(10^4, sample(n1+n2, n2))  # Increase number of replicates
r.dist<-apply(ra.mx, 2, function(x) mean(obs[x])-mean(obs[-x]))


## ----2d, echo=F, fig.height=3, fig.width=5, fig.align='left', cache=T------------------
hist(r.dist, freq=F, xlab='XLABEL', main='ADD TITLE HERE', 
    xlim=c(-15,15), ylim=c(0,0.1), breaks=50)


## ----2e, echo=F, fig.height=3, fig.width=5, fig.align='left', cache=F------------------


## ----2g--------------------------------------------------------------------------------
RMD<-function(x1,x2) mean(abs(x1-median(x1)))/mean(abs(x2-median(x2)))
RMD(Novice, Experienced)


## ----2i-a, cache=T---------------------------------------------------------------------
r.dist.rmd<-apply(ra.mx, 2, function(x) RMD(obs[x], obs[-x]))


## ----2i-b, echo=F, fig.height=3, fig.width=5, fig.align='left', cache=F----------------
hist(r.dist.rmd, freq=F, xlab='XLABEL', main='TITLE', 
    xlim=c(0,3), ylim=c(0,2), breaks=50)


## ----2j, fig.height=3, fig.width=5, fig.align='left',echo=F, cache=F-------------------
obs.RMD<-RMD(Novice, Experienced)

