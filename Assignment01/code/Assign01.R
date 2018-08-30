#######################################################
### STAT 411/511 - Fall 2018
### R code for help in completing Assignment 1
###  with a lot of extra stuff that might be useful
#######################################################

### At the start of a new R session 
#  - make sure you check or set your working directory
#  - start R with a clean slate 
getwd()  
rm(list=ls(all=T))  # removes all stored objects

### Another good practice: put whatever packages you need at the top
library(ggplot2)
  theme_set(theme_bw()) # Changes ggplot theme to simplier version

### Problem 1 #########################################
## Use rnorm() to take samples from a normal distribution
# rnorm() takes the arguments n, mean, and sd
rnorm(n=100,mean=10.2,sd=3.4)
rnorm(100, 10.2, 3.4)  # If not told, R assumes arguments are in order
rnorm(100)             # Defaults to mean = 0, sd = 1
rnorm(10)

## Plotting histograms of data with normal curve
#  We'll use base graphics for now
#  For a nice description of base graphics parameters see
#   https://www.statmethods.net/advgraphs/parameters.html 

#Plot a histogram for NEW set of n=100
hist(rnorm(100))
hist(rnorm(100), freq=FALSE) 
hist(rnorm(100), freq=FALSE, xlim=c(-4,4), ylim=c(0,2))   

#Draw a curve using the normal distribution function dnorm()
curve(dnorm, xlim=c(-4,4))
curve(dnorm, from=-4, to=4, ylab="Density", main="Standard Normal Curve")  

# Putting the two together
curve(dnorm, from=-4, to=4, col='red', lwd=2,
   ylab='Density', main='TITLE') 
hist(rnorm(5), freq=F, add=T)

  
### Problem 2 #########################################     
## Step 1 - n=200
y.200 <- rnorm(200, mean=0, sd=1)
hist(y.200, freq=FALSE, nclass=40, xlim=c(-4,4), col="lightgray")

#We would like to ask the computer if each value is greater than or equal to 1.96 and get a yes/no (T/F) back
#What does this give you?
y.200>=1.96
  
#R will treat TRUEs as 1's and FALSE's as 0's, so we can add them up
#What does this give us?
sum(y.200>=1.96)

#Now, let's turn it into a proportion
length(y.200)  #what does this do?
sum(y.200>=1.96)/length(y.200)  #Here is your proportion!

#Or we could use the mean() function 
#  which calculates the sample average of the 1's and 0's.
# Make sure you understand why this works.
mean(y.200>=1.96)

## Step 2 - n=10000
y.lots <- rnorm(10000, mean=0, sd=1)
hist(y.lots, freq=FALSE, nclass=40, xlim=c(-4,4), col="lightgray") 
abline(v=1.96, lwd=2, col=2) #Add a vertical line to the histogram

## Step 3 - Normal curve
## pnorm(x) integrates the area under the normal density curve to the right
#   of the argument x.  
#  Which of these are what you want?
 pnorm(1.96)
 1-pnorm(1.96)
 pnorm(-1.96) 
 pnorm(Inf)   # What do this do?

### Problem 3 ######################################### 
##  A fancy plot to look at areas
?curve #look at help file for curve and try to make sense of it
 curve(dnorm, from=-4, to=4, lwd=3, ylim=c(-0.02,0.4))
 abline(h=0,v=0)

 x <- seq(-4, 1.645, length=100)  # What does this do?
 x 
  
?polygon  #look at help file for polygon and try to make sense of it
 polygon(c(x,rev(x)), c(dnorm(x), rep(0,100)), border=4, col="lightblue")
  
?text #look at help file for text and try to make sense of it
 text(1.645,-.01,"1.645")
 text(-.2,.18,".95 in area") 
 
##3a. We can combine T/F answers using logical operators 
# & (and), | (or) 
# See https://www.statmethods.net/management/operators.html
# Some examples to demonstrate:
( x<-rnorm(10, mean=0, sd=1) )
x < -1 | x > 1 # Is the value less than -1 OR greater than 1?
x > -1 & x < 1 # Is the value greater than -1 AND less than 1?
x >= -1 & x <= 1  

## 3c. How can we do this?
# You might find the order() or sort() functions handy
?order    # Check out the help
?sort      

x
order(x)
x[order(x)] # What does this do?
sort(x)  

( 0.40*length(y.lots)-> c.40 ) # 40% of 10000
( q.40 <- sort(y.lots)[c.40] )

# Or we can use the built-in function quantile()
help(quantile) # Same as ?quantile
 
( q.40<-quantile(y.lots, 0.40) ) 
# This gives a slightly different answer.  See 'Types'
#   section in help(quantiles) to understand why. 
  
#A plot to show the value relative to the histogram of draws
  hist(y.lots, nclass=40, col="lightgray") 
  abline(v=q.40, lwd=2, col=3)
  
## 3d. qnorm(x) and pnorm(x) are opposites
#    qnorm() takes the percentile and returns the z-value
#    pnorm() takes a z-value and returns the percentile
qnorm(0.4)
pnorm(qnorm(0.4))

### Problem 4 ######################################### 
dat<-data.frame(id=1:60, obs=rnorm(n=60, mean=0, sd=2))
  str(dat) # What do we have here?
  head(dat)
  
## Random assignment
#  There are a lot of ways to do this.  Here is just one:
?sample
dat$trt<-factor('A', levels=c('A','B'))  # An new column
   ( pickB<-sample(1:60, size=30) )
   dat$trt[pickB]<-'B' 
   dat$trt                  
# Make sure you understand how this assignment works!
# Extra  credit for coming up with a different way

## Add the treatment effect
dat$new.obs<-dat$obs
dat$new.obs[pickB]<-dat$obs[pickB] + 3

  str(dat) # Now what do we have?
  head(dat)
  
## Plots - I will repeat with both base graphics or ggplot
# Stem-and-leaf plot
stem(dat$obs)    # This prints to the console, not the plotting window.

## The rest of these plots will print to the 'plot' window in RStudio
##  Please take time to familiarize yourself with plot options.
##  When you include plots in your assignments, they should be 
##   cleaned nicely and labeled...as well as you can for now. 

## A couple of good resources for base graphics plots:
##   -  http://rpubs.com/SusanEJohnston/7953
##   -  http://publish.illinois.edu/johnrgallagher/files/2015/10/BaseGraphicsCheatsheet.pdf

## A couple of good resources for ggplot:
##   - http://r-statistics.co/ggplot2-Tutorial-With-R.html
##   - https://www.rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf

## Histograms
SetA<-dat[dat$trt=='A',]$new.obs
SetB<-dat[dat$trt=='B',]$new.obs
par(mfrow=c(1,2)) # Sets to print two paneled graph
hist(SetA, col=3, nclass=10, freq=F, main="Treatment A")
hist(SetB, col=4, nclass=10, freq=F, main="Treatment B")


ggplot(dat, aes(x=new.obs))+geom_histogram(bins=10)+facet_wrap(~trt)

## Boxplots
par(mfrow=c(1,1))
boxplot(dat$new.obs~dat$trt, col=c(3,4), main="Side-by-side boxplots")

ggplot(dat, aes(x=trt, y=new.obs, fill=trt))+geom_boxplot()

##  A histogram with a boxplot too
par(mfrow=c(2,1)) # Splits the plotting window into 2 rows and 1 column
boxplot(dat$obs, col=3, ylim=c(-6,6), horizontal = TRUE)
hist(dat$obs, col=3, xlim=c(-6,6), main=NULL, xlab='Observed values')

 # OR # 
hist(dat$obs, main="Set A", col="lightgray", ylim=c(0,15))
boxplot(dat$obs, col='gray', lwd=2, pch=18, horizontal = TRUE, add=TRUE, at=14)

 # Much harder in ggplot # 
library(ggpubr)
p1<-ggplot(dat, aes(x=obs))+geom_histogram(bins=10)
p2<-ggplot(dat, aes(y=obs))+geom_boxplot()+coord_flip()
ggarrange(p2, p1, heights = c(1, 2), align = "hv", ncol = 1, nrow = 2)

## A new variable and a scatterplot
dat$new.var<-rnorm(nrow(dat))
  head(dat)

par(mfrow=c(1,1))  
plot(dat$obs, dat$new.var)

ggplot(dat, aes(x=obs, y=new.var))+geom_point()