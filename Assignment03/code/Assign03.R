### Problem 2 ------------------------------------------------------------- 
# For this problem, edit the code from other examples data sets to
#  fit the data for this problem

# Dataset to use for Assignment03, Problem 2:
zinc<-Sleuth3::ex0125
str(zinc)

# Examples from lectures
## Boxplots
boxplot(Depth ~ Year, data=Sleuth3::case0201)
boxplot(Rainfall ~ Treatment, data=Sleuth3::case0301)

## Histograms
par(mfcol=c(2,1)) 
with(Sleuth3::case0201, {
  hist(Depth[Year==1976], breaks=50, xlim=c(6,12))
  hist(Depth[Year==1978], breaks=50, xlim=c(6,12)) })

par(mfcol=c(2,1))
with(Sleuth3::case0301, {
  hist(Rainfall[Treatment=='Unseeded'], breaks=100, xlim=c(0,3000))
  hist(Rainfall[Treatment=='Seeded'], breaks=100, xlim=c(0,3000)) })


## QQplots
par(mfrow=c(1,2))
with(Sleuth3::case0101, {
  qqnorm(Score[Treatment=='Intrinsic']); qqline(Score[Treatment=='Intrinsic'])
  qqnorm(Score[Treatment=='Extrinsic']); qqline(Score[Treatment=='Extrinsic'])
})

# The par() function changes plotting options.  We can use the arguments
#  mfrow=c(nr,nc) OR mfcol=c(nr,nc)            [DON'T USE BOTH AT ONCE!]
# to create multiple panels in one plotting area.
#
# nr x nc determines the layout of the page (number of rows x number of columns)
# mfrow fills in rows first, then the columns
# mfcol fills in columns first, then rows
#
# We can also use the oma=c(Bottom,Left,Top,Right) to set an outer margin to allow
#  for an overall title, like this (example from Problem 3):

par(mfrow=c(1,3), oma=c(0,0,3,0)) # 1 row x 3 col panels, with a 3 line extra margin at top

curve(dexp(x,rate=1), xlim=c(0,10), main='Scenario 1: Skewed distribution')     #Plot1
curve(dt(x,df=3), xlim=c(-4,4), main='Scenario 2: Thick-tailed (high kurtosis)')#Plot2
curve(dunif(x, min=0, max=1), xlim=c(-0.5,1.5), main='Scenario 3: Symmetric')   #Plot3

title('Non-normal Population Distribution Scenarios', outer=T) # Overall title in outer margin

# More info on working with margins: https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet/


### Using random draws to help check assumptions -----------------------------
# For the next few weeks, as we're getting started with checking assumptions, I will
#  expect to see random draws from a normal distribution to compare with the observed 
#  data. You will need to set the draws with sample sizes that match the original data, 
#  then using the comparison between your data and these plots to decide whether the 
#  deviations from normal in your data are expected sampling error or more than expected.  
# Show the plots in your reports, and refer to them in your answer!


set.seed(10)  # set.seed() is a tool to make random draws repeatable 
              #  (especially helpful if you want to discuss specific draws!)  

x<-rnorm(34, 6.4, 5)             # Creating some example data
y<-rnorm(26, 5.9, 2)             # For Problem 2, replace this code with
obs<-c(x,y)                      #  with Zinc values from each group 



# Original sample versus draws from a normal distribution (Hit <Esc> to Exit)

par(mfrow=c(1,2), ask=T)
for(i in 1:100){
  boxplot(x, y, main='Original data')
  boxplot(rnorm(length(x)), rnorm(length(y)), main='Normal dataset')
}


par(mfrow=c(1,2), ask=T)
for(i in 1:100){
  hist(x, main='Original')
  hist(rnorm(length(x)), main='Normal')
}


par(mfrow=c(1,2), ask=T)
for(i in 1:100){
  qqnorm(x, main='Original'); qqline(x)
  randx<-rnorm(length(x))
  qqnorm(randx, main='Normal'); qqline(randx)
}


# More on par()
# ask=T is another par() argument that is useful WHEN RUNNING IN THE R CONSOLE
# It pauses the plot until you click or press ENTER. 
# Thus not overwriting the current plot until you have said ok.
# *** --> YOU SHOULD NOT USE ASK=T IN AN RMD FILE <-- ***
# It is for interactive use ONLY!
# Instead use multiple panels, such as:

set.seed(214)
par(mfrow=c(1,5), oma=c(0,0,3,0))
for(i in 1:5){
  boxplot(rnorm(n1), rnorm(n2))
}
title('Five random draws from normal distributions with equal variance', outer=T)



### Problem 3 -------------------------------------------------------------
# To take a single random sample from each distribution and calculate the sample mean:
Y<-rexp(n=30, rate=1)
 mean(Y)
 
Y<-rt(n=30, df=3)
 mean(Y)
 
Y<-runif(n=30, min=0, max=1)
 mean(Y)


# In general, we will use replicate() commands to repeat this sampling:
mX1<-replicate(10^5, mean(rexp(n=30, rate=1)))

hist(mX1, freq=F)
curve(dnorm(x, mean=1, sd=1/sqrt(30)), add=T) # Add the normal curve 
                                              #  predicted by CLT to help identify
                                              #  non-normality. You can find the 
                                              #  population mean and sd in the
                                              #  Scenarios figure


# It's important to understand what these replicate() commands are really doing...
#  Write a for loop to take a sample, calculate the sample mean, and store the results  
#  over a bunch of replicates. 

n.sims <- 500  # number of replicates for building sampling distributions
meanY<-numeric(n.sims) # a blank vector for sample means 

for(){  # Set up a variable and sequence to loop over
  
  # Steps within the for loop:
  # Take a random sample from the population distribution 
  # Save the mean of the random sample
  #  
  
}


# Use the system.time() command to see the difference in running time between these options
system.time(mX1<-replicate(10^5, mean(rexp(n=30, rate=1))))

system.time(
  # Copy and Paste your for-loop in here to compare
  #
  #
  #  
)
