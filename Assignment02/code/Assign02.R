#######################################################
### STAT 411/511 
### R code for help in completing Assignment 2
#######################################################

### At the start of a new R session 
#  - make sure you check or set your working directory
#  - start R with a clean slate 
getwd()  
rm(list=ls(all=T))  # removes all stored objects

### Problem 1 #########################################
finch<-Sleuth3::ex0218 
  head(finch)
  str(finch)

finch$Year<- ifelse() # Use ifelse() to change Year variable 
                      #   from numeric to character (Before/After)
  finch$Year<-factor(finch$Year, levels=c('Before','After'))  # This step will 
                      # record your new Year column as a factor, and set the 
                      # order of the levels to have Before first, After second.
                      #  (Default order for factors is alphabetical)  

### For Problem 1, part 2: You can copy and paste each option into the body of
###  your if statement, but make sure you run through each line-by-line to see 
###  what the code is doing!

## Option 1: Permutation test
obs.diff<-with(finch, mean(Depth[Year=='After'])-mean(Depth[Year=='Before']))

n2<-sum(finch$Year=='After')
ra.mx<-replicate(10^6, sample(nrow(finch), n2, replace=F))
r.diffs<-apply(ra.mx, 2, function(id){
           mean(finch$Depth[id])-mean(finch$Depth[-id])  })

print(c('est'=obs.diff, 
        'lwr'=quantile(r.diffs+obs.diff, c(0.025))[[1]],
        'upr'=quantile(r.diffs+obs.diff, c(0.975))[[1]],
        'p' = mean(abs(r.diffs)>=abs(obs.diff))))

## Option 2: T-test
tstat<-lm(Depth ~ Year, data=finch)

print(c('est'=coefficients(tstat)[[2]], 
        'lwr'=confint(tstat)[2,1],
        'upr'=confint(tstat)[2,2],
        'p' = summary(tstat)$coef[2,4]))

## Option 3: Sassy message
print('sassy message')

### Problem 2 #########################################
Novice<-c(30, 35, 26, 40, 36, 20, 45, 31, 33, 29, 21, 48)
Experienced<-c(31, 15, 25, 19, 28, 17, 19, 18, 24, 10, 20, 21)

# We could also store this in a data.frame 
nN<-length(Novice)
nE<-length(Experienced)
my.data<-data.frame(subject=1:(nN+nE),
                    errors=c(Novice, Experienced), 
                    experience=factor(rep(1:2, c(nN,nE)), 
                                  labels=c('Novice','Experienced'))) 

## A couple of plots of the data
library(ggplot2)
  #dot plot within ggplot2  
    ggplot(my.data, aes(x=errors)) +  #setting up data and x-variable
      geom_dotplot() +               #telling it we want a dotplot
      facet_grid(experience ~ .)      #split up for each treatment
            
  #stacked histograms with ggplot2
    ggplot(my.data, aes(x=errors)) +  #setting up data and x-variable
      geom_histogram(binwidth=1) +   #telling it we want histogram
      facet_grid(experience ~ .)          # with binwidth=1

  #side-by-side boxplots with ggplot2
    ggplot(my.data, aes(y=errors, x=experience)) +  #setting up data and vars
      geom_boxplot() +                            #add boxplot
      geom_point(colour="red", size=3, shape=1,   #add points to boxplot
       position=position_jitter(height=0))

## Comparing a difference in means
obs.diff<-mean(Novice)-mean(Experienced)

## Approximate ampling distribution
# One randomization
ra2 <- sample(my.data$subject, size=nN, replace=FALSE)

my.data$subject[ra2]  #Novice subjects (same as ra2)
my.data$subject[-ra2] #Experienced subjets (subject numbers NOT in ra2)

ra2.diff <- mean(my.data$error[ra2])-mean(my.data$error[-ra2])
ra2.diff

# Many randomizations
ra.mat <- replicate(10^6, sample(my.data$subject, size=nN, replace=FALSE))

rand.dist <- apply(ra.mat, 2, function(id){ 
                     mean(my.data$error[id])-mean(my.data$error[-id]) })
                     
## Plotting the approximate sampling distribution                     
hist(rand.dist, col=gray(0.7), nclass=100)
  abline(v=obs.diff, col=2, lwd=3)
  abline(v=-obs.diff, col=2, lwd=1) #to look as extreme or more               
  
# Proportions  ## Think about which version you want based on the question!
mean(rand.dist>=obs.diff)
mean(rand.dist>=-obs.diff)
mean(abs(rand.dist)>=abs(obs.diff))

quantile(rand.dist, c(0.025, 0.975)) # Quantile cut-offs from reference distribution
                     
## Testing for a difference in variances
# calculate a test statistic for part 2:
#   first try using the ratio of standard deviations
sd.ratio<-with(my.data,
   sd(errors[experience=="Novice"])/sd(errors[experience=="Experienced"]))

#obtain approximate sampling distribution
ra.mat <- replicate(10^6, sample(my.data$subject, size=nN, replace=FALSE))
rand.dist <- apply(ra.mat, 2, function(id){ 
                     sd(my.data$error[id])/sd(my.data$error[-id]) })

#make a histogram
hist(rand.dist, nclass=100,col=gray(0.9),
     main="Sampling Distribution of SD ratio")
mean(rand.dist >= sd.ratio)

# calculate a test statistic for part 2:
#   now try root mean deviance

# A function to calculate RMD
RMD <- function(x1, x2){ 
    mean(abs(x1 - median(x1)))/mean(abs(x2-median(x2))) 
  }

obs.RMD<-with(my.data,
   RMD(errors[experience=="Novice"], errors[experience=="Experienced"]) )

#obtain approximate sampling distribution
ra.mat <- replicate(10^6, sample(my.data$subject, size=nN, replace=FALSE))
rand.dist <- apply(ra.mat, 2, function(id){ 
                     RMD(my.data$error[id], my.data$error[-id]) })
#make a histogram
hist(rand.dist, nclass=100, col=gray(0.9),
  main="Sampling Distribution of RMD")
mean(rand.dist >= obs.RMD)

### BONUS: Thinking about the formulas for standard deviation vs 
# root mean deviance, why might we prefer RMD?  What circumstance 
# would lead to a difference between these two measures? 
