### Assignment08 - SLR diagnostic plots & Stats Report #########################
### Problem 1: SLR diagnostic plots  ###########################################
### For Problem 1, you should use the code below to create datasets with
###  which to explore the SLR diagnostic plots.  That means, rerunning
###  the commands below to create datasets reflecting each of the  
###  different scenarios ("good" linear relationship, non-linear 
###  relationship, non-constant variance, data with outliers); and 
###  testing them out with different sample sizes and distribution of
###  x-values.
### Your job is to get comfortable describing residual diagnostic plots 
###  for SLR.  In your write-up, please make sure you show enough detail
###  (plots with repetition, discussion) to convince Christian and I that 
###  you know your stuff AND to act as a reference to go back to for when 
###  you review lm() diagnostic plots in the future.  

## Step 1: Use the locator() function to set up a fake data set for EACH of the 
##          different scenarios.  
## THIS MUST BE DONE IN THE R CONSOLE ----  Not in an .Rmd file!!!

# Create a blank plot (use the same scales for each scenario) 
plot(x=c(0,10), y=c(0,100), type='n', xlab='x', ylab='y') # Create a blank plot

# Point'n'Click to create a sample using locator() 
xy<-locator(n=30, type='p')       # Set your sample size by changing value for n
                                            # The R Console will pause while
                                            #  you click on the graph to select
                                            #  your points.  Hit <Esc> in the 
                                            #  console if you want to stop early!
xy<-as.data.frame(xy)  


## Step 2: Copy and Paste your fake data into .Rmd file for using in the rest of 
##           the assignment.

dput(xy)  # dput() writes a text representation of your object that you can  
          #         the copy into your .Rmd file to use
          # Select the output from the dput() command in the R Console 
          # Paste it into its own R chunk in your .Rmd
          # Use include=F in your chunk header so that the messiness doesn't show 


## Step 3: Fit a SLR 
fit<-lm(y~x, data=xy)
  summary(fit)          # R's usual summary output
  model.matrix(fit)     # Design matrix
  fitted.values(fit)    # Predicted values
  residuals(fit)        # Residuals
 
# Just FYI, we could get predicted values and residuals using the design matrix
# R is not doing magic here!  
   model.matrix(fit) %*% coef(fit)  # Predicted values from design matrix
   fit$model$y - model.matrix(fit) %*% coef(fit)  # Residuals from design matrix
   
# Step 4: Describe distribution of data from scatter plot
#          + Adding the regression fit can help in looking for:
#              - Symmetry of residuals (are data points equally distributed 
#                                          above and below regression line?)
#              - Spread of residuals (are data points equally variable no matter
#                                     where you are on the x-axis?)
#              - Linearity (does that data approximately follow a straight line,
#                             or is there some other pattern that gets missed
#                             by a straight line model?)
#
#         + Make sure to think/talk about the distribution of x-values
#             - Are x-values equally distributed across their range or are data
#                clumped in some part of their range?
#             - How close are the observed x-values to x=0 (location of intercept)? 

plot(y~x, data=xy)        # Using base R graphics
abline(fit)

ggplot(xy, aes(x=x, y=y)) + geom_point() +     # Using ggplot()
  stat_smooth(method='lm', se=F)               #  NOTE: ggplot is fitting its own
                                               #   version of your linear model
                                               # - Not refering directly to your
                                               #   'fit' object
                                               
ggplot(data.frame(xy, fit=fitted.values(fit)), aes(x=x))+    # Using ggplot()
   geom_point(aes(y=y)) + geom_line(aes(y=fit))              #  alternative ways
            
      
# Step 5: Describe distribution of data using diagnostic plots
plot(fit)                   # R's default diagnostic plots
                            # See LScr_0304.R for more comments/detail
par(mfrow=c(1,4))           #  for interpreting diagnostic plots.
plot(fit)
 
# A fancy panelled plot using base graphics 
layout(cbind(c(1,1),matrix(2:5, nrow=2)))  # create a matrix of 5 panels: 
                                           #     1     2     4
                                           #     1     3     5

par(pty='s', cex.main=2)                    # Scatterplot in the big panel
plot(y~x, data=xy, main='Scenario __')      #  pty='s' makes the plotting "type"
abline(fit)                                 #     (area) be a square 

par(pty='m', mar=c(4,2,1.5,0))              # Diagnostic plots in the 4 smaller panels
plot(fit, cex.caption=0.8)                  #  pty='m' turns plotting type
                                            #  back to using 'maximum' area 

### Problem 2: Old Faithful data  ##############################################
fname<-file.choose()          # Do not use interactive functions in .Rmd!
fname                         # R Console ONLY! 
                                                            
old.faithful <- read.csv(fname)            # To use in your .Rmd, replace 'fname'
old.faithful <- read.csv('path/to/file')   # with the path saved in fname
 str(old.faithful)

# Initial visualization  
ggplot(old.faithful, aes(x=DURATION, y=INTERVAL))+
  geom_point() + labs(title="Duration of Eruption vs. Time to Next Eruption")

last_plot() + stat_smooth(method='lm', se=F)

# Fit SLR
lm.out <- lm( INTERVAL ~ DURATION, data=old.faithful )
  summary(lm.out)
  confint(lm.out, level=0.95)
  
# Residual plots by hand 
lm.out.data<-data.frame(lm.out$model, 
                        Fitted    = fitted.values(lm.out),
                        Residuals = residuals(lm.out),
                        std.resid = rstandard(lm.out),
                        leverage  = hatvalues(lm.out),
                        cooksd    = cooks.distance(lm.out))

ggplot(lm.out.data, aes(x=Fitted, y=Residuals)) + geom_point() +
   geom_hline(yintercept=0)+
   stat_smooth(method='loess', se=F)+
   labs(x = 'Fitted eruption intervals', y = 'Residual', 
        title='Fitted vs Residual plot')
        
ggplot(lm.out.data, aes(sample=Residuals)) + geom_qq() + geom_qq_line()  # What
ggplot(lm.out.data, aes(sample=std.resid)) + geom_qq() + geom_qq_line()  #  changes?

ggplot(lm.out.data, aes(x=Fitted, y=sqrt(abs(std.resid)))) + geom_point() +
   stat_smooth(method='loess', se=F)+
   labs(title='Scale-Location plot')
 
ggplot(lm.out.data, aes(x=leverage, y=std.resid)) + geom_point() +
  geom_hline(yintercept=0) 
   
# R's diagnostic plots - includes lowess line etc
par(mfrow=c(2,2))
plot(lm.out) 

# Other plots...
ggplot(old.faithful, aes(x=DATE, y=INTERVAL))+geom_point()

ggplot(old.faithful, aes(x=factor(DATE), y=INTERVAL))+
   geom_boxplot()+geom_point()
   
ggplot(data.frame(old.faithful, id=1:nrow(old.faithful)),
        aes(x=id, y=INTERVAL)) + geom_path()

ggplot(data.frame(old.faithful, id=1:nrow(old.faithful)),
        aes(x=id, y=INTERVAL)) + geom_path() + 
        facet_grid(.~DATE, space='free_x', scales='free')+
        scale_x_continuous(breaks=seq(0,nrow(old.faithful), by=5),
                        minor_breaks=seq(1,nrow(old.faithful), by=1))

# Plot of regression fit                        
fit.data <- data.frame(DURATION = seq(min(old.faithful$DURATION), 
                                      max(old.faithful$DURATION), 
                                    length.out=100))
fit.out<-predict(lm.out, newdata=fit.data, se.fit=T) # Estimates and SEs
scheffe.mult<-sqrt(2*qf(0.95, df1=2, df2=105))       # Multiplier for CI
   fit.data$INTERVAL<- fit.out$fit
     # Simultaneous confidence bands (adjusted for mult comparisons)
   fit.data$CI.lwr  <- fit.out$fit - scheffe.mult*fit.out$se.fit
   fit.data$CI.upr  <- fit.out$fit + scheffe.mult*fit.out$se.fit
     # Simultaneous prediction bands (adjusted for mult comparisons)
   fit.data$PI.lwr<-fit.out$fit-scheffe.mult*sqrt(sigma(lm.out)^2+fit.out$se.fit^2)
   fit.data$PI.upr<-fit.out$fit+scheffe.mult*sqrt(sigma(lm.out)^2+fit.out$se.fit^2)
   
ggplot(old.faithful, aes(x=DURATION, y=INTERVAL)) +  
   geom_ribbon(data=fit.data, aes(ymin = PI.lwr, ymax=PI.upr),fill='green',alpha=0.1)+
   geom_ribbon(data=fit.data, aes(ymin = CI.lwr, ymax=CI.upr),fill='blue',alpha=0.1)+
   geom_line(data=fit.data, col='blue', size=1) + geom_point()
