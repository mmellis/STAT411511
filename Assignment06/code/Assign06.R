### Fatty Acid Experiment ###
cpfa<-Sleuth3::ex0518
  str(cpfa)

## Columns in this dataset: ---------------------------------
#  Protein   - Protein levels in rat livers in some unknown units
#  Treatment - Fatty acid treatments in 5 levels (50,150,300,450,600) or Controls (no treatment)
#  Day       - Treatments were delivered one on each day across 5 days
#  TrtDayGroup - Column with separate groups for each day and treatment option  
  
# For this problem, we will be using the Protein, Treatment, and TrtDayGroup columns. 
    
## Edits you may want to make to the data.frame before you use it:
#  - Shorten column names to make them easier to view in output
  names(cpfa)<-c('whatever', 'another column', 'more junk', 'who cares') 
  
#  - Make sure factors are ordered the way you want
  cpfa$whatever<-factor(cpfa$whatever, levels=unique(cpfa$whatever))
    # use levels argument of factor() to order a factor to your liking
    # using unique() will match the order of appearance in cpfa$whatever

#  - Shorten treatment labels to make them easier in output
  cpfa$whatever <- factor(cpfa$whatever, labels=c('newA','newB','newC'))
    # use labels argument of factor() to change labels for each level
    # order will match levels(cpfa$whatever)
    # you can change both levels and labels arguments in factor() at once
    # but make sure labels matches ORIGINAL levels, not the one you're assigning

##  YOU WILL NEED TO EDIT THE REST OF THIS CODE TO REFLECT ANY CHANGES ABOVE!    

  
# Initial summary -----
  with(cpfa,    data.frame(
    n=tapply(Protein, TrtDayGroup, length),    # Sample size by Treatment Group
    mean=tapply(Protein, TrtDayGroup, mean),   # Mean 
    sd=tapply(Protein, TrtDayGroup, sd)))      # Standard deviation

# Boxplots ----
boxplot(Protein ~ TrtDayGroup, data=cpfa, col=rep(c('gold','blue'), each=5))
  # With boxplot(), you can specify the colors of the boxes manually, using the col argument
  # Here, I'm setting the first 5 boxes to be gold and the second 5 to be blue, using
  rep(c('gold','blue'),each=5)
  # NOTE!!! This just means that the first 5 will be gold and the second 5 will be blue
  # there's nothing here to ENSURE that those will be the correct treatment/control groups!
  
# This command adds a legend to a base graphics plot: legend()
legend(x=0, y=200, c('Treatment','Control'), fill=c('gold','blue'))


# using ggplot() to do a fancier plot
library(ggplot2)
ggplot(cpfa, aes(x=TrtDayGroup, y=Protein, fill=Treatment))+geom_boxplot()


# Fit the models ------------
# For residual plots, you can focus on residuals of fit.10.  If the full model looks
#  consistent with assumptions, any reduced model should also be ok.

fit.10a<-lm(Protein ~ TrtDayGroup, data=cpfa)         # Intercept parameterizations
fit.6a<-lm(Protein ~ Treatment, data=cpfa)


fit.10b<-lm(Protein ~ TrtDayGroup - 1, data=cpfa)     # No intercept parameterizations
fit.6b<-lm(Protein ~ Treatment - 1, data=cpfa)


# Design matrices -------
which.rows<-c(6, 9, 11, 16, 24, 26) # A selection of rows to look at
cpfa[which.rows, ]                  # Data values from our rows

# Before looking at the design matrices that R builds for you, go through the steps
#  we covered in class by hand to determine:
#   - the formula for the linear model
#   - the dimensions of each component in your formula
#   - write out the design matrix, parameter vector and fitted values based on coefficients
#      for the 6 lines of data selected above
#
# -- Repeat these steps for both intercept and no intercept versions

model.matrix(fit.10a)[which.rows, ]  # design matrix for full model
model.matrix(fit.10b)[which.rows, ]

model.matrix(fit.6a)[which.rows, ]   # design matrix for reduced model
model.matrix(fit.6b)[which.rows, ]


# When you ran the lm() function in R, it both builds the structure for the linear model
#  and estimates the coefficients, such that all the betas are replaced by their estimated 
#  values. Check the following for each of your models above to compare with your calculations:

summary(fit.10a)                      # Full output summary reported by R
summary(fit.10a)$coefficients         # Access to coefficients (= more compact display without
                                      #   all the bells and whistles)
coefficients(fit.10a)                 # Coefficient vector (estimated values for the betas)
fitted.values(fit.10a)[which.rows, ]  # Fitted values for our data selection  

# Pay special attention to which parts of the output change between intercept and 
#  no intercept versions of the same model (e.g., fit.10a vs fit.10b).


# Run the ANOVA -----
# Note: You should check assumptions BEFORE interpreting results!!  These values can ONLY 
#  be trusted if the assumptions underlying them are valid.
# See LScr_0205.R, LScr_0207.R for more code for creating assumption checking figures.
# For residual plots, you can focus on residuals of fit.10.  If the full model looks
#  consistent with assumptions, any reduced model should also be ok.

fit.10<-lm(Protein ~ TrtDayGroup, data=cpfa)
fit.6<-lm(Protein ~ Treatment, data=cpfa)
anova(fit.6,fit.10)

# F-ratio vs F-distribution ------
pf(F.obs, df1=num.df, df2=den.df, lower.tail=F) # Using pf() to get p-value

# A nice plot to illustrate p-value from F-distribution
plot(x=c(0,8), y=c(0,0.8), type='n',
     main='F distritution with 4 and 20 df', xlab=NA, ylab='df(x,4,20)')
rect(6.1803, -0.01, 8, 0.8, col='lightblue', border=NA)
curve(df(x, 4, 20), from=0, to=8, add=T)
arrows(x0=6.1803, y0=0.2, y1=0, lwd=2, col='blue')
text(7, 0.5, "F = 6.1803\np = 0.002", col='blue')

curve(df(x, 4, 10), from=0, to=8, add=T, lwd=2, col='red')

# Linear combination -----
# See Lecture Scripts for more details...
bta<-coefficients(summary(fit.10))   # Coefficient table from fit.10
C<-matrix(c(rep(0,5), 1, -1, 0, 0, 0), nrow=1)  # Match column order with design matrix
dimnames(C)<-list("C1", names(coef(fit.10))) # add names (helpful!)  

# Make sure to ALWAYS check contrast before using!
#  - contrast coefficients MUST match order for design matrix and coefficients
C

# Matrix multiplication to get estimate for combination
g <- c( C %*% bta[,"Estimate"] )                   # Estimate
se.g <- c( sqrt( C^2 %*% bta[,"Std. Error"]^2 ) )  # SE(Estimate)

# P-values for estimates
t.obs <- g / se.g                                 # t-ratio = (est - 0)/se(est)
pt(t.obs, df=fit.10$df.residual, lower.tail=F)   # p-value (one-sided, greater than)
p <- 2*pt(abs(t.obs), df=fit.10$df.residual, lower.tail=F) # p-value (two-sided)

# CI for estimate
mult<- qt(c(0.025, 0.975), df=fit.10$df.residual) # multiplier from t-distribution
g + mult * se.g                                    # 95% confidence interval





