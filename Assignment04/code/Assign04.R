library(ggplot2)
theme_set(theme_bw())

#### Assignment04 - Problem 2  -----------------------------------------------
# Parts a-c should be answered without requiring any additional code
### 2d) -----------------------------------------------------------------------

TRex<-Sleuth3::ex0523
  str(TRex)  # Check formating BEFORE using the data...is this factor ordering what you want??
  TRex$Bone<-factor(TRex$Bone, levels=unique(TRex$Bone))
  
# boxplot
ggplot(TRex, aes(x=Bone, y=Oxygen))+geom_boxplot()+geom_jitter(height=0, width=0.1)

# histograms
ggplot(TRex, aes(x=Oxygen))+geom_histogram()+facet_wrap(~Bone, ncol=2)

# qqplot
ggplot(TRex, aes(sample=Oxygen))+geom_qq()+geom_qq_line()+facet_wrap(~Bone, nrow=1)

### Fit full model
lm.full<-lm(Oxygen ~ Bone, data=TRex)
  ## Note that for ANOVA we don't actually need to look at the summary()
  ##  output for the model that we fit.  The F-test in an ANOVA is based on 
  ##  a comparison of two models!  
  ## Before we get to the statistical test, we need to make sure that the model 
  ##  underlying the ANOVA is appropriate for our data.  We test assess this 
  ##  using the full model.  

## We're going to start relying more and more on residuals to assess violations
##  of assumptions.  What changes when I switch from using the original response
##  values to using residuals?
ggpubr::ggarrange(
  ggplot(TRex, aes(x=Bone, y=Oxygen))+geom_boxplot()+geom_jitter(height=0),
  ggplot(data.frame(TRex, Residuals=residuals(lm.full)), aes(x=Bone, y=Residuals))+
      geom_boxplot()+geom_jitter(height=0),
 nrow=2, ncol=1)

## Once the observed differences in means are removed, we'd like to be able 
##  to treat the distribution residuals as a single, normally distributed
##  population with constant variance.

# qqplot of residuals
qqnorm(residuals(lm.full)); qqline(residuals(lm.full))

# fitted vs residuals
#  "fitted" value = group mean for each observation
#  We want there to be no pattern between the magnitude of the residuals and 
#  their group means.  Increasing variance (spread of residuals) with predicted
#  mean is a strong indication of the need for transformation.
plot(fitted.values(lm.full), residuals(lm.full))

# boxplot of residuals vs random draws
par(ask=T, mfrow=c(1,2))
for(i in 1:100){
  boxplot(residuals(lm.full) ~ TRex$Bone)
  
  random.data<-data.frame(Oxygen = rnorm(nrow(TRex)), Bone=TRex$Bone)
  rdata.full<-lm(Oxygen ~ Bone, data=random.data)
  boxplot(residuals(rdata.full) ~ random.data$Bone)
}

### 2e) -----------------------------------------------------------------------
lm.full<-lm(Oxygen ~ Bone, data=TRex)
lm.red<-lm(Oxygen ~ 1, data=TRex)
  anova(lm.red, lm.full)
  
  
  
  