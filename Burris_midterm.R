# We first run ANOVA as if the design was completely randomized.
# An ANOVA table is produced from the linear fit of brand and chewiness
# The results indicate no significant difference in chewiness. However, visualizing the data tells us that the variance amongst each brand is very high.
# Tukey's multiple comparisons comfirms lack of significance.
# We instead decide that a randomized block design better describes the experiment.

# read data
library(readr)
Data <- read.csv("~/Desktop/Biometry/brownieCB.csv",header=T)
attach(Data)

blockF=as.factor(block)
brandF=as.factor(brand)

######### not taking days to be blocks 
# (assuming completely randomized design) ##########

# visualize data, lots of variance within brands, less variance between bands
plot(brand,chewy,ylab="Chewiness", xlab="Brand")

# fit linear model
fit <- lm(chewy ~brandF, data=Data)
anova(fit)

# check assumptions
qqnorm(fit$residuals)
# compare to normal
qqline(fit$residuals) 

# Tukey's multiple comparisons 
library(multcompView)
library(emmeans) 
fit_e <- emmeans(fit, "brandF") #estimate means from fit and factor=brand
pairs(fit_e)  # intervals for pairs
plot(fit_e)   # plot means and std errors

# output comparison groups
cld(fit_e,adjust="Tukey") 

######### taking days to be blocks ##########
# A plot of the data by days indicates possible significant difference in chewiness across brands.
# The ANOVA table indicates significance difference in brand means and day means. 
# Tukey comparisons indicates brands 1 and 2 are not significantly difference, but brand 4 is significantly different from 1 and 2. Brand 3 is not significantly different from any of the other brands. 

# plot data, x=days, y=chewiness, color=brand 
plot(block,chewy,col=c("red","blue","black","green")[brand],pch = c(15,16,17,18)[brand])
legend("topleft",legend = c("B1","B2","B3","B4"),col = c("red","blue","black","green"),pch = c(15,16,17,18))

# fit block model
fitB <- lm(chewy ~brandF+blockF, data=Data)
anova(fitB)

# check assumptions
qqnorm(fitB$residuals)
# compare to normal
qqline(fitB$residuals) 

# run tukey comparisons on new model. 
fitB_e <- emmeans(fitB, "brandF") #estimate means from fit and factor=brand
pairs(fitB_e)  # intervals for pairs
plot(fitB_e)   # plot means and std errors

# output comparison groups
cld(fitB_e,adjust="Tukey") 
