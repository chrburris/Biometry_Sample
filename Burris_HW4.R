################ Christie Burris  ##################
################ Biometry Homework 4 ##################


################### Question 1 ########################

# Prompt: The diameters of three species of pine trees were compared
# at each of four locations using five randomly selected trees per species.  
# a.	Assume both factors are fixed.  
# Make an ANOVA table to test for interaction and main effects.
# b.	Make an interaction plot; what does it tell you?

########## Response ############

# a. ANOVA assuming both factors are fixed: the differences in diameter
# means are significant (p=.0003) however the significance of the location
# and interaction between species and location are both insignificant 
# (location p-value = .4779, interaction p-value = .4128)

# b. The interaction plot seems to indicate that the locations impact the
# diameter of trees across species. 
# The three species have higher differences in means in location 1 than in location 4. 
# The shape of the lines across species are different. 
# Species B has a drop in mean at location 3 whereas species A and C have an increase in mean. 

# c. the residuals do not look normal. 
# The plot of the residuals appears to have a logarithmic shape. 

diam <- read.table("~/Desktop/Biometry/hw4/ch19q09.txt",header=T,sep = ",")
colnames(diam) <- c("species","location", "diameter")
attach(diam)

# view data
plot(species,diameter)

speciesF = as.factor(species)
localF = as.factor(location)

# anova assuming factors are fixed. Including interactions
fit = lm(diameter~speciesF+localF+speciesF:localF,data=diam)
anova(fit)

# interaction plot
interaction.plot(location,species,diameter,fun=mean) 

qqnorm(fit$residuals) # check assumptions
qqline(fit$residuals) # compare



################### Question 2 ########################
# The Tukey comparison indicates that species A and C are not significantly different, while species B is significantly different from both A and C. 

#install.packages("emmeans")
#install.packages("multcompView")
library(multcompView)
library(emmeans) 
fitA_e <- emmeans(fit, "speciesF") #estimate means from fit and factor species
pairs(fitA_e)  # intervals for pairs
plot(fitA_e)   # plot means and std errors
cld(fitA_e,adjust="Tukey") 

#install.packages("phia")
library(phia)

means.fit <- interactionMeans(fit)
means.fit

# In the top right plot we see that Species A and C overlap in
# variance intervals and neither overlap with B as indicated
# in the Tukey comparison analysis.
# Also, the bottom left plot indicates variation in location is 
# not significant whatsoever. 
# We see almost complete overlap in every interval for species A, B and C.  

plot(means.fit)



################### Question 3 ########################

# Analyse whether a transformation is needed in the dbiomass variable of data. 
# To do so, we check whether variance increases with mean. 
# If it does, then a transformation will help to reduce the variance at higher mean values. 
# We plot means vs. variance for the interaction species/water with a boxplot
# and see that in fact variance does increase with mean. 
# This indicates that we should transform the data.
# We use boxcox to get the optimal power transformation. 
# The optimal transformation falls in the range (,). 
# We choose the square root transformation as this is the most relevant 
# transformation we know within the interval.
# The new boxplot indicates less variation in variance as mean increases. 
# There is however still high variation at the highest means.

library(readr)
d <- read_csv("~/Desktop/Biometry/L4 two factor models/data/ex13-48plants2.csv")
attach(d)

species <- as.factor(species)
water <- as.factor(water)
fit <- lm(dbiomass ~ water*species, data=d)
anova(fit)

library(car)
Anova(fit,Type=III)

# check assumptions 
qqnorm(fit$residuals)
qqline(fit$residuals)

# checking variance vs. mean
specieswater  <- factor(interaction(d$species,d$water))  
par(mfrow=c(1,1))
boxplot(dbiomass~specieswater)

library(MASS) 

# use boxcox to find optimal transformation
# optimal transformation is the maximum on the plot, 
# good to choose a relevant transformation that's close to the maximum

boxcox(dbiomass~specieswater, lambda = seq(-.25,1,length=100))

# this is a power transformation
d$droot = dbiomass^0.5

# recheck mean vs variance
boxplot(d$droot~specieswater)
