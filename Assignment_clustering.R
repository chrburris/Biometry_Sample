# 1a. We first output the correlation matrix to look for high values. 
# We observe is high correlation amongst top and bottom sets. For instance, top and bottom cannine (cor=.808)
# as well as top and bottom premolars (cor=.896). Naturally, the total number of teeth is highly, positively correlated with all other appearances of teeth apart from molars.
# Molars are negatively correlated with everything besides each other, with the exception of a low positive correlation with bottom incisors. 

# Making a plot of this allows us to visualize and interpret these correlations. Correlation with totals seem to reflect a legitimate trend. 
# Top and bottom premolars create a positive lienar trend. Although the data is not presented well for canines, only 3/32 animals only have one of the two teeth. 
# All other animals have either both or none, indicating a high positive correlation.

# 1b. We perform PCA to determine the dimensionality of the data. From the scree plot and the cumulative proportions it does not seem like 2 components is adequate. 

# 1c. The biplot presents the data on the coordinates of the first 2 components. The display is quite skewed to in component 2. 
# This is line with what we discovered from the scree plot. It seems that 2 components do not adequately describe the data. 

dataset<-read.csv("~/Desktop/VT/Classes/Biometry/hw7/mammals.csv",header=T)

# keep only the numerical data, remove names
numdata = dataset[,2:10]

# correlation matrix and plot
round(cor(numdata),3)
pairs(numdata)

# pca summary, notice cumulative proportions. The majority is picked up by 2 components, but it isn't until 5 components that we pick up 95%.
pca = prcomp(numdata, center=TRUE,scale=TRUE)
summary(pca)

#screeplot with horizontal line. 3 components necessary to drop below 1
screeplot(pca,type='l',main='Screeplot for mammal teeth data') 
abline(1,0,col='red',lty=2) 

# create biplot, notice that component 2 still has a lot of spread
par(mfrow=c(1,1),cex=0.7)
biplot(pca, scale=0, xlab='component 1', ylab='component 2')





#2a. Of the continuous variables (mgp, disp, drat, wt, qsec), we notice high negative correlation between mpg and disp/wt.
#    This makes sense as heavier cars tend to get fewer mpg and cars with more power tend to be heavier. 
#    drat is also negatively correlated with wt and disp, but not as highly as mpg. 

#2b. PCA indicates 2 components are sufficient to characterize the data. The biplot indicates that one axis contains mostly information on wt, mpg, drat, and disp. 
#    The other axis is primarily determined by qsec. As such, two components are sufficient to summarize the data. One might interpret these components as 1) performance and 2) power

attach(mtcars)
cars=as.matrix(mtcars[,1:11])

# pull continuous variables
cars.cont=cars[,c(1,3,5:7)]

# correlation matrix and plot
round(cor(cars.cont),3)
pairs(cars.cont)

# PCA summary, notice cumulative proportions. The majority is picked up by 2 components, but it isn't until 5 components that we pick up 95%.
cars.pca = prcomp(cars.cont, center=TRUE,scale=TRUE)
summary(cars.pca)

# extract data from PCA
eigenvectors = cars.pca$loadings
eigenvalues = cars.pca$sdev*cars.pca$sdev
prop_var_exp = eigenvalues/sum(eigenvalues)
cumulative = cumsum(prop_var_exp)

# nice table of eigenvalues, the proportion of the expression from each component and the cumulative contribution of components. 
results = as.data.frame(cbind(eigenvalues, prop_var_exp, cumulative))
round(results,3)

screeplot(cars.pca,type='l',main='Screeplot for MPG data') #screeplot with dots and lines
abline(1,0,col='red',lty=2)

par(mfrow=c(1,1),cex=0.7)
biplot(cars.pca, scale=0, xlab='component 1', ylab='component 2',xlim=c(-4,4),ylim=c(-4,4))

