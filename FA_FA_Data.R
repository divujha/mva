

































install.packages("corrr")
library('corrr’)
install.packages("psych",dependencies = T) 
library(psych)
install.packages("GPArotation") 
library(GPArotation)
df=FactorAnalysis 
summary(df)
colSums(is.na(df))

#Compute the Correlation matrix
c<- cor(df) 
c

#Plotting the Correlation matrix
corrplot(cor(df),method = 'circle’)

#Determine Number of Factorsto Extract
fa.parallel(c, fa="fa")

#Extract (and rotate) factors:
e=fa(c,5,rotate = "promax")
e

#visualizing the factor model
fa.diagram(e$loadings)





