





























install.packages("corrr")
library('corrr’)

install.packages("psych",dependencies = T) 
library(psych)

install.packages("GPArotation") 
library(GPArotation)

df=read.csv("C:/ProgramData/Microsoft/Windows/Start Menu/Programs/RStudio/train.csv") 
summary(df)

colSums(is.na(df)) 

df=na.omit(df) 
colSums(is.na(df) 

df= [,9:22]
e=cor(df [,9:22])

#Plotting the Correlation matrix : 
corrplot(cor(df),method = 'circle’) 

#Determine Number of Factorsto Extract
fa.parallel(e, fa="fa")

#Extract (and rotate) factors
e=fa(c,5,rotate = "promax")
e

fa.diagram(e$loadings)














