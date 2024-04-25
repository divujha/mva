


























































# Step 1: Install all the required packages necessary for performing PCA.
install.packages("corrr")
library(corrr)
install.packages("ggcorrplot") 
library(ggcorrplot)
install.packages("FactoMineR") 
library(FactoMineR) 
install.packages("ggplot2")
library(ggplot2)
install.packages("factoextra",dependencies = TRUE) 
library(factoextra)
install.packages("gridExtra")
library(gridExtra)
# Step 2: Load Dataset.
data=read.csv("Factor Dataset")
view(data)
str(data)
head(data)
# Step 4: Check for Null Values.
colSums(is.na(data))
# Step 5: Normalizing the data.
data_norm=scale(data) 
head(data_norm)
# Step 6: Compute the correlation matrix.
corr_matrix=cor(data_norm) 
corr_matrix 
ggcorrplot(corr_matrix)
# Step 7: Apply PCA.
data_pca=princomp(corr_matrix) 
summary(data_pca)
# Step 8: Loading matrix.
data_pca$lAoadings[,1:3]
# Step 9: Visualization of the principal component.
fviz_eig(data_pca,addlabels=TRUE)
# Step 10: Biplot of the attributes.
fviz_pca_var(data_pca,col.var="black")
# Step 11: Contribution of each variable.
fviz_cos2(data_pca, choice = "var", axes = 1:3)
# Step 12: Biplot Combined With cos2.
fviz_pca_var(data_pca, col.var = "cos2", gradient.cols = c("black", "orange", "green"), repel =
               TRUE) 