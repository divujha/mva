











































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
df=read.csv("Apple dataset")
view(df)
str(df)
# Step 3: Removing unnecessary Columns if required
#data <- data[, -1] 
#head(data)
# Step 4: Check for Null Values.
colSums(is.na(df))
df=na.omit(df) #for removing missing value
colSums(is.na(df))
# Step 5: Normalizing the data.
df$Acidity=as.numeric(df$Acidity)
df=df[,2:8]
data_normalized =scale(df) 
head(data_normalized)
#Compute the Correlation matrix :
corr_matrix <- cor(data_normalized)
Corr_matrix
ggcorrplot(corr_matrix)
#Applying PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
#Loading matrix :
data.pca$loadings[, 1:4]
#Visualization of the principal component :
fviz_eig(data.pca, addlabels = TRUE)
#Biplot of the attributes :
fviz_pca_var(data.pca, col.var = "black")
#Contribution of each variable :
fviz_cos2(data.pca, choice = "var", axes = 1:3)
#Biplot Combined With cos2 :
fviz_pca_var(data.pca, col.var = "cos2",
            gradient.cols = c("black", "orange", "green"),
            repel = TRUE)
