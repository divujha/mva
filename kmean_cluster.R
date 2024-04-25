






































#Libraries: 
library(factoextra) 
library(cluster) 
library(NbClust) 
library(ggplot2) 
library(gridExtra) 
library(patchwork) 
library(maps) 
library(dplyr)

#Sales ProducƟon data K-means:- 
# Read the CSV file (replace 'your_file.csv' with the actual file path) 
df <- read.csv("D:/kaushik/MSC/MVAsem2/Sales_Product_Details.csv") 
head(df,2)

#Convert non-numerical col to numerical col 
df$Product_DescripƟon <- as.numeric(factor(df$Product_DescripƟon))
df$Product_Line <- as.numeric(factor(df$Product_Line)) 
df$Product_Category <- as.numeric(factor(df$Product_Category)) 
df$Raw_Material <- as.numeric(factor(df$Raw_Material)) 
df$Region <- as.numeric(factor(df$Region)) 
head(df,2) 

#Check for null values and remove it 
df1 <- na.omit(df) 
null_count <- colSums(is.na(df1)) 
print(null_count) 

# Standardize the data 
df1 <- scale(df1) 
head(df1,2) 

# Visualize opƟmal number of clusters using WCSS
fviz_nbclust(df1, kmeans, method = "wss") 

#calculate gap staƟsƟc based on number of clusters
gap_stat <- clusGap(df1, 
                    FUN = kmeans, 
                    nstart = 25, 
                    K.max = 10, 
                    B = 50) 

#plot number of clusters vs. gap staƟsƟc
fviz_gap_stat(gap_stat) 

#make this example reproducible 
set.seed(1) 
#perform k-means clustering with k = 3 clusters 
km1 <- kmeans(df1, centers = 3, nstart = 25) 
km1

#plot results of final k-means model 
fviz_cluster(km1, data = df1) 

#EDA of 3 clusters: 
#find means of each cluster 
cluster_means <- aggregate(df, by=list(cluster=km1$cluster), mean) 
cluster_means 

# Combine cluster assignments with original data 
final_data <- cbind(df, cluster = km1$cluster) 
Bar graph for all clusters together: 
#FuncƟon to create bar graphs for Product_ID against QuanƟty for all clusters together
plot_product_id_vs_quanƟty_all_clusters <- funcƟon(cluster_data) {
  ggplot(cluster_data, aes(x = as.factor(Product_ID), y = QuanƟty)) +
    geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") + 
    labs(Ɵtle = "Product ID vs. QuanƟty",
         x = "Product ID", y = "Average QuanƟty") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# Create bar graph for all clusters together 
plot_list <- list() 
for (i in unique(final_data$cluster)) { 
  cluster_subset <- final_data[final_data$cluster == i, ] 
  plot_list[[i]] <- plot_product_id_vs_quanƟty_all_clusters(cluster_subset) +
    ggƟtle(paste("Cluster", i))
} 
# Combine plots into a single plot with subplots 
all_plots <- do.call(grid.arrange, c(plot_list, ncol = 3)) 

Create pie charts for each cluster: 
plots <- lapply(unique(final_data$cluster), funcƟon(cluster_num) {
  cluster_subset <- final_data[final_data$cluster == cluster_num, ] 
  region_counts <- table(cluster_subset$Region) 
  pie_data <- data.frame(region = names(region_counts), count = as.numeric(region_counts)) 
  ggplot(pie_data, aes(x = "", y = count, fill = region)) + 
    geom_bar(width = 1, stat = "idenƟty") +
    coord_polar(theta = "y") + 
    labs(Ɵtle = paste("Region DistribuƟon for Cluster", cluster_num)) +
    theme_void() + 
    theme(legend.posiƟon = "right")
  }) 

# Combine plots into a single plot with subplots 
combined_plot <- wrap_plots(plots, nrow = 1) 
# Display the combined plot 
print(combined_plot)


# Add cluster informaƟon to the dataset
df$Cluster <- as.factor(km1$cluster) 

#Create scaƩer plot for laƟtude and longitude: 
# Plot map focusing on specified limits 
world <- map_data("world") 
plot_map <- ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray90", color = "black") + 
  geom_point(data = df, aes(x = Longitude, y = LaƟtude, color = Cluster), size = 3) +
  labs(Ɵtle = "LaƟtude and Longitude ScaƩer Plot") +
  theme_minimal() + 
  scale_color_manual(values = c("blue", "red", "green")) + # Define colors for clusters 
  coord_cartesian(xlim = c(-10, 10), ylim = c(48, 55)) + # Adjust the limits as specified 
  geom_text(data = df, aes(label = Cluster , x = Longitude, y = LaƟtude), size = 3, vjust = -0.5, posiƟon = 
              posiƟon_jiƩer(width = 0.1, height = 0.1)) # Add jiƩering to text labels
# Display the plot 
print(plot_map)

# Group points with the same coordinates and cluster, and print corresponding dates 
grouped_df <- df %>% 
  group_by(LaƟtude, Longitude, Cluster) %>%
  summarise(Dates = paste(Date, collapse = ", ")) 
print(grouped_df) 

#HAC:- 
# Compute the distance matrix using Euclidean distance 
res.dist <- dist(df) 
# Perform hierarchical clustering 
res.hc <- hclust(d = res.dist, method = "ward.D2") 
# Print the hierarchical clustering result 
print(res.hc) 

# Visualize the dendrogram with cluster membership 
fviz_dend(res.hc, cex = 0.5, k = 3, color_labels = TRUE)

# Compute copheneƟc distance
res.coph <- copheneƟc(res.hc)
# CorrelaƟon between copheneƟc distance and the original distance
correlation <- cor(res.dist, res.coph) 
print(correlation)
# Perform hierarchical clustering with a different method (opƟonal)
res.hc2 <- hclust(res.dist, method = "average") 
correlation2 <- cor(res.dist, copheneƟc(res.hc2))
print(correlation2)

# Visualize the dendrogram with cluster membership by average method 
fviz_dend(res.hc2, cex = 0.5, k = 3, color_labels = TRUE)

# Determine the cluster membership 
grp <- cutree(res.hc, k = 3) 
print(grp)

# Display the distribuƟon of clusters
table_grp <- table(grp) 
print(table_grp) 












