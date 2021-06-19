library(readxl)
library(cluster.datasets)
library(tidyverse)
library(gridExtra)

df <- read_xlsx("C:\\Users\\Mikayla\\OneDrive\\Desktop\\runoff\\GWFR.xlsx")


plot1 <- df %>% 
  ggplot(aes(x = "Rainmm", y = "Rainyear")) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "blue") +
  labs(x = "", y="Rainyear")

plot2 <- df %>%
  ggplot(aes(x = "Runoff", y = "Rainmm")) + 
  geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "orange") +
  labs(x = "", y="Rainmm")

plot3 <-  df %>%
  ggplot(aes(x = "Elevation", y = "Rainmm")) + 
  geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "green") +
  labs(x = "", y="Rainmm")

plot4 <-  df %>%
  ggplot(aes(x = "Usagelit", y = "Rainmm")) + 
  geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "red") +
  labs(x = "", y="Rainmm")


grid.arrange(plot1, plot2, plot3, plot4)

set.seed(123)
input <- df[,2:6]

kmeans(input, centers = 3, nstart = 20)
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(input, nc = 20)

set.seed(123)
clustering <- kmeans(input, centers = 8, nstart = 20)
clustering

library(cluster)
library(factoextra)

sil <- silhouette(clustering$cluster, dist(input))
fviz_silhouette(sil)
library(GGally)
library(plotly)

df$cluster <- as.factor(clustering$cluster)

p <- ggparcoord(data = df, columns = c(2:6), groupColumn = "cluster", scale = "std") + labs(x = "Rainmm", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend)
library(dplyr)

distance_mat <- dist(df, method = 'euclidean') 
distance_mat 

# Fitting Hierarchical clustering Model  
# to training dataset 
set.seed(240)  # Setting seed r
Hierar_cl <- hclust(distance_mat, method = "complete") 
Hierar_cl 

# Plotting dendrogram 
plot(Hierar_cl, cex = 0.6, hang = -1) 

Hierar_cl2 <- agnes(df, method = "complete")

# Agglomerative coefficient
Hierar_cl2$acrr
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

Hierar_cl3 <- agnes(df, method = "complete")
pltree(Hierar_cl3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

Hierar_cl4 <- diana(df)

# Divise coefficient; amount of clustering structure found
Hierar_cl4$dc


# plot dendrogram
pltree(Hierar_cl4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

Hierar_cl_a <- agnes(df, method = "complete")
cutree(as.hclust(Hierar_cl_a), k = 8)

# Cut diana() tree into 4 groups
Hierar_cl_d <- diana(df)
cutree(as.hclust(Hierar_cl_d), k = 8)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
Hierar_cl1 <- hclust(res.dist, method = "complete")
Hierar_cl2 <- hclust(res.dist, method = "complete")

# Create two dendrograms
dend1 <- as.dendrogram (Hierar_cl1)
dend2 <- as.dendrogram (Hierar_cl2)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)
fviz_nbclust(df, FUN = hcut, method = "wss")
fviz_nbclust(df, FUN = hcut, method = "silhouette")
