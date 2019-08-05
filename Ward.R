# 2019 리더십평가 데이터를 활용하여 군집분석(Clustering Analysis)을 실시해 본다.
# 군집분석 실시 전에 최적화된 군집의 숫자를 찾아내 본다. - Ward 법 포함

rm(list=ls())
setwd("D:/B2B/2019/02.Operation/17.LDS/01_data")

lds19 <- read.csv("19ldspnn.csv")
lds19 <- na.omit(lds19)
lds19x <- lds19[,(82:87)]


library(dplyr)
library(VIM)

set.seed(20)
aggr(lds19)

summary(lds19)

# K-Means Clustering 을 하기 전에 Hierachical Clustering (Ward법 - 탐색적 군집분석)을
# 먼저 시행하여 적정 군집의 수를 파악해 봄.


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


df <- lds19x
df <- na.omit(df)
df <- scale(df)
head(df)
# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)

lds19x %>%
  mutate(cluster = sub_grp) %>%
  head

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))

# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

# Determine optimal Clusters

# Elbow Methos
fviz_nbclust(df, FUN = hcut, method = "wss")

# Average Silhouette Method
fviz_nbclust(df, FUN = hcut, method = "silhouette")

# Gap Statistic Method
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
