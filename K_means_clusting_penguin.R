# K Means clustering (군집분석)
# Kmeans Clustering of Penguins
# https://www.r-bloggers.com/2020/09/kmeans-clustering-of-penguins-2/
# 분류의 갯수를 결정하는 방법에 대한 코드


rm(list=ls())

library(tidyverse)
library(janitor)
library(palmerpenguins)
library(knitr)

# 1. Source the Data

penguins_raw <- read_csv(path_to_file("penguins_raw.csv")) %>%
  clean_names()

opts_chunk$set(warning = FALSE, message = FALSE)


# 2. Exploratory Data Analysis

library(skimr)
skim (penguins_raw)


library (GGally)

ggpairs(
  data = penguins_raw,
  columns = c(10:14),
  diag = list(continuous = wrap("barDiag", color = "blue", size =4)),
  upper = list(continuous = wrap("cor", size = 4, bins = 60))
)


# 3. Data wrangling

penguins <- penguins_raw %>%
  rename (
    bill_length = culmen_length_mm,
    bill_depth = culmen_depth_mm,
    flipper_length = flipper_length_mm,
    body_mass = body_mass_g
  ) %>%
  mutate (
    id = row_number(),
    species = word (species, 1),
    bill_length = scale(bill_length),
    bill_depth = scale(bill_depth),
    flipper_length = scale(flipper_length)
  ) %>%
  select (id, species, island, sex, bill_length, bill_depth, flipper_length, body_mass) %>%
  drop_na (sex)


# 4. Principal component analysis

library(factoextra)
library(FactoMineR)

penguins_PCA <-PCA(penguins[5:7], graph = F)
fviz_screeplot(penguins_PCA)


fviz_pca_biplot(penguins_PCA, geom = "point") +
  geom_point (alpha = 0.2)


# 5. Identify optimal number of clusters

# Method 1 - Visual Inspection

library(patchwork)
library(glue)
library(here)

kmeans_flex <- function (k) {
  penguins_kmeans <- kmeans(penguins[5:7], k) 
  fviz_cluster(penguins_kmeans, geom = "point", data = penguins[5:7]) +
    labs(title = glue("{k} clusters")) +
    theme (
      plot.background = element_blank(),
      panel.background = element_blank(),plot.title = element_text (margin = margin(0,0,5,0), hjust = 0.5, size = 12, color = "grey", family = "Lato"),
      legend.text = element_text(hjust = 0, size = 8, family = "Lato"),
      legend.position = "none",
      legend.title = element_text(size = 8),
      axis.title = element_text (size = 8),
      axis.text = element_text (size = 8)
    )
}

cluster_possibles <- map (1:9, kmeans_flex)

cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "Kmeans Clustering of Penguins across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: Joel Soroos @soroosj  |  Data: R palmerpenguins package via R4DS Tidy Tuesday",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0)) 
    )
  )

methodologies <- c("wss", "silhouette", "gap_stat")

cluster_optimal <- map (methodologies, ~fviz_nbclust (penguins[5:7], kmeans, method = .x))

summary(cluster_optimal)  # 3개로 분류하는 것이 최적이라는 것을 보여줌.

# Method 2 - Elbow

cluster_optimal[[1]]


# Method 3 - Silhouette

cluster_optimal[[2]]


# Method 4 - Gap Statistic

cluster_optimal[[3]]


# Method 5: Multiple indexes

library (NbClust)

cluster_30_indexes <- NbClust(data = penguins[5:7], distance = "euclidean", min.nc = 2, max.nc = 9, method = "complete", index ="all")


fviz_nbclust(cluster_30_indexes) +
  theme_minimal() +
  labs(title = "Frequency of Optimal Clusters using 30 indexes in NbClust Package")


