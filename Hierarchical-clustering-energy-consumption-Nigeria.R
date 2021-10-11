library(readr)
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(stats)


sect8b_plantingw3 <- read_csv("Desktop/sect8b_plantingw3.csv")
#View(sect8b_plantingw3)
non_food_exp <- sect8b_plantingw3
str(non_food_exp)
summary(non_food_exp)

glimpse(non_food_exp)

energy_cons <- non_food_exp %>%
  select(zone, item_desc, s8q4) %>%
  filter(item_desc %in% c("KEROSENE", "PETROL", "FIREWOOD", "ELECTRICITY"))%>%
  na.omit() 

(mean_energy_per_zone <-  energy_cons %>%
  pivot_wider(names_from = item_desc, values_from = s8q4, values_fn = mean) )

mean_energy_per_zone <- mean_energy_per_zone %>%
  mutate(geopolitical_zone = case_when(
    zone == 1 ~ "North central",
    zone == 2 ~ "North east",
    zone == 3 ~ "North west",
    zone == 4 ~ "South east",
    zone == 5 ~ "South south",
    zone == 6 ~ "South west"
  )) %>%
  select(-1)

View(mean_energy_per_zone)


### scaling the data
mean_energy_scale <- scale(mean_energy_per_zone[,c(1:4)])
head(mean_energy_scale)

#### Dissimilarity matrix
d_mean_energy_scale <- dist(mean_energy_scale, method = "euclidean")

## hierarchical clustering using complete linkage
hc1_mean_energy_scale <- hclust(d_mean_energy_scale, method = "complete")

## plot the obtained dendrogram
plot(hc1_mean_energy_scale, cex = 0.6, hang = -1)



## agnes clustering
hc2_mean_energy_scale <- agnes(mean_energy_scale, method = "complete")
str(hc2_mean_energy_scale)
hc2_mean_energy_scale$ac

## identify HC method with stronger clustering structure
m_energy <- c("average", "single", "complete", "ward")
names(m_energy) <- c("average", "single", "complete", "ward")

## function to compute coefficient
ac_energy <- function(x){
  agnes(mean_energy_scale, method = x)$ac
}
## apply function
map_dbl(m_energy, ac_energy)

### visualize dendrogram
hc3_mean_energy_scale <- agnes(mean_energy_scale, method = "ward")
pltree(hc3_mean_energy_scale, cex = 0.6, hang = -1, main = "Dendrogram of agnes")



#### divisive clustering
# compute divisive hierarchical clustering
hc4_mean_energy_scale <- diana(mean_energy_scale)

# divisive coeffieict amount of clustering structure found
hc4_mean_energy_scale$dc

#plot dendrogram
pltree(hc4_mean_energy_scale, cex = 0.6, hang = -1, main = "Dendrogram of diana")

## identify subgroups
# wards method
hc5_mean_energy_scale <- hclust(d_mean_energy_scale, method = "ward.D2")

# CUT TREE INTO 4 GROUPS
sub_grp_energy <- cutree(hc5_mean_energy_scale, k = 2)

### Number of members in each cluster
table(sub_grp_energy)

## cluster to data
mean_energy_per_zone%>%
  mutate(cluster = sub_grp_energy) %>%
  head
## dendrogram with borders around 4 clusters
plot(hc5_mean_energy_scale, cex = 0.6)
rect.hclust(hc5_mean_energy_scale, k = 4, border = 2:5)

## visualize with fviz_cluster
fviz_cluster(list(data = mean_energy_scale[, 1:4], cluster = sub_grp_energy))

#### using cutree with agnes and diana
# Cut agnes() tree into 4 groups
hc_a_mean_energy <- agnes(mean_energy_scale, method = "ward")
cutree(as.hclust(hc_a_mean_energy), k = 4)

## cut diana() into 4 groups
hc_d_mean_energy <- diana(mean_energy_scale)
cutree(as.hclust(hc_d_mean_energy), k = 3)


### computing and comparing two dendrograms
# compute distance measure
res.dist_mean_energy <- dist(mean_energy_scale, method = "euclidean")

# compute 2 hierarchical clusterings
hc1_mean_energy_scale1 <- hclust(res.dist_mean_energy, method = "complete")
hc2_mean_energy_scale1 <- hclust(res.dist_mean_energy, method = "ward.D2")

# create two dendrograms
dend1_mean_energy <- as.dendrogram(hc1_mean_energy_scale1)
dend2_mean_energy <- as.dendrogram(hc2_mean_energy_scale1)

###
tanglegram(dend1_mean_energy, dend2_mean_energy)


#### customizing the dendrogram
dend_list_mean_energy <- dendlist(dend1_mean_energy, dend2_mean_energy)

tanglegram(dend1_mean_energy, dend2_mean_energy,
           highlight_distinct_edges = FALSE, # Turn off dashed lines
           common_subtrees_color_lines = FALSE, # turn-off line colors
           common_subtrees_color_branches = TRUE, # color common branches
           main = paste("entanglement = ", round(entanglement(dend_list_mean_energy), 2)))

## elbow method of determing optimal clusters
fviz_nbclust(mean_energy_scale, FUN = hcut, method = "wss", k.max = 4)

## average silhouette method
fviz_nbclust(mean_energy_scale[,1:4], FUN = hcut, method = "silhouette", k.max = 4)

## gap statistics method
gap_stat_mean_energy <- clusGap(mean_energy_scale, FUN = hcut, nstart = 25, K.max = 4, B = 50)
fviz_gap_stat(gap_stat_mean_energy)

