library(readr)
sect8c_plantingw3 <- read_csv("Desktop/sect8c_plantingw3.csv")
View(sect8c_plantingw3)

health_expense <- sect8c_plantingw3 %>%
  select(state, item_desc, s8q6)%>%
  filter(item_desc %in% "HEALTH EXPENDITURES (EXCLUDING") %>%
  select(1,3) %>%
  na.omit() %>%
  group_by(state) 

 health_expense_mean <- summarize(health_expense, mean_health_expenditure = mean(s8q6))
 
 health_expense_mean <- health_expense_mean %>%
   mutate(state_names = case_when(
     state == 1 ~ "Abia",
     state == 2 ~ "Adamawa",
     state == 3 ~ "Akwa Ibom",
     state == 4 ~ "Anambra",
     state == 5 ~ "Bauchi",
     state == 6 ~ "Bayelsa",
     state == 7 ~ "Benue",
     state == 8 ~ "Borno", 
     state == 9 ~ "Cross River", 
     state == 10 ~ "Delta", 
     state == 11 ~ "Ebonyi", 
     state == 12 ~ "Edo",
     state == 13 ~ "Ekiti", 
     state == 14 ~ "Enugu", 
     state == 15 ~ "Gombe", 
     state == 16 ~ "Imo", 
     state == 17 ~ "Jigawa", 
     state == 18 ~ "Kaduna", 
     state == 19 ~ "Kano", 
     state == 20 ~ "Katsina", 
     state == 21 ~ "Kebbi",
     state ==22 ~ "Kogi", 
     state == 23 ~ "Kwara", 
     state == 24 ~ "Lagos", 
     state == 25 ~ "Nasarawa",
     state == 26 ~ "Niger", 
     state == 27 ~ "Ogun", 
     state == 28 ~ "Ondo", 
     state == 29 ~ "Osun", 
     state == 30 ~ "Oyo",
     state == 31 ~ "Plateau", 
     state == 32 ~ "Rivers", 
     state == 33 ~ "Sokoto", 
     state == 34 ~ "Taraba",
     state == 35 ~ "Yobe",
     state == 36 ~ "Zamfara", 
     state == 37 ~ "FCT Abuja")
   ) %>%
   select(-1)

 ## scale the data
 health_expense_scaled <- scale(health_expense_mean[,1])
 
 ## cal dissimilarity matrix
 diss_health_expense_scaled <- dist(health_expense_scaled, method = "euclidean")
 
## hierarchical clustering wtih complete linkage
hcl_health_expense_scaled <-  hclust(diss_health_expense_scaled, method = "complete")
 
plot(hcl_health_expense_scaled, hang = .2, cex = 0.7) 



## agnes clustering
agnes_hcl_health_expense_scaled <- agnes(health_expense_scaled, method = "complete")
str(agnes_hcl_health_expense_scaled)
agnes_hcl_health_expense_scaled$ac

## identify HC method with stronger clustering structure
hc_method <- c("average", "single", "complete", "ward")
names(hc_method) <- c("average", "single", "complete", "ward")

## function to compute agglomerative coefficient for various methods
## Higher agglomerative coefficient (ac) indicate higher clustering structure
agglom_coeff <- function(x){
  agnes(health_expense_scaled, method = x)$ac
}
## apply function
map_dbl(hc_method, agglom_coeff)

### agnes clustering with ward which had highest ac
#visualize dendrogram
agnes_ward_health_expense_scaled <- agnes(health_expense_scaled, method = "ward")
pltree(agnes_ward_health_expense_scaled, cex = 0.6, hang = .1, main = "Dendrogram of agnes")



#### divisive clustering
# compute divisive hierarchical clustering
div_cluster_health_expense <- diana(health_expense_scaled)

# divisive coeffieict amount of clustering structure found
div_cluster_health_expense$dc

#plot dendrogram for divisive clustering
pltree(div_cluster_health_expense, cex = 0.6, hang = .2, main = "Dendrogram of diana")

## identify subgroups
# wards method
ward_hcl_health_expense <- hclust(diss_health_expense_scaled, method = "ward.D2")

# CUT TREE INTO 4 GROUPS
sub_grp_health_expense <- cutree(ward_hcl_health_expense, k = 6)

### Number of members in each cluster
table(sub_grp_health_expense)

## cluster to data
health_expense_mean%>%
  mutate(cluster = sub_grp_health_expense) %>%
  head
## dendrogram with borders around 4 clusters
plot(ward_hcl_health_expense, cex = 0.6)
rect.hclust(ward_hcl_health_expense, k = 6, border = 2:5)

## visualize with fviz_cluster
fviz_cluster(list(data = health_expense_scaled, cluster = sub_grp_health_expense))

#### using cutree with agnes and diana
# Cut agnes() tree into 4 groups
cutree(as.hclust(agnes_ward_health_expense_scaled), k = 4)

## cut diana() into 4 groups
cutree(as.hclust(div_cluster_health_expense), k = 4)

### computing and comparing dendrograms
## We have already computed the dissimilarity matrix and hierarchical clustering for two
# two methods namely complete and ward.D2. Now we can create dendrogram objects for them and
# plot them together to compare
# create two dendrograms
complete_dendrogram <- as.dendrogram(hcl_health_expense_scaled)
ward.D2_dendrogram <- as.dendrogram(ward_hcl_health_expense)

### plot the dendrograms to compare and match
tanglegram(complete_dendrogram, ward.D2_dendrogram)


#### customizing the dendrogram
# First we can chain multiple dendrograms together to customize them easily
dend_list_health_expense <- dendlist(complete_dendrogram, ward.D2_dendrogram)

tanglegram(complete_dendrogram, ward.D2_dendrogram,
           highlight_distinct_edges = FALSE, # Turn off dashed lines
           common_subtrees_color_lines = FALSE, # turn-off line colors
           common_subtrees_color_branches = TRUE, # color common branches
           main = paste("entanglement = ", round(entanglement(dend_list_health_expense), 2)))

## elbow method of determing optimal clusters
fviz_nbclust(health_expense_scaled, FUN = hcut, method = "wss")

## average silhouette method
fviz_nbclust(health_expense_scaled, FUN = hcut, method = "silhouette")

## gap statistics method
gap_stat_health_expense <- clusGap(health_expense_scaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat_health_expense)




View(health_expense_mean)


View(health_expense_scaled)
