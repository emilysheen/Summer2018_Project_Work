

### K-means clustering of ARI2 Installation Data
### Emily Sheen

path <- "C:/Users/eshee/Box Sync/Documents/Virginia Tech DSPG SDAL/R/ari_base_char/"
setwd(path)


library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(cluster)    # clustering algorithms
#install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization
#install.packages("ggmap")
library(ggmap)
#install.packages("gridExtra")
library(gridExtra)

#  Load Installation Data

ari2 <- read_csv("installation_data_product.csv")


# Step 1) Delete NA VALUES

map(ari2, ~sum(is.na(.)))  #counts NA values for each variable
length(ari2$base)   # length before NA deletion

ari2NotNA <- na.omit(ari2)
#remove columns with NA: Dan Liden found clustering results were similar between
# removing columns with NA values versus removing rows, and since we have few rows
# it is better to remove columns with NA values
colnames(ari2)
#ari2 <- subset(ari2, select = -c(violence, pupil_teacher_ratio, pct_65_older, annual_drug_spending_hh_avg))
length(ari2$base)

length(na.omit(ari2)$base)  #one base has NA value for region, but this is not used for clustering

# Step 2) Remove categorical/descriptive variables not to be used in clustering & scale variables

contNotNA <- subset(ari2NotNA, select = -c(base, STATE, REGION, lat, long)) #only cluster based on cont. vars.

length(contNotNA$population)
contNotNA <- scale(contNotNA)

# Step 3) Choose a distance measure
distance <- get_dist(contNotNA, method = "euclidean") #methods are euclidean, pearson, spearmen, etc.
fviz_dist(distance, show_labels = TRUE, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(contNotNA, centers = 2, nstart = 25)
str(k2)
k2$centers
k2$cluster
length(k2$cluster)

fviz_cluster(k2, data = contNotNA)

#add columns for base, state, region, lat, long, and cluster
clustered <- contNotNA %>%
  as_tibble() %>%
  mutate(kMeansCluster = k2$cluster,
         base = ari2NotNA$base,
         state = ari2NotNA$STATE,
         region = ari2NotNA$REGION,
         lat = ari2NotNA$lat,
         long = ari2NotNA$long)
colnames(clustered)

#plot of unemployment vs. obesity by clusters.  INTERESTING
ggplot(data = clustered, aes(unemployment_rate, obese, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of unemployment vs. income_below_poverty.  NOT INTERESTING
ggplot(data = clustered, aes(unemployment_rate, income_below_poverty, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of divorced vs. with_children_under_18.  NOT INTERESTING
ggplot(data = clustered, aes(divorced, with_children_under_18, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of armed_forces vs. veteran.  NOT INTERESTING
ggplot(data = clustered, aes(armed_forces, veteran, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of less_than_high_school vs. teen_birth.  MAYBE INTERESTING
ggplot(data = clustered, aes(less_than_high_school, teen_birth, color = factor(kMeansCluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)


#DAN LIDEN CODE TO PLOT CLUSTERS ON THE MAP

usa = map_data("usa")
states = map_data("state")
ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  geom_point(data = clustered,
             aes(x = long, y = lat, color = as.factor(kMeansCluster)),
             size = 2)


### TRY OTHER NUMBERS OF CLUSTERS

k3 <- kmeans(contNotNA, centers = 3, nstart = 25)
k4 <- kmeans(contNotNA, centers = 4, nstart = 25)
k5 <- kmeans(contNotNA, centers = 5, nstart = 25)
k6 <- kmeans(contNotNA, centers = 6, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "point", data = contNotNA) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = contNotNA) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = contNotNA) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = contNotNA) + ggtitle("k = 5")
p6 <- fviz_cluster(k5, geom = "point",  data = contNotNA) + ggtitle("k = 6")

grid.arrange(p2, p3, p4, p5, p6, nrow = 2)


#######################################################################
### To determine the appropriate number of clusters, we can use     ###
###   1) Elbow method: plot SSTOT by # clusters, choose k at bend   ###
###   2) Silhouette method                                         ###
###   3) Gap statistic                                              ###
#######################################################################

### ELBOW METHOD: plot the "within cluster sum of squared distances against the # clusters K

fviz_nbclust(contNotNA, kmeans, method = "wss", k.max = 15)  #Notice bend at elbow at 4-5 clusters

#  fvis_nbclust function is essentially mapping the number of clusters k
#  against the total within cluster sum of squared distances (sq. distance
#  between each observation and the cluster center)

#ELBOW METHOD BY HAND:
library(purrr)

# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(contNotNA, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Optimal number of clusters appears to be between 3-4 (when flattens)



### SILHOUETTE METHOD: silhouette method measures how well an observation lies within its cluster
# and how far apart it lies from other clusters.  A higher average silhouette value equates to
# more optimal number of clusters.  https://en.wikipedia.org/wiki/Silhouette_(clustering)

fviz_nbclust(contNotNA, kmeans, method = "silhouette")  #notice optimal silhouette at 2 clusters


#SILHOUETTE METHOD BY HAND
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(contNotNA, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(contNotNA))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


## Gap Statistic Method
# compute gap statistic
gap_stat <- clusGap(contNotNA, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50)
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)
# Gap statistic suggests 6 clusters is optimal

####  THE OPTIMAL CLUSTER K IS DIFFERENT FROM EACH METHOD

###   Assume 3 clusters is optimal based on silhouette and elbow methods

final <- kmeans(contNotNA, 3, nstart = 25)
print(final)

fviz_cluster(final, data = contNotNA)
str(final)
final$centers
final$cluster
length(final$cluster)

#Append final cluster information to dataset
#add columns for base, state, region, lat, long, and cluster

clustered <- contNotNA %>%
  as_tibble() %>%
  mutate(kMeansCluster = final$cluster,
         base = ari2NotNA$base,
         state = ari2NotNA$STATE,
         region = ari2NotNA$REGION,
         lat = ari2NotNA$lat,
         long = ari2NotNA$long)
colnames(clustered)
clustered$kMeansCluster

#DO PLOTS AGAIN ON FINAL CLUSTERS

#plot of unemployment vs. obesity by clusters.  INTERESTING
ggplot(data = clustered, aes(unemployment_rate, obese, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of unemployment vs. income_below_poverty.  NOT INTERESTING
ggplot(data = clustered, aes(unemployment_rate, income_below_poverty, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of divorced vs. with_children_under_18.  NOT INTERESTING
ggplot(data = clustered, aes(divorced, with_children_under_18, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of armed_forces vs. veteran.  NOT INTERESTING
ggplot(data = clustered, aes(armed_forces, veteran, color = factor(kMeansCluster), label = base)) +
  geom_text()

#plot of less_than_high_school vs. teen_birth.  MAYBE INTERESTING
ggplot(data = clustered, aes(less_than_high_school, teen_birth, color = factor(kMeansCluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)


#DAN LIDEN CODE TO PLOT CLUSTERS ON THE MAP

usa = map_data("usa")
states = map_data("state")
ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  geom_point(data = clustered,
             aes(x = long, y = lat, color = as.factor(kMeansCluster)),
             size = 2)


write.csv(clustered, file = "kMeans_clustered_installation_data.csv",
          row.names = FALSE)
