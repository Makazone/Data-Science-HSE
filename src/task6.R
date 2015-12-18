# Before running, make sure those guys are onboard
# install.packages(c("e1071","class","ggplot2"))

library(e1071)
library(class)
library(ggplot2)

# Norm functions
normalize = function(x) {
  y = (x - min(x))/(max(x) - min(x))
  y
}

# Norm function to bring data to (0,0)
# Used in iK-means and Anomalous Pattern
normalizeByMean = function(x) {
  y = (x - mean(x))/(max(x) - min(x))
  y
}

# Alright, guess we can start rolling!
# ─────────────NYA────────────────────────
# ───▐▀▄───────▄▀▌───▄▄▄▄▄▄▄─────────────
# ───▌▒▒▀▄▄▄▄▄▀▒▒▐▄▀▀▒██▒██▒▀▀▄──────────
# ──▐▒▒▒▒▀▒▀▒▀▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▀▄────────
# ──▌▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▄▒▒▒▒▒▒▒▒▒▒▒▒▀▄──────
# ▀█▒▒▒█▌▒▒█▒▒▐█▒▒▒▀▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▌─────
# ▀▌▒▒▒▒▒▒▀▒▀▒▒▒▒▒▒▀▀▒▒▒▒▒▒▒▒▒▒▒▒▒▒▐───▄▄
# ▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▌▄█▒█
# ▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒█▒█▀─
# ▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒█▀───
# ▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▌────
# ─▌▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▐─────
# ─▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▌─────
# ──▌▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▐──────
# ──▐▄▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▄▌──────
# ────▀▄▄▀▀▀▀▀▄▄▀▀▀▀▀▀▀▄▄▀▀▀▀▀▄▄▀────────

# Prep data

full_data = read.table("data/seeds_dataset.txt")
# 1. area A, 
# 2. perimeter P, 
# 3. compactness C = 4*pi*A/P^2, 
# 4. length of kernel, 
# 5. width of kernel, 
# 6. asymmetry coefficient 
# 7. length of kernel groove. 
# 8. Type = Kama, Rosa and Canadian
colnames(full_data) = c("Area", "Perimeter", "Compactness", "Length_k", "Width_k", "Asymmetry", "Groove_g", "Type")

# Since Compactness can be computed from Area and Perimeter, we exclude it 
full_data = subset(full_data, select = -Compactness)

############### SIMPLE K-MEANS ###############

data = subset(full_data, select = -Type)
data = as.data.frame(lapply(data, normalize))
km.out=kmeans(data, 4, nstart=20)
#km.out$cluster

ggplot(data, aes(Area, Groove_g)) + 
  aes(shape = factor(full_data$Type, labels = c("Kama", "Rosa", "Canadian"))) +
  labs(shape = "Plant type") +
  geom_point(aes(colour = factor(km.out$cluster)), size = 4) +
  labs(colour = "Cluster ID") +
  geom_point(colour="grey90", size = 1.5) +
  ggtitle("Clustering Results")





############### ANOMALOUS PATTERN METHOD ###############

# @param data - data.frame
# @return (S_k, c_k) the anamalous cluster list and its center
AnomalousPattern = function(data, normalized = TRUE) {
  if (!normalized) {
    data = as.data.frame(lapply(data, normalizeByMean))
  }
  
  # Take 0 point as a fixed center
  a = rep(0,ncol(data)) 
  
  # Initial setting: Put a tentative centroid, c, as the entity farthest away from the origin, 0.
  c = c()
  minDist = Inf
  for (i in 1:nrow(data)) {
    d = dist(rbind(data[i,], a))
    if (minDist > d) {
      c = data[i,]
      minDist = d
    }
  }
  
  # Furthest point 
  # Used for DEBUG ONLY
  c.init = c(c)
  
  c.prev = c()
  for (i in 1:50) {
    S = c()
    # Cluster update: Determine cluster list S around c against the only other “centroid” 0
    # so that entity yi is assigned to S if d(yi , c) < d(yi , 0).
    for (i in 1:nrow(data)) {
      d.c    = dist(rbind(data[i,], c))  
      d.zero = dist(rbind(data[i,], a))
      if (d.c < d.zero) {
        S <- c(S, i)
      } 
    }
    
    # c.prev = c
    c = as.data.frame(lapply(data[S, ], mean))
  }
  
  return(list(S, c))
}

################## TEST Anomalous pattern ##################
data = subset(full_data, select = -Type)
data = as.data.frame(lapply(data, normalizeByMean))
result = AnomalousPattern(data[, c("Area", "Groove_g")])
S = result[[1]]
c = rbind(result[[2]])
data[,"Cluster"] = 1
data[S, "Cluster"] = 2

ggplot(data, aes(Area, Groove_g)) + 
  aes(shape = factor(full_data$Type, labels = c("Kama", "Rosa", "Canadian"))) +
  labs(shape = "Plant type") +
  geom_point(aes(colour = factor(data[,"Cluster"])), size = 4) +
  labs(colour = "Cluster ID") +
  geom_point(colour="grey90", size = 1.5) +
  ggtitle("Clustering Results")
################## TEST Anomalous pattern ##################


################## iK-means ##################

# ------------------------------- #
# iK-means #
data = subset(full_data, select = -Type)
data = as.data.frame(lapply(data, normalizeByMean))

DataScatter = function(cluster) {
  return(Reduce("+", cluster^2))
}

# Setting: Preprocess and standardize the dataset. Take t as the threshold of resolution. 
# Put k = 1 and Ik = I, the original entity set.
I = data
k = 1
K_max = 50
minClusterSize = 5

S = list()

initDataScatter = DataScatter(data)

while (TRUE) {
  # Anomalous pattern: Apply AP to Ik to find k-th anomalous pattern Sk and its centroid ck.
  anamResults = AnomalousPattern(I)
  S[[length(S)+1]] = anamResults[[1]]
  
  if (k == 1) {
    c = rbind(anamResults[[2]])
  } else {
    c = rbind(c, anamResults[[2]])
  }
  
  # Test: If Stop-condition does not hold: 
  #   - All of I has been clustered, S.k = I.k;
  if (identical(data[S[[length(S)]], ], data)) {
    length(S[[length(S)]])
    print("All of I has been clustered")
    break
  }
  
  #   - The total contribution of the first k clusters to the data scatter T has
  #     reached a pre-specified threshold such as 50 %.
  totalContribution = 0
  for (i in 1:length(S)) {
    totalContribution = totalContribution + DataScatter(data[S[[i]],])
  }
  if (totalContribution / initDataScatter > 0.8) {
    print("Data scatter of first k clusters is greater 50")
    break
  }
  
  print("1")
  
  #   - Contribution of Sk is too small; for example, it is comparable with the
  #     average contribution of a single entity, T/N, where T is the data scatter.
  averageContribution = initDataScatter / nrow(data)
  if (abs(DataScatter(S[[length(S)]]) - averageContribution) < 0.1) {
    print("Small contribution")
    break
  }
  print("2")
  #   - Number of clusters k has reached its pre-specified value K.
  # remove S.k from I, make k +=1 and I = I − S.k, after which step 1 is executed again. 
  if (k > K_max) {
    print("k max reached")
    break
  }
  
  print("3")
  # Otherwise proceed 
  
  #I = as.data.frame(lapply(I, "[", -S[[length(S)]]))
  I = I[-S[[length(S)]], ]
  k = k + 1
  
  if (length(I) == 0) {
    break
  }
  print(k)
}

# Discarding small clusters: 
# Remove all of the found clusters containing t entities or less. 
# Denote the number of remaining clusters by K and re-label them so that their centroids are c1, c2, ..., cK
smallClusterIndecies = c()
for (i in 1:length(S)) {
  if (length(S[[i]]) < minClusterSize) {
    smallClusterIndecies = c(smallClusterIndecies, i) 
  }
}

if (!is.null(smallClusterIndecies)) {
  c = c[-smallClusterIndecies, ]
}

# Do K-Means using c1, c2, ..., cK as initial seeds.
km.out = kmeans(data, c)
# km.out$cluster

ggplot(data, aes(Area, Groove_g)) + 
  aes(shape = factor(full_data$Type, labels = c("Kama", "Rosa", "Canadian"))) +
  labs(shape = "Plant type") +
  geom_point(aes(colour = factor(km.out$cluster)), size = 4) +
  labs(colour = "Cluster ID") +
  geom_point(colour="grey90", size = 1.5) +
  ggtitle("iK-means Clustering Results")



