library(e1071)
library(class)
library(ggplot2)

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

data = subset(full_data, select = -Type)

normalize = function(x) {
  y = (x - min(x))/(max(x) - min(x))
  y
}

data = as.data.frame(lapply(data, normalize))

# -------------------- #
km.out=kmeans(data, 4, nstart=20)
#km.out$cluster

ggplot(data, aes(Area, Groove_g)) + 
  aes(shape = factor(full_data$Type, labels = c("Kama", "Rosa", "Canadian"))) +
  labs(shape = "Plant type") +
  geom_point(aes(colour = factor(km.out$cluster)), size = 4) +
  labs(colour = "Cluster ID") +
  geom_point(colour="grey90", size = 1.5) +
  ggtitle("Clustering Results")

# Anomalous pattern method #
data = subset(full_data, select = -Type)

# @param data - data.frame
# @return (S_k, c_k) the anamalous cluster list and its center
AnomalousPattern = function(data) {
  # 1. Pre-processing: Specify a reference point a = (a1, ..., aV ) (when in doubt, take a to be the data grand mean) 
  a = as.data.frame(lapply(data, mean))
  # and standardize the original data table by shifting the origin to a = (a1 , ..., aV ).      
  # __TODO__ Do some shit 
  
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
  
  c.init = c(c)
  
  c.prev = c()
  for (i in 1:20) {
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

result = AnomalousPattern(data)
S = result[[1]]
c = rbind(result[[2]])

# ------------------------------- #
# iK-means #
data = subset(full_data, select = -Type)

# Setting: Preprocess and standardize the dataset. Take t as the threshold of resolution. 
# Put k = 1 and Ik = I, the original entity set.
I = data
k = 1
t = 10

while (TRUE) {
  # Anomalous pattern: Apply AP to Ik to find k-th anomalous pattern Sk and its centroid ck.
  anamResults = AnomalousPattern(I)
  S = anamResults[[1]]
  
  if (k == 1) {
    c = rbind(anamResults[[2]])
  } else {
    c = rbind(c, anamResults[[2]])
  }
  
  # Test: If Stop-condition does not hold: 
  #   - All of I has been clustered, S.k = I.k;
  #   - The total contribution of the first k clusters to the data scatter T has
  #     reached a pre-specified threshold such as 50 %.
  #   - Contribution of Sk is too small; for example, it is comparable with the
  #     average contribution of a single entity, T/N, where T is the data scatter.
  #   - Number of clusters k has reached its pre-specified value K.
  # remove S.k from I, make k +=1 and I = I − S.k, after which step 1 is executed again. 
  # Otherwise proceed 
  
  if (identical(data[S, ], data) | length(S) < t | k > 3) {
    break
  }
  
  I = as.data.frame(lapply(I, "[", -S))
  k = k + 1
  
  if (length(I) == 0) {
    break
  }
}

# Discarding small clusters: 
# Remove all of the found clusters containing t entities or less. 
# Denote the number of remaining clusters by K and re-label them so that their centroids are c1, c2, ..., cK

# __TODO__

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



