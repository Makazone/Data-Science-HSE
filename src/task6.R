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
km.out=kmeans(data, 3, nstart=20)
km.out$cluster

ggplot(data, aes(Area, Groove_g)) + 
  aes(shape = factor(full_data$Type, labels = c("Kama", "Rosa", "Canadian"))) +
  labs(shape = "Plant type") +
  geom_point(aes(colour = factor(km.out$cluster)), size = 4) +
  labs(colour = "Cluster ID") +
  geom_point(colour="grey90", size = 1.5) +
  ggtitle("Clustering Results")
