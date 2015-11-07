data = read.csv("data/data.csv", sep = ",")
# We select H (Magnitude) as our mama
selectedFeature = data$H

# Build histogram 
hist(selectedFeature, main = "Distribution of H", xlab = "H", breaks = 8)

# Build boxplot
boxplot(selectedFeature, main = "H (Magnitude)", horizontal = TRUE)

# Now we examine some characteristics (mode, mean, median)

# First, to compute mode
Mode = function(x) {
  ux = unique(x)
  tab = tabulate(match(x, ux))
  ux[tab == max(tab)]
}
Mode(selectedFeature)

# median and mean
median(selectedFeature)
mean(selectedFeature)

# Confidence interval
n <- nrow(data)
m <- mean(selectedFeature)
s <- sd(selectedFeature)
conf.int <- c(m - 1.965 * s / sqrt(n), m + 1.965 * s / sqrt(n))
cat(sprintf("CI = (%f; %f)", conf.int[1], conf.int[2]))

# Bootstrapping
# pivotal
n <- 5000
means <- c()
for (i in 1:n) {
  means[i] <- mean(sample(selectedFeature, nrow(data), replace = TRUE))
}
hist(means) # like a normal, bitch! to-do-do-do-to-do-do...
m <- mean(means)
s <- sd(means)
pivotal.conf.int <- c(m - 1.965 * s / sqrt(n), m + 1.965 * s / sqrt(n))
cat(sprintf("PCI = (%f; %f)", pivotal.conf.int[1], pivotal.conf.int[2]))

# nonpivotal
sorted.means  <- sort(means)
nonpivotal.conf.int <- c(sorted.means[0.025 * n], sorted.means[n - 0.025 * n])
cat(sprintf("NPCI = (%f; %f)", nonpivotal.conf.int[1], nonpivotal.conf.int[2]))
