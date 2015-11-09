data = read.csv("data/data.csv", sep = ",")
# We select H (Magnitude) as our mama
selectedFeature = data$H

##### TASK 1 ######

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

# What if we delete outliers
outliers = boxplot(selectedFeature, plot=FALSE)$out
featureNoOutliers = selectedFeature[-match(outliers, selectedFeature)]

# and recompute some staff
Mode(featureNoOutliers)
median(featureNoOutliers)
mean(featureNoOutliers)
mean(Mode(featureNoOutliers))

##### TASK 2 ######

# Confidence interval
n <- nrow(data)
m <- mean(selectedFeature)
s <- sd(selectedFeature)
conf.int <- c(m - 1.965 * s / sqrt(n), m + 1.965 * s / sqrt(n))
cat(sprintf("CI = (%f; %f)\n", conf.int[1], conf.int[2]))

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
cat(sprintf("PCI = (%f; %f)\n", pivotal.conf.int[1], pivotal.conf.int[2]))

# nonpivotal
sorted.means  <- sort(means)
nonpivotal.conf.int <- c(sorted.means[0.025 * n], sorted.means[n - 0.025 * n])
cat(sprintf("NPCI = (%f; %f)\n", nonpivotal.conf.int[1], nonpivotal.conf.int[2]))


##### TASK 3 ######

n <- 5000
medians <- c()
for (i in 1:n) {
  medians[i] <- median(sample(selectedFeature, nrow(data), replace = TRUE))
}
hist(medians)

n <- 5000
sampledModes <- c()
for (i in 1:n) {
  modes = Mode(sample(selectedFeature, nrow(data), replace = TRUE))
  for (value in modes) {
    sampledModes <- c(sampledModes, value)
  }
}
hist(sampledModes)

sorted.medians  <- sort(medians)
nonpivotal.conf.int <- c(sorted.medians[0.025 * n], sorted.medians[n - 0.025 * n])
cat(sprintf("NPCI = (%f; %f)\n", nonpivotal.conf.int[1], nonpivotal.conf.int[2]))

sorted.Modes  <- sort(sampledModes)
nonpivotal.conf.int <- c(sorted.Modes[0.025 * n], sorted.Modes[n - 0.025 * n])
cat(sprintf("NPCI = (%f; %f)\n", nonpivotal.conf.int[1], nonpivotal.conf.int[2]))
