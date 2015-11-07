data = read.csv("data/data.csv", sep = ",")
# We select H (Magnitude) as our ma
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

# and the rest

median(selectedFeature)
mean(selectedFeature)