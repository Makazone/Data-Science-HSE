data = read.csv("data/data.csv", sep = ",")
attach(data) # helps to avoid writing data$Q everytime

# Scatter plot of all features, except PHA and discovery_date
pairs(~H+MOID+q+Q+period, data=data, main="Scatterplot Matrix")

# Looks like we have a strong correlation! Ya!
plot(Q, period, main="Scatterplot of Q vs period",  xlab="Q", ylab="period", pch=19)

# Now onto Y = aX + b
model = lm(period~Q)
regression = lm(period~Q)$coefficients
cat(sprintf("period = %f Q + %f\n", regression[2], regression[1]))

# Add bright red line to our scatter plot
abline(regression, col="red") # regression line (y~x) 

# How about mean absolute error?
MAE = sum(abs(lm(period~Q)$residuals))/70
cat(sprintf("MAE = %f\n", MAE))

# Coefficient of Determination
r = summary(model)$r.squared
cat(sprintf("Coefficient of Determination = %f\n", r))

# And finaly correlation of period and Q
cat(sprintf("Cov(period, Q) = %f\n", cor(Q, period)))