data = read.csv("data/sasha_data.csv", sep = ",")
attach(data) # helps to avoid writing data$Q everytime

# Exclude certain variables
#excludedVars = names(data) %in% c("Region") 
#data = data[!excludedVars]

includedVars = c("Age", "Immun", "CNS", "Milz", "Leber") 
data = data[includedVars]

Normalize = function(x) {
  y = (x - min(x))/(max(x) - min(x))
  y
}