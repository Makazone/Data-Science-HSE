data = read.csv("data/data.csv", sep = ",")
attach(data) # helps to avoid writing data$Q everytime

boxplot(q~orbit_class,
        horizontal=TRUE,
        col=c("chocolate1","cyan2","violetred1"), # just take a look http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
        xlab="q"
)

# Tabular regression
rows = lapply(unique(orbit_class), 
              function(asteroidClass) {
                dataSet = data[orbit_class == asteroidClass,]
                c(
                  nrow(dataSet),
                  mean(dataSet$q),
                  sd(dataSet$q)
                  )
              })
# Reference row with every item
# rows[[length(rows)+1]] = c(nrow(data), mean(data$MOID), sd(data$MOID))

# Find correlation
sigma.w = sum(
  unlist(lapply(rows, 
                function(row) {
                  print(row)
                  row[1]*row[3]^2/nrow(data)           
                }))
)
correlation = 1 - (sigma.w) / sd(data$q)^2

# Part 2

mosaicplot(orbit_class~PHA, main="", xlab="Orbit Class")

cont.table = table(orbit_class, PHA)
print(cont.table)

norm.cont.table = cont.table / nrow(data)
print(norm.cont.table)

norm.row.sums = rowSums(norm.cont.table) 
norm.col.sums = colSums(norm.cont.table) 

size = dim(norm.cont.table)
ketleMatrix = matrix(1:length(norm.cont.table), nrow=size[1], ncol=size[2])
for (rowIndex in 1:size[1]) {
  for (colIndex in 1:size[2]) {
    ketleMatrix[rowIndex, colIndex] = norm.cont.table[rowIndex,colIndex] / (norm.row.sums[rowIndex] * norm.col.sums[colIndex]) - 1
  }
}
print(ketleMatrix)

max(ketleMatrix)

Q = sum(ketleMatrix*norm.cont.table)
print(Q)