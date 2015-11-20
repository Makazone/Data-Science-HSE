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
