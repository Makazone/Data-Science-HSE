full_data = read.csv("data/sasha_data.csv", sep = ",")
attach(full_data)

full_data = full_data[which(Leuc < 1000), ]

# Ну, у нас данные по детям, у которых лейкоз (назовём их ЛЕЙКОДЕТИ)
# Надо выбрать какой-то целевой признак, зависящий от других признаков, 
# чтобы можно было этот целевой предсказывать там, где он неизвестен
#
# У нас в данных есть признак Tod - Исход лечения: жив (0), мертв (1), выбыл из наблюдения (2) 
# [я мог ошибиться с цифрами 0, 1 и 2, но не суть]

# Ну, попробуем его объяснять признаками:
#
# Age - Возраст в момент постановки диагноза [кол]
# Leuc - Количество лейкоцитов на 1 нл крови [кол]
# Leber - Пальпируемый размер печени (см) [кол]
# Milz - Пальпируемый размер селезенки (см) [кол]

# Итак, Tod, я выбираю тебя! Это качественный признак, задаёт 2 класса
# ────────▄███████████▄────────
# ─────▄███▓▓▓▓▓▓▓▓▓▓▓███▄─────
# ────███▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓███────
# ───██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██───
# ──██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██──
# ─██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██─
# ██▓▓▓▓▓▓▓▓▓███████▓▓▓▓▓▓▓▓▓██
# ██▓▓▓▓▓▓▓▓██░░░░░██▓▓▓▓▓▓▓▓██
# ██▓▓▓▓▓▓▓██░░███░░██▓▓▓▓▓▓▓██
# ███████████░░███░░███████████
# ██░░░░░░░██░░███░░██░░░░░░░██
# ██░░░░░░░░██░░░░░██░░░░░░░░██
# ██░░░░░░░░░███████░░░░░░░░░██
# ─██░░░░░░░░░░░░░░░░░░░░░░░██─
# ──██░░░░░░░░░░░░░░░░░░░░░██──
# ───██░░░░░░░░░░░░░░░░░░░██───
# ────███░░░░░░░░░░░░░░░███────
# ─────▀███░░░░░░░░░░░███▀─────
# ────────▀███████████▀────────

# Выбираем только количественные признаки
includedVars = c("Age", "Leuc", "Leber", "Milz") 
data = full_data[includedVars]

normalize = function(x) {
  y = (x - min(x))/(max(x) - min(x))
  y
}

# Гы, прочитал задание. Да это же игрушечный машин лёрнинг на коленке, ёба!

# Нам нужно использовать два алгоритма, причём у нас объясняющие признаки и количественные, и качественные
# Заюзаем:
#   1. k Nearest Neighbours (на три класса)
#   2. Naive Bayes

# Метод номер раз
N = nrow(data)

target = full_data[c('Tod')] 
data   = as.data.frame(lapply(data, normalize))

set.seed(1228)
smp.size <- floor(0.65 * N)
train.id <- sample(c(1:N), size = smp.size)

data.train <- data[train.id,]
data.test <- data[-train.id,]
target.train  <- target[train.id]
target.test <- target[-train.id]

library(class)

target.predict = knn(data.train, data.test, target.train, k = 1)
table(target.predict, target.test)

target.predict <- knn(data.train, data.test, target.train, k = 3)
table(target.predict, target.test)

target.predict  <- knn(data.train, data.test, target.train, k = 5)
table(target.predict, target.test)

target.predict  <- knn(data.train, data.test, target.train, k = 7)
table(target.predict, target.test)

# Метод номер два
# install.packages('e1071')
library(e1071)

includedVars = c("Tod", "Age", "Leuc", "Leber", "Milz") 
data = full_data[includedVars]

# Надо перевести количественные признаки в качественные

# BEWARE, code ahead smells like ...
# TEEN SPIRIT
#
# Here we are now; entertain us
# I feel stupid and contagious
# Here we are now; entertain us
# A mulatto, an albino, a mosquito, my libido
# A denial, a denial, a denial, a denial, a denial
# A denial, a denial, a denial, a denial
#

# Age 
buckets = hist(data$Age, breaks=8)$breaks
data$Age_q = 0
for(i in 2:length(buckets)) {
  if (nrow(data[which(Age > buckets[i-1] & Age <= buckets[i]),]) > 0) {
    data[data$Age > buckets[i-1] & data$Age <= buckets[i],]$Age_q = i
  }
}
data$Age_q = as.factor(data$Age_q)
data[, "Age"] <- NULL

# Leuc
buckets = hist(data$Leuc, breaks=500)$breaks
data$Leuc_q = 0
for(i in 2:length(buckets)) {
  if (nrow(data[which(Leuc > buckets[i-1] & Leuc <= buckets[i]),]) > 0) {
    data[data$Leuc > buckets[i-1] & data$Leuc <= buckets[i],]$Leuc_q = i
  }
}
data$Leuc_q = as.factor(data$Leuc_q)
data[, "Leuc"] <- NULL

# Leber
buckets = hist(data$Leber)$breaks
data$Leber_q = 0
for(i in 2:length(buckets)) {
  if (nrow(data[which(Leber > buckets[i-1] & Leber <= buckets[i]),]) > 0) {
    data[data$Leber > buckets[i-1] & data$Leber <= buckets[i],]$Leber_q = i
  }
}
data$Leber_q = as.factor(data$Leber_q)
data[, "Leber"] <- NULL

# Milz
buckets = hist(data$Milz)$breaks
data$Milz_q = 0
for(i in 2:length(buckets)) {
  if (nrow(data[which(Milz > buckets[i-1] & Milz <= buckets[i]),]) > 0) {
    data[data$Milz > buckets[i-1] & data$Milz <= buckets[i],]$Milz_q = i
  }
}
data$Milz_q = as.factor(data$Milz_q)
data[, "Milz"] <- NULL

data$Tod = as.factor(data$Tod)

target = full_data[,"Tod"] 

model = naiveBayes(data$Tod ~ ., data = data[, 2:ncol(data)])
predict(model, data[1:10,])

pred = predict(model, data[1:10, ])
table(pred, target)

# P.S.: Все комментарии были написаны в невменяемом состоянии. Возможно, они помогут накрапать отчёт
