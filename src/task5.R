library(e1071)
library(class)

# Все любят котиков!
#   /\     /\
#  {  `---'  }
#  {  O   O  }  
#~~|~   V   ~|~~  
#   \  \|/  /   
#    `-----'__
#    /     \  `^\_
#   {       }\ |\_\_   W
#   |  \_/  |/ /  \_\_( )
#    \__/  /(_E     \__/
#      (  /
#       MM

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

target = full_data[, 'Tod'] 
data   = as.data.frame(lapply(data, normalize))

set.seed(1228)
smp.size <- floor(0.65 * N)
train.id <- sample(c(1:N), size = smp.size)

data.train <- data[train.id,]
data.test <- data[-train.id,]
target.train  <- target[train.id]
target.test <- target[-train.id]

target.predict = knn(data.train, data.test, target.train, k = 1)
table(target.predict, target.test)

target.predict <- knn(data.train, data.test, target.train, k = 3)
table(target.predict, target.test)

target.predict  <- knn(data, data, full_data[, "Tod"], k = 3)
table(target.predict, full_data$Tod)

target.predict  <- knn(data.train, data.test, target.train, k = 7)
table(target.predict, target.test)

# ----------------- №3 - start -------------------
# kNN: k - number of neighbours considered

N = nrow(data)

target = full_data[, 'Tod'] 
data   = as.data.frame(lapply(data, normalize))

set.seed(1228)
smp.size <- N
train.id <- sample(c(1:N), size = smp.size)

data.train <- data[train.id,]
data.test <- data[-train.id,]
target.train  <- target[train.id]
target.test <- target[-train.id]

target.predict = knn(data.train, data, target, k = 1)
table(target.predict, target)
#               target
# target.predict    0    1    2
#              0 1798   19  177
#              1   17    1    2
#              2  179    0   21
# 0.9017051153 = P_1
# 0.05 = P_2
# 0.105 = P_3
# 0.9017051153 = R_1
# 0.05 = R_2
# 0.105 = R_3
# 
# P_avg = 0.3522350384
# R_avg = 0.3522350384
# F = 0.3522350384

target.predict <- knn(data.train, data, target, k = 3)
table(target.predict, target)
#               target
# target.predict    0    1    2
#              0 1933   20  195
#              1    1    0    0
#              2   60    0    5
# 0.8999068901 = P_1
# 0 = P_2
# 0.07692307692 = P_3
# 0.9694082247 = R_1	
# 0 = R_2
# 0.025 = R_3
# 
# P_avg = 0.325609989
# R_avg = 0.3314694082
# F = 0.3285135733

target.predict  <- knn(data, data, full_data[, "Tod"], k = 3)
table(target.predict, full_data$Tod)
# target.predict    0    1    2
#              0 1978   17  159
#              1    0    2    1
#              2   16    1   40
# 0.9182915506 = P_1
# 0.6666666667 = P_2
# 0.701754386 = P_3
# 0.9919759278 = R_1
# 0.1 = R_2
# 0.2 = R_3
# 
# P_avg = 0.7622375344
# R_avg = 0.4306586426
# F = 0.5503650498

target.predict  <- knn(data.train, data, target, k = 7)
table(target.predict, target)
#               target
# target.predict    0    1    2
#              0 1994   20  199
#              1    0    0    0
#              2    0    0    1
# 0.9010393131 = P_1
# N/A = P_2
# 1 = P_3
# 1 = R_1
# 0 = R_2
# 0.005 = R_3
# 
# P_avg = 0.633679771
# R_avg = 0.335
# F = 0.4382928799

# ----------------- №3 - end -------------------

# ----------------- №4 - start -------------------
N = nrow(data)

target = full_data[, 'Tod'] 
data   = as.data.frame(lapply(data, normalize))

errorMatrix = matrix(0, 3, 3)
for (i in 1:N) {
  train.id = i
  
  data.train = data[-train.id,]
  data.test  = data[train.id,]
  
  target.train = target[-train.id]
  target.test  = target[train.id]
  
  target.predict = knn(data.train, data.test, target.train, k = 3)
  t1 = as.numeric(target.predict[1])
  t2 = as.numeric(target.test[1])+1
  errorMatrix[t1, t2] = errorMatrix[t1, t2] + 1
}
errorMatrix
# ----------------- №4 - end -------------------

# Метод номер два
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
data$Age_q = 2
for(i in 2:length(buckets)) {
  if (nrow(data[which(Age > buckets[i-1] & Age <= buckets[i]),]) > 0) {
    data[which(Age > buckets[i-1] & Age <= buckets[i]),]$Age_q = i
  }
}
data$Age_q = as.factor(data$Age_q)
data[, "Age"] <- NULL

# Leuc
buckets = hist(data$Leuc, breaks=100)$breaks
data$Leuc_q = 2
for(i in 2:length(buckets)) {
  if (nrow(data[which(Leuc > buckets[i-1] & Leuc <= buckets[i]),]) > 0) {
    data[which(Leuc > buckets[i-1] & Leuc <= buckets[i]),]$Leuc_q = i
#    data[data$Leuc > buckets[i-1] & data$Leuc <= buckets[i],]$Leuc_q = i
  }
}
data$Leuc_q = as.factor(data$Leuc_q)
data[, "Leuc"] <- NULL

# Leber
buckets = hist(data$Leber)$breaks
data$Leber_q = 2
for(i in 2:length(buckets)) {
  if (nrow(data[which(Leber > buckets[i-1] & Leber <= buckets[i]),]) > 0) {
    data[which(Leber > buckets[i-1] & Leber <= buckets[i]),]$Leber_q = i
  }
}
data$Leber_q = as.factor(data$Leber_q)
data[, "Leber"] <- NULL

# Milz
buckets = hist(data$Milz)$breaks
data$Milz_q = 2
for(i in 2:length(buckets)) {
  if (nrow(data[which(Milz > buckets[i-1] & Milz <= buckets[i]),]) > 0) {
    data[which(Milz > buckets[i-1] & Milz <= buckets[i]),]$Milz_q = i
  }
}
data$Milz_q = as.factor(data$Milz_q)
data[, "Milz"] <- NULL

data$Tod = as.factor(data$Tod)

target = full_data[,"Tod"] 

model = naiveBayes(data$Tod ~ ., data = data[, 2:ncol(data)])
predict(model, data)

pred = predict(model, data)
table(pred, data[, "Tod"])

# ----------------- №3 - start -------------------
# pred    0    1    2
#    0 1992   18  195
#    1    0    2    0
#    2    2    0    5
# 0.9034013605 = P_1
# 1 = P_2
# 0.7142857143 = P_3
# 0.998996991 = R_1
# 0.1 = R_2
# 0.025 = R_3
# 
# P_avg = 0.8725623583
# R_avg = 0.3746656637
# F = 0.5242331784

# ----------------- №3 - end -------------------

# ----------------- №4 - start -------------------

# Don't forget to run code from previous step to convert data to factors 
N = nrow(data)

errorMatrix = matrix(0, 3, 3)
for (i in 1:N) {
  train.id = i
  
  data.train = data[-train.id,]
  data.test  = data[train.id,]
  
  target.train = data[-train.id]
  target.test  = target[train.id]
  
  model = naiveBayes(data.train$Tod ~ ., data = data.train[, 2:ncol(data.train)])
  pred  = predict(model, data.test)
  #table(pred, data[, "Tod"])

  t1 = as.numeric(target.predict[1])
  t2 = as.numeric(target.test[1])+1
  errorMatrix[t1, t2] = errorMatrix[t1, t2] + 1
}
errorMatrix
# ----------------- №4 - end -------------------

# P.S.: Все комментарии были написаны в невменяемом состоянии. Возможно, они помогут накрапать отчёт
