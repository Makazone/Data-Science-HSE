full_data <- read.csv("data/sasha_data.csv", sep = ",")

# Ну, у нас данные по детям, у которых лейкоз (назовём их ЛЕЙКОДЕТИ)
# Надо выбрать какой-то целевой признак, зависящий от других признаков, 
# чтобы можно было этот целевой предсказывать там, где он неизвестен
#
# У нас в данных есть признак Tod - Исход лечения: жив (0), мертв (1), выбыл из наблюдения (2) 
# [я мог ошибиться с цифрами 0, 1 и 2, но не суть]
# Ну, попробуем его объяснять признаками:
#
# Rand1 - Основной действующий препарат [кач]
# Immun - Иммунофенотип [кач]
# CNS - Статус нервной системы: поражена, не поражена [кач]
# Sex - Пол [кач]
# Mediastinum - Статус средостения: поражено, не поражено [кач]
# Age - Возраст в момент постановки диагноза [кол]
# Leuc - Количество лейкоцитов на 1 нл крови [кол]
# Leber - Пальпируемый размер печени (см) [кол]
# Milz - Пальпируемый размер селезенки (см) [кол]

# Итак, Tod, я выбираю тебя! Это качественный признак, задаёт 3 класса
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
N  <-  nrow(data)

target  <-  full_data[,18] # 18 - колонка Tod
data <- as.data.frame(lapply(data[,1:ncol(data)], normalize))

set.seed(1228)
smp.size <- floor(0.65 * N)
train.id <- sample(c(1:N), size = smp.size)

data.train <- data[train.id,]
data.test <- data[-train.id,]
target.train  <- target[train.id]
target.test <- target[-train.id]

library(class)

target.predict  <- knn(data.train, data.test, target.train, k = 1)
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
includedVars = c("Tod", "Rand1", "Immun", "CNS", "Sex", "Mediastinum") 
ndata = full_data[includedVars]
ntarget  <-  ndata[,1]

N  <- nrow(ndata)
set.seed(1228)
smp.size <- floor(0.66 * N)
train.id <- sample(c(1:N), size = smp.size)

ndata.train <- ndata[train.id,]
ndata.test <- ndata[-train.id,]
ntarget.train  <- ntarget[train.id]
ntarget.test <- ntarget[-train.id]

model <- naiveBayes(ntarget.train ~ ., data = ndata.train)
pred <- predict(model, ndata.test)
table(pred, ntarget.test)

# P.S.: Все комментарии были написаны в невменяемом состоянии. Возможно, они помогут накрапать отчёт
