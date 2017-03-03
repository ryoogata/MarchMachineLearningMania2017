require(caret)
require(dplyr)

# 指数表示の回避
options(scipen=10)

#
# データの読み込み
#
load("data/train.RData")
load("data/test.RData")

# train と test をマージ
# all <- bind_rows(train, test)
all <- train

# 目的変数の factor 化
all[which(all$flg == 1),"flg"] <- "yes"
all[which(all$flg == 0),"flg"] <- "no"

# 目的変数名を response に変更
names(all)[names(all) == "flg"] <- "response"


# Feature Engineering 

#
# Dummy 変数なし
#

# all から train/test のデータを抽出
all.train <- all
all.train$response <- as.factor(all.train$response)

# 不要な列: data を
# all.train <- subset(all.train, select = -c(data))
# all.test <- subset(all.test, select = -c(data))

# 再現性のため乱数シードを固定
set.seed(10)

# 訓練データと検証データに分割する
# Train 用の列番号を作成
inTrain <- caret::createDataPartition(all.train$response, p = .8, list = FALSE)
train.train <- all.train[inTrain,]
train.test <- all.train[-inTrain,]

dim(train.train)
# [1] 52768    34

train.train$response %>% table
# .
# 0     1 
# 27044 25724 

dim(train.test)
# [1] 13190    34

train.test$response %>% table
# .
# 0    1 
# 6760 6430 
