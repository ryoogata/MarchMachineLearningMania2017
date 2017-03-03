require(caret)
require(caretEnsemble)
require(pROC)
require(doParallel)

require(rpart)
require(partykit)
require(rattle)

source("script/R/fun/tools.R")
result.rpart.df <- readRDS("result/result.ranger.df.data")

#
# 前処理
#
source("./Data-pre-processing.R")

my_preProcess <- c("center", "scale")

data_preProcess <- "none"

TRAIN <- all.train
TRAIN.TRAIN <- train.train
TRAIN.TEST <- train.test
TEST <- test

#
# 欠損値処理
#

# 欠損値の確認
sapply(train, function(x) sum(is.na(x)))

#
# rpart
#

# seeds の決定
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 500)
seeds[[51]] <- sample.int(1000, 1)

my_control <- trainControl(
  method = "cv"
  ,number = 10
  ,summaryFunction = mnLogLoss
  ,classProbs = TRUE
  ,verbose = TRUE
  ,savePredictions = "final"
  ,index = createResample(TRAIN.TRAIN$response, 10)
  ,seeds = seeds
)

doParallel <- trainControl(
  method = "cv"
  ,number = 10
  ,summaryFunction = mnLogLoss
  ,classProbs = TRUE
  ,allowParallel=TRUE
  ,verboseIter=TRUE
  ,savePredictions = "final"
  ,index = createResample(TRAIN.TRAIN$response, 10)
  ,seeds = seeds
)

# 説明変数一覧の作成
explanation_variable <- names(subset(TRAIN, select = -c(response)))

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

model_list <- caretList(
  x = TRAIN.TRAIN[,explanation_variable]
  ,y = TRAIN.TRAIN$response
  ,trControl = doParallel
  #,preProcess = my_preProcess
  ,tuneList = list(
    fit.ranger = caretModelSpec(
      method = "ranger"
      ,metric = "logLoss" 
      ,tuneGrid = expand.grid(mtry = c(5))
      ,importance = 'impurity'
    )
  )
)

stopCluster(cl)
registerDoSEQ()

model_list[[1]]$times
# $everything
# ユーザ   システム       経過  
# 230.002     11.100   1199.263 

model_list[[1]]
model_list[[1]]$finalModel
rattle::fancyRpartPlot(model_list[[1]]$finalModel)
model_list[[1]]$finalModel$variable.importance
varImp(model_list[[1]], scale = FALSE, useModel = FALSE)
varImp(model_list[[1]], scale = FALSE)
plot(varImp(model_list[[1]], scale = FALSE))

ggplot(model_list[[1]]) 


#
# テストデータにモデルを当てはめる ( Prob )
#
allProb <- caret::extractProb(
                              list(model_list[[1]])
                              ,testX = subset(TRAIN.TEST, select = -c(response))
                              ,testY = unlist(subset(TRAIN.TEST, select = c(response)))
                             )

# dataType 列に Test と入っているもののみを抜き出す
testProb <- subset(allProb, dataType == "Test")
tp <- subset(testProb, object == "Object1")


###精度確認
log_loss<-function(x,y){
  z<--mean(
    x*log(y)+(1-x)*log(1-y)
  )
  return(z)
}

tp <- dplyr::mutate(tp, flg = as.character(tp$obs))
tp[which(tp$flg == "yes"), "flg"] <- 1
tp[which(tp$flg == "no"), "flg"] <- 0
tp$flg <- as.numeric(tp$flg)

log_loss(tp$flg, tp$yes)


# 結果の保存
result.ranger.df <- rbind(result.ranger.df, summaryResult(model_list[[1]]))
saveRDS(result.rpart.df, "result/result.ranger.df.data")

# predict() を利用した検算 
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test.verification <- predict(
    model_list[[1]]$finalModel
    ,subset(TRAIN.TEST, select = -c(response))
  )
} else {
  # preProcess を指定している場合
  pred_test.verification <- preProcess(
    subset(TRAIN.TEST, select = -c(response))
    ,method = my_preProcess
  ) %>%
    predict(., subset(TRAIN.TEST, select = -c(response))) %>%
    predict(model_list[[1]]$finalModel, .)
}

TRAIN.TEST <- dplyr::mutate(TRAIN.TEST, flg = as.character(TRAIN.TEST$response))
TRAIN.TEST[which(TRAIN.TEST$flg == "yes"), "flg"] <- 1
TRAIN.TEST[which(TRAIN.TEST$flg == "no"), "flg"] <- 0
TRAIN.TEST$flg <- as.numeric(TRAIN.TEST$flg)

log_loss(TRAIN.TEST$flg, pred_test.verification$predictions[,"yes"])

#
# 予測データにモデルの当てはめ
#
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test <- predict(model_list[[1]]$finalModel, TEST, type="response")$prediction[,2]
  
  PREPROCESS <- "no_preProcess"
} else {
  # preProcess を指定している場合
  pred_test <- preProcess(TEST, method = my_preProcess) %>%
    predict(., TEST) %>%
    predict(model_list[[1]]$finalModel, .)
  
  pred_test <- pred_test[,2]
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}


#submitの形式で出力(CSV)
#データ加工
out <- data.frame(TEST$id, pred_test)
names(out) <- c("id","pred")

# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  SUBMIT_FILENAME <- paste("./submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_rpart.csv", sep = "")
  
  if ( !file.exists(SUBMIT_FILENAME) ) {
    write.table(out, #出力データ
                SUBMIT_FILENAME, #出力先
                quote = FALSE, #文字列を「"」で囲む有無
                col.names = TRUE, #変数名(列名)の有無
                row.names = FALSE, #行番号の有無
                sep = "," #区切り文字の指定
    )
    break
  }
}
