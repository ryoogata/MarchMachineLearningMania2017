###使用ライブラリ
library(data.table)
library(dplyr)

###データ読込
load("data/train.RData")
load("data/test.RData")

###データ分割
train_train<-train %>%
  dplyr::filter(Season<2010) %>%
  dplyr::select(-Season, -team1, -team2)
train_test<-train %>%
  dplyr::filter(Season>=2010, Season<2013)

###ロジスティック回帰
model_logi<-glm(flg~., data=train_train, family=binomial(link="logit"))

###モデル適用
tt_p<-predict(model_logi, newdata=train_test, type="response")
t_p<-predict(model_logi, newdata=test, type="response")

###精度確認
log_loss<-function(x,y){
  z<--mean(
    x*log(y)+(1-x)*log(1-y)
  )
  return(z)
}

log_loss(train_test$flg, tt_p)
# > log_loss(train_test$flg, tt_p)
# [1] 0.6194924

###サブミット形式に合わせる
submit<-data.frame(test, pred=t_p) %>%
  dplyr::select(id, pred)

###CSV出力
write.csv(submit, "C:/Users/tarou/Desktop/kaggle/DSS1/submit/submit_20170226_1.csv",
          quote=FALSE, row.names=FALSE)

