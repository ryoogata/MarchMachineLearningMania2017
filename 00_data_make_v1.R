###使用ライブラリ
library(data.table)
library(dplyr)

###データ読込
dat <- data.table::fread("data/RegularSeasonDetailedResults.csv",
           header=TRUE, data.table=FALSE)
sample <- data.table::fread("data/sample_submission.csv",
              header=TRUE, data.table=FALSE)

# > anyNA(dat)
# [1] FALSE

###データ整形
#1 試合内容を, 年ごとかつチームごとに積みなおす
#2 年ごとに各変数の平均値, 勝率を計算
#3 元からあるのteam番号の小さい方の勝ちflg作成
vars_name1<-c("Season","team1","score1","fgm1",
             "fga1","fgm31","fga31","ftm1",
             "fta1","or1","dr1","ast1",
             "to1","stl1","blk1","pf1",
             "win1")
vars_name2<-c("team2","Season","score2","fgm2",
             "fga2","fgm32","fga32","ftm2",
             "fta2","or2","dr2","ast2",
             "to2","stl2","blk2","pf2",
             "win2")
W_dat<-dat %>%
  dplyr::select(Season,
                starts_with("W", ignore.case = TRUE)) %>%
  dplyr::select(-Wloc) %>% # 勝利チームの試合場所
  dplyr::mutate(win=1)
names(W_dat)<-vars_name1

L_dat<-dat %>%
  dplyr::select(Season,
                starts_with("L", ignore.case = TRUE)) %>%
  dplyr::mutate(win=0)
names(L_dat)<-vars_name1

vars1<-rbind(W_dat, L_dat) %>% #1
  dplyr::group_by(team1, Season) %>%
  dplyr::summarise_each(funs(mean), everything()) %>% #2
  dplyr::ungroup(.) %>%
  dplyr::arrange(team1, Season) # データフレームを指定した列でソート
vars1$Season<-vars1$Season+1

vars2<-vars1
names(vars2)<-vars_name2

# > anyNA(vars)
# [1] FALSE

#3
fun_flg<-function(w,l){
  z<-ifelse(w<l, 1, 0)
  return(z)
}

train<-dat %>%
  dplyr::select(Season, Wteam, Lteam) %>%
  dplyr::mutate(team1=mapply(min, dat$Wteam, dat$Lteam),
                team2=mapply(max, dat$Wteam, dat$Lteam),
                flg=fun_flg(Wteam, Lteam)) %>%
  dplyr::select(-Wteam, -Lteam) %>%
  dplyr::inner_join(vars1, by=c("Season", "team1")) %>%
  dplyr::inner_join(vars2, by=c("Season", "team2"))

test<-sample %>%
  dplyr::mutate(Season=as.integer(substr(sample$id, 1, 4)),
                team1=as.integer(substr(sample$id, 6, 9)),
                team2=as.integer(substr(sample$id, 11, 14))) %>%
  dplyr::inner_join(vars1, by=c("Season", "team1")) %>%
  dplyr::inner_join(vars2, by=c("Season", "team2")) %>%
  dplyr::select(-pred)

# > anyNA(train)
# [1] FALSE

# > anyNA(test)
# [1] FALSE

###保存
save(train, file="data/train.RData")
save(test, file="data/test.RData")

