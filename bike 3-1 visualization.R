


### 변수간 관계 확인
setwd("E:/bigdata/dataset/bike")
df <- read.csv("train.csv")

str(df)
sel = c("temp", "atemp", "humidity","windspeed","count")
trA_sub <- df[  , sel]  # 수치형만 뽑아서 객체 만들기

## 산점도 확인
plot(trA_sub)


## Boxplot 확인 - 수치형 
# season
boxplot(df$season)
boxplot(df$count ~ df$season)
boxplot(df$temp)

## 막대 그래프(barplot)
barplot(df$season, df$count)

## 변수간 관계 확인 
sel = c("temp", "atemp", "humidity","windspeed","count")
trA_sub <- df[  , sel]
str(trA_sub)
## 상관계수
tr_cor = cor(trA_sub)
tr_cor
#install.packages("corrplot")
library(corrplot)
corrplot(tr_cor, method='color', addCoef.col = "black")
