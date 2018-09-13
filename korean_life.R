

# korean life

install.packages("dplyr")
install.packages("foreign") 
install.packages("readxl")
library(dplyr)
library(foreign)
library(readxl)

search()

setwd("E:/bigdata/dataset/R2_ex")
raw_data <- read.spss(file="Koweps_hpc10_2015_beta1.sav", to.data.frame=T)
data <- raw_data


head(data)
tail(data)
dim(data)
str(data)
summary(data)
names(data)


colnames(data)


# rename 

data <- rename(data,
               sex=h10_g3,
               birth=h10_g4,
               marriage=h10_g10,
               religion=h10_g11,
               income=p1002_8aq1,
               code_job=h10_eco9,
               code_region=h10_reg7)


sel = c('sex', 'birth', 'marriage', 'religion', 'income', 'code_job', 'code_region')
head( data[, sel])
df_sel = data[,sel]


class(df_sel$sex)
table(df_sel$sex)
cntS <- table(df_sel$sex)
barplot(cntS)



head(df_sel$birth)
cntBirth = table(df_sel$birth)
barplot(cntBirth, main='태어난 연도')



head(df_sel$marriage)
table(df_sel$marriage)
cntMarriage = table(df_sel$marriage)
barplot(cntMarriage, main='혼인상태')


head(df_sel$religion)
table(df_sel$religion)
cntReligion = table(df_sel$religion)
barplot(cntReligion, main='종교(1:있음, 2:없음)')


range(df_sel$income, na.rm=T)
hist(df_sel$income)
boxplot(df_sel$income)


install.packages("Amelia")
library(Amelia)
missmap(df_sel, main="Missing Map")

table(is.na(df_sel$sex))
table(is.na(df_sel$income))



# filter, group_by, summarise

df_selOK <- df_sel[!is.na(df_sel$income) , ]
str(df_selOK)
table(df_selOK$sex, useNA = "ifany" )
range(df_selOK$income)

tapply(df_selOK$income, df_selOK$sex, mean)

sex_income <- df_sel %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))
sex_income




df_sel$age <- 2015 - df_sel$birth + 1
summary(df_sel$age)


df_sel <- df_sel %>%
  mutate(age_level = ifelse(age <50, "young", "old"))
head(df_sel)

cntAgeLevel = table(df_sel$age_level)
barplot(cntAgeLevel)





age_income <- df_sel %>% 
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
head(age_income)
is(age_income)



library(ggplot2)
ggplot(data= age_income, aes(x=age, y=mean_income)) + geom_line()


top10 <- age_income %>%
  arrange(-mean_income) %>%
  head(10)
top10


top10 <- age_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10
