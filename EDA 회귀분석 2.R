library(leaps)
library(data.table)

teampitcher <- fread('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/데이터 전처리/1차_최종_데이터/1차_최종_팀투수.csv', header = TRUE, encoding = 'UTF-8')
colnames(teampitcher)

regfit.full=regsubsets(실점~.,Hitters,nvmax=)