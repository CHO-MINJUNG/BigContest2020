library(car)
teamhitter = read.csv('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/AfterMay_DF/hitter_r.csv', stringsAsFactors =TRUE,header = TRUE, encoding="UTF-8")
str(teamhitter)
teamhitter = teamhitter[4:68]
colnames(teamhitter)
teamhitter = teamhitter[,-c(5:28)]
hitter_stepwise <- function (hitterdata){
  null <- lm(최근타자득점 ~ 1, data= hitterdata)
  full <- lm(최근타자득점 ~ . , data = hitterdata)
  
  # 단계적 회귀(Stepwise regression)
  stepwise = step(null, direction = "both", scope = list(upper = full))
  return(stepwise)
}
hitter_result = hitter_stepwise(teamhitter)
summary(hitter_result)
vif(hitter_result)

teamhitter = teamhitter[,-c(6)]

teampitcher = read.csv('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/AfterMay_DF/pitcher_r.csv', stringsAsFactors =TRUE,header = TRUE, encoding="UTF-8")
str(teampitcher)
colnames(teampitcher)
teampitcher = teampitcher[4:80]
teampitcher = teampitcher[,-c(5:33)]
pitcher_stepwise <- function (pitcherdata){
  null <- lm(최근투수실점 ~ 1, data= pitcherdata)
  full <- lm(최근투수실점 ~ . , data = pitcherdata)
  
  # 단계적 회귀(Stepwise regression)
  stepwise = step(null, direction = "both", scope = list(upper = full))
  return(stepwise)
}
pitcher_result = pitcher_stepwise(teampitcher)
summary(pitcher_result)
vif(pitcher_result)

teampitcher = teampitcher[,-c(5)]
