library(MASS)
library(data.table)

### 타자 관련
hitter_forward <- function (hitterdata){
  null <- lm(득점 ~ 1, data= hitterdata)
  full <- lm(득점 ~ . , data = hitterdata)
  
  # 전진 선택법(Forward selection)
  forward <- step(null, direction = "forward", scope = list(lower=null, upper = full))
}

hitter_backward <- function (hitterdata){
  null <- lm(득점 ~ 1, data= hitterdata)
  full <- lm(득점 ~ . , data = hitterdata)
  
  # 후진 제거법(Backward elimination)
  backward <- step(full, direction = "backward")
}

hitter_stepwise <- function (hitterdata){
  null <- lm(득점 ~ 1, data= hitterdata)
  full <- lm(득점 ~ . , data = hitterdata)
  
  # 단계적 회귀(Stepwise regression)
  stepwise = step(null, direction = "both", scope = list(upper = full))
}

### 투수 관련
pitcher_forward <- function (pitcherdata){
  null <- lm(실점 ~ 1, data= pitcherdata)
  full <- lm(실점 ~ . , data = pitcherdata)
  
  # 전진 선택법(Forward selection)
  forward <- step(null, direction = "forward", scope = list(lower=null, upper = full))
}

pitcher_backward <- function (pitcherdata){
  null <- lm(실점 ~ 1, data= pitcherdata)
  full <- lm(실점 ~ . , data = pitcherdata)
  
  # 후진 제거법(Backward elimination)
  backward <- step(full, direction = "backward")
}

pitcher_stepwise <- function (pitcherdata){
  null <- lm(실점 ~ 1, data= pitcherdata)
  full <- lm(실점 ~ . , data = pitcherdata)
  
  # 단계적 회귀(Stepwise regression)
  stepwise = step(null, direction = "both", scope = list(upper = full))
}


teamhitter = read.csv('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/데이터 전처리/팀타자 최종.csv', header = TRUE)
teampitcher <- fread('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/데이터 전처리/1차_최종_데이터/1차_최종_팀투수.csv', header = TRUE, encoding = 'UTF-8')
result = pitcher_stepwise(teampitcher)

