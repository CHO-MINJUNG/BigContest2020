library(car)
teamhitter = read.csv('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/AfterMay_DF/hitter_r.csv', stringsAsFactors =TRUE,header = TRUE, encoding="UTF-8")
str(teamhitter)
colnames(teamhitter)
teamhitter = teamhitter[,c("팀코드","상대팀코드","더블헤더코드","초말","최근OBP",
                          "최근SLG","최근OPS","최근GPA","최근타율",
                          "최근BABIP","최근HR.","최근K.","최근BB.",
                          "최근BBK","최근wOBA","최근wRAA","최근wRC","최근RC","최근RC27","최근XR",
                          "최근SPD","최근SECA","최근PSN","최근PA","최근B3B","최근BB","최근HR",
                          "최근K","최근HBP","최근IBB","최근SB","최근CS","최근AB","최근HIT",
                          "최근B1B","최근B2B","최근타자득점","최근득점권타수","최근득점권안타","최근득점권타율")]
hitter_stepwise <- function (hitterdata){
  null <- lm(최근타자득점 ~ 1, data= hitterdata)
  full <- lm(최근타자득점 ~ . , data = hitterdata)
  
  # 단계적 회귀(Stepwise regression)
  stepwise = step(null, direction = "both", scope = list(upper = full))
  return(stepwise)
}
result = hitter_stepwise(teamhitter)
summary(result)
vif(result)

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
result = pitcher_stepwise(teampitcher)
summary(result)


