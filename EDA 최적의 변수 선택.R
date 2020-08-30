library(leaps)

hitter_test <- function (hitter){
  num = ncol(hitter)
  regfit.full = regsubsets(득점~.,hitter, nvmax =num-1, really.big = T)
  reg.summary = summary(regfit.full)
  
  adjr2 = which.max(reg.summary$adjr2)
  cp = which.min(reg.summary$cp)
  bic = which.min(reg.summary$bic)
  
  par(mfrow=c(2,2))
  plot(reg.summary$adjr2,type="l",xlab="No. of variables", ylab="Adjusted R2")
  points(adjr2,reg.summary$adjr2[adjr2],col="red",cex=2,pch=20)
  plot(reg.summary$cp,ylab="Cp",type="l")
  points(cp,reg.summary$cp[cp],col="red",cex=2,pch=20)
  plot(reg.summary$bic, ylab="BIC",type="l")
  points(bic,reg.summary$bic[bic],col="red",cex=2,pch=20)
}

pitcher_test <- function (pitcher){
  num = ncol(pitcher)
  regfit.full = regsubsets(실점~.,pitcher, nvmax =num-1, really.big = T)
  reg.summary = summary(regfit.full)
  
  adjr2 = which.max(reg.summary$adjr2)
  cp = which.min(reg.summary$cp)
  bic = which.min(reg.summary$bic)
  
  par(mfrow=c(2,2))
  plot(reg.summary$adjr2,type="l",xlab="No. of variables", ylab="Adjusted R2")
  points(adjr2,reg.summary$adjr2[adjr2],col="red",cex=2,pch=20)
  plot(reg.summary$cp,ylab="Cp",type="l")
  points(cp,reg.summary$cp[cp],col="red",cex=2,pch=20)
  plot(reg.summary$bic, ylab="BIC",type="l")
  points(bic,reg.summary$bic[bic],col="red",cex=2,pch=20)
}


teamhitter <- read.csv('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/1차_최종_데이터/1차_최종_최근_팀타자.csv', header = TRUE, encoding = 'UTF-8')
result = hitter_test(teamhitter)