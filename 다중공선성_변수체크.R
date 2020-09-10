library(googledrive)
library(leaps)
library(dplyr)
drive_auth()
driveDF = drive_find(type = "csv"); driveDF
driveDF$name

drive_download(file = driveDF$name[1], path = paste(PATH, driveDF$name[1], sep = ""), overwrite= TRUE)
drive_download(file = driveDF$name[2], path = paste(PATH, driveDF$name[2], sep = ""), overwrite= TRUE)
drive_download(file = driveDF$name[3], path = paste(PATH, driveDF$name[3], sep = ""), overwrite= TRUE)
drive_download(file = driveDF$name[4], path = paste(PATH, driveDF$name[4], sep = ""), overwrite= TRUE)
drive_download(file = driveDF$name[5], path = paste(PATH, driveDF$name[5], sep = ""), overwrite= TRUE)
drive_download(file = driveDF$name[6], path = paste(PATH, driveDF$name[6], sep = ""), overwrite= TRUE)
drive_download(file = driveDF$name[7], path = paste(PATH, driveDF$name[7], sep = ""), overwrite= TRUE)

teamhitter <- read.csv('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/1차_최종_데이터/team_hitter.csv', header = TRUE, encoding = 'UTF-8')

teamhitter = teamhitter %>% select(RUN, OPS, BABIP )

num = ncol(teamhitter)
regfit.full = regsubsets(RUN~.,teamhitter, nvmax =num-1)
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

which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
coef(regfit.full,7)

