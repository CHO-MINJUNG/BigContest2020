<<<<<<< HEAD
library(leaps)

teamhitter <- read.csv('C:/Users/chomjung/OneDrive - �������б�/�����׽�Ʈ2020/1��_����_������/1��_����_�ֱ�_��Ÿ��.csv', header = TRUE, encoding = 'UTF-8')
teamhitter <- teamhitter[,-c(1:4)]
teamhitter <- teamhitter[,-1]
colnames(teamhitter)

regfit.full = regsubsets(����~.,teamhitter, nvmax =43, really.big = T)
reg.summary = summary(regfit.full)

which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

par(mfrow=c(2,2))
plot(reg.summary$adjr2,type="l",xlab="No. of variables", ylab="Adjusted R2")
points(19,reg.summary$adjr2[19],col="red",cex=2,pch=20)
plot(reg.summary$cp,ylab="Cp",type="l")
points(14,reg.summary$cp[14],col="red",cex=2,pch=20)
plot(reg.summary$bic, ylab="BIC",type="l")
points(13,reg.summary$bic[13],col="red",cex=2,pch=20)

k<-10
set.seed(1)
folds<-sample(1:k,nrow(teamhitter),replace=TRUE)
cv.errors<-matrix(NA,k,43,dimnames=list(NULL,paste(1:43)))#10x19 NA ����
for(j in 1:k){
  best.fit=regsubsets(����~.,data=teamhitter[folds!=j,],nvmax=43,really.big=T)
  for(i in 1:30){
    test.mat.fold<-model.matrix(����~.,data=teamhitter[folds==j,])
    coefi.fold<-coef(best.fit,id=i)
    pred.fold<-test.mat.fold[,names(coefi.fold)]%*%coefi.fold
    cv.errors[j,i]<-mean((teamhitter$����[folds==j]-pred.fold)^2)
  }
}
mean.cv.errors<-apply(cv.errors,2,mean) #�� ���� ������ ��հ�
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
=======
library(leaps)

teamhitter <- read.csv('C:/Users/chomjung/OneDrive - �������б�/�����׽�Ʈ2020/1��_����_������/1��_����_�ֱ�_��Ÿ��.csv', header = TRUE, encoding = 'UTF-8')
teamhitter <- teamhitter[,-c(1:4)]
teamhitter <- teamhitter[,-1]

regfit.full = regsubsets(����~.,teamhitter, nvmax =43, really.big = T)
reg.summary = summary(regfit.full)

k<-10
set.seed(1)
folds<-sample(1:k,nrow(teamhitter),replace=TRUE)
cv.errors<-matrix(NA,k,43,dimnames=list(NULL,paste(1:43)))#10x19 NA ����
for(j in 1:k){
  best.fit=regsubsets(����~.,data=teamhitter[folds!=j,],nvmax=19)
  for(i in 1:19){
    test.mat.fold<-model.matrix(����~.,data=teamhitter[folds==j,])
    coefi.fold<-coef(best.fit,id=i)
    pred.fold<-test.mat.fold[,names(coefi.fold)]%*%coefi.fold
    cv.errors[j,i]<-mean((teamhitter$����[folds==j]-pred.fold)^2)
  }
}
mean.cv.errors<-apply(cv.errors,2,mean) #�� ���� ������ ��հ�
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
>>>>>>> dfda973716f1b1b2717ad8c494c05edf6e6d62b6
