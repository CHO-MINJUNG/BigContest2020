library(leaps)

teamhitter <- read.csv('C:/Users/chomjung/OneDrive - ¸íÁö´ëÇÐ±³/ºòÄÜÅ×½ºÆ®2020/1Â÷_ÃÖÁ¾_µ¥ÀÌÅÍ/1Â÷_ÃÖÁ¾_ÃÖ±Ù_ÆÀÅ¸ÀÚ.csv', header = TRUE, encoding = 'UTF-8')
teamhitter <- teamhitter[,-c(1:4)]
teamhitter <- teamhitter[,-1]

regfit.full = regsubsets(µæÁ¡~.,teamhitter, nvmax =43, really.big = T)
reg.summary = summary(regfit.full)

k<-10
set.seed(1)
folds<-sample(1:k,nrow(teamhitter),replace=TRUE)
cv.errors<-matrix(NA,k,43,dimnames=list(NULL,paste(1:43)))#10x19 NA »ý¼º
for(j in 1:k){
  best.fit=regsubsets(µæÁ¡~.,data=teamhitter[folds!=j,],nvmax=19)
  for(i in 1:19){
    test.mat.fold<-model.matrix(µæÁ¡~.,data=teamhitter[folds==j,])
    coefi.fold<-coef(best.fit,id=i)
    pred.fold<-test.mat.fold[,names(coefi.fold)]%*%coefi.fold
    cv.errors[j,i]<-mean((teamhitter$µæÁ¡[folds==j]-pred.fold)^2)
  }
}
mean.cv.errors<-apply(cv.errors,2,mean) #°¢ º¯¼ö °¹¼öº° Æò±Õ°ª
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')