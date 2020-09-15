library(fpp2)
data = read.csv('C:/Users/chomjung/OneDrive - 명지대학교/빅콘테스트2020/1차_최종_데이터/1차_최종_최근_팀타자.csv', stringsAsFactors =TRUE, header = TRUE, encoding="UTF-8")
NCdata= subset(data, 팀코드=='NC')
SKdata= subset(data, 팀코드=='SK')
KTdata= subset(data, 팀코드=='KT')
OBdata= subset(data, 팀코드=='OB')
WOdata= subset(data, 팀코드=='WO')
HHdata= subset(data, 팀코드=='HH')
SSdata= subset(data, 팀코드=='SS')
LGdata= subset(data, 팀코드=='LG')
HTdata= subset(data, 팀코드=='HT')
LTdata= subset(data, 팀코드=='LT')

# NC
NCdata <- ts(NCdata$잔루,start = 2016,end = 2019,frequency = 20)
NCdata = stl(NCdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(NCdata)
ft = max(0,1-var(NCdata$time.series[,3])/(var(NCdata$time.series[,2]+NCdata$time.series[,3]))); ft
fs = max(0,1-var(NCdata$time.series[,3])/(var(NCdata$time.series[,1]+NCdata$time.series[,3]))); fs

# SK
SKdata <- ts(SKdata$2루타,start = 2016,end = 2019,frequency = 20)
SKdata = stl(SKdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(SKdata)
ft = max(0,1-var(SKdata$time.series[,3])/(var(SKdata$time.series[,2]+SKdata$time.series[,3]))); ft
fs = max(0,1-var(SKdata$time.series[,3])/(var(SKdata$time.series[,1]+SKdata$time.series[,3]))); fs

# KT
KTdata <- ts(KTdata$단타,start = 2016,end = 2019,frequency = 20)
KTdata = stl(KTdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(KTdata)
ft = max(0,1-var(KTdata$time.series[,3])/(var(KTdata$time.series[,2]+KTdata$time.series[,3]))); ft
fs = max(0,1-var(KTdata$time.series[,3])/(var(KTdata$time.series[,1]+KTdata$time.series[,3]))); fs

# OB
OBdata <- ts(OBdata$단타,start = 2016,end = 2019,frequency = 20)
OBdata = stl(OBdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(OBdata)
ft = max(0,1-var(OBdata$time.series[,3])/(var(OBdata$time.series[,2]+OBdata$time.series[,3]))); ft
fs = max(0,1-var(OBdata$time.series[,3])/(var(OBdata$time.series[,1]+OBdata$time.series[,3]))); fs

# WO
WOdata <- ts(WOdata$단타,start = 2016,end = 2019,frequency = 20)
WOdata = stl(WOdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(WOdata)
ft = max(0,1-var(WOdata$time.series[,3])/(var(WOdata$time.series[,2]+WOdata$time.series[,3]))); ft
fs = max(0,1-var(WOdata$time.series[,3])/(var(WOdata$time.series[,1]+WOdata$time.series[,3]))); fs

# HH
HHdata <- ts(HHdata$단타,start = 2016,end = 2019,frequency = 20)
HHdata = stl(HHdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(HHdata)
ft = max(0,1-var(HHdata$time.series[,3])/(var(HHdata$time.series[,2]+HHdata$time.series[,3]))); ft
fs = max(0,1-var(HHdata$time.series[,3])/(var(HHdata$time.series[,1]+HHdata$time.series[,3]))); fs

# SS
SSdata <- ts(SSdata$득점권타수,start = 2016,end = 2019,frequency = 20)
SSdata = stl(SSdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(SSdata)
ft = max(0,1-var(SSdata$time.series[,3])/(var(SSdata$time.series[,2]+SSdata$time.series[,3]))); ft
fs = max(0,1-var(SSdata$time.series[,3])/(var(SSdata$time.series[,1]+SSdata$time.series[,3]))); fs

# LG
LGdata <- ts(LGdata$득점권타수,start = 2016,end = 2019,frequency = 20)
LGdata = stl(LGdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(LGdata)
ft = max(0,1-var(LGdata$time.series[,3])/(var(LGdata$time.series[,2]+LGdata$time.series[,3]))); ft
fs = max(0,1-var(LGdata$time.series[,3])/(var(LGdata$time.series[,1]+LGdata$time.series[,3]))); fs

# HT
HTdata <- ts(HTdata$득점권타수,start = 2016,end = 2019,frequency = 20)
HTdata = stl(HTdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(HTdata)
ft = max(0,1-var(HTdata$time.series[,3])/(var(HTdata$time.series[,2]+HTdata$time.series[,3]))); ft
fs = max(0,1-var(HTdata$time.series[,3])/(var(HTdata$time.series[,1]+HTdata$time.series[,3]))); fs

# LT
LTdata <- ts(LTdata$득점권타수,start = 2016,end = 2019,frequency = 20)
LTdata = stl(LTdata, t.window=13, s.window="periodic", robust=TRUE)
autoplot(LTdata)
ft = max(0,1-var(LTdata$time.series[,3])/(var(LTdata$time.series[,2]+LTdata$time.series[,3]))); ft
fs = max(0,1-var(LTdata$time.series[,3])/(var(LTdata$time.series[,1]+LTdata$time.series[,3]))); fs

