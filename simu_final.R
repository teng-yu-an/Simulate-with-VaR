setwd("/Users/tengyuan/simulate/final")
data_2882<-read.table("2882.TW.csv", sep= ",", na.strings = "null", header = T)
data_3008<-read.table("3008.TW.csv", sep= ",", na.strings = "null", header = T)
data_1301<-read.table("1301.TW.csv", sep= ",", na.strings = "null", header = T)

data_2882[,1]<-as.Date(data_2882[,1])
data_3008[,1]<-as.Date(data_3008[,1])
data_1301[,1]<-as.Date(data_1301[,1])


data_2882<-data_2882[data_2882[,1]>="2011-01-01",]
data_3008<-data_3008[data_3008[,1]>="2011-01-01",]
data_1301<-data_1301[data_1301[,1]>="2011-01-01",]



##### 資料清理 #####
##replace NA's with their previous values 
NA_rep<-function(x, colx){ 
  
  ind_miss<-which(is.na(x[, colx]))  
  
  for(i in ind_miss){           
    x[i, colx]<-x[(i-1), colx]
    
  }
  
  return(x)
  
}

data_1301<-NA_rep(data_1301, 6) #把Adj.Close用前一天的填上
data_2882<-NA_rep(data_2882, 6)   
data_3008<-NA_rep(data_3008, 6)

##### 報酬率
setwd("/Users/tengyuan/financial")
source("function_FDA.R")

data_2882$ret<-c(NA, retx(data_2882$Adj.Close)) 
data_3008$ret<-c(NA, retx(data_3008$Adj.Close))
data_1301$ret<-c(NA, retx(data_1301$Adj.Close)) 

datax<-data.frame(matrix(0, nrow(data_2882), 4))
datax[,1]<-data_2882$Date
datax[,2:ncol(datax)]<-cbind(data_2882$ret, data_3008$ret, data_1301$ret)  

colnames(datax)<-c("Date","x2882","x3008","x1301")    

head(datax) #三支股票的報酬率

data_return <- datax
corx<-cor(data_return[-1,2:ncol(data_return)]) #相關係數

##### 投資組合：計算在 〖fix-weighted, 1/N〗 投資組合策略下的投資組合報酬
##〖fix-weighted, 1/N〗
datax$retN<-apply(datax[, 2:ncol(datax)], 1, mean, na.rm = T)   ##note to set na.rm = T
datax$retN<-datax$retN*100       

head(datax)

##remove the first row of NA
datax<-datax[-1, ]

summary(datax[, 5]) #可以畫表二：投資組合報酬率之敘述統計
sd(datax[, 5])
my_skewness(datax[, 5])
my_kurtosis(datax[, 5])



##### VaR：計算分別用【expanding window】和【rolling window】算〖fix-weighted, 1/N〗的VaR
kx<-250                                     ##window length  
alpha<-0.05                                 ##VaR level

result1<-NULL
result2<-NULL

##use a "for" loop
for(i in 1:(nrow(datax)-kx)){        
  
  ##expanding window
  varx<-VaR_samplex(datax[1:(i+kx-1), 5], 1, alpha)         
  result1<-rbind(result1,varx)
  
  ##rolling window
  varx1<-VaR_samplex(datax[i:(i+kx-1), 5], 1, alpha)          
  result2<-rbind(result2,varx1)
  
}

#報酬率、兩種VaR整合成data1
datax1<-data.frame(matrix(0, nrow(datax)-kx, 4))
datax1[,1]<-datax$Date[(kx+1):nrow(datax)]
datax1[,2]<-datax[(kx+1):nrow(datax), 5]  
datax1[,3:ncol(datax1)]<-cbind(result1, result2)
colnames(datax1)<-c("Date","r","VaR_Exp","VaR_Rw")

#畫圖
showtext_auto(enable = TRUE)
rangex<-range(datax1[, 2:ncol(datax1)])

plot(x = datax1$Date, y = datax1$r, type="l", main = "報酬率及 VaR (Alpha = 0.05) -- 歷史模擬法",
     ylim = rangex, col = "gray",
     xlab = "Date", ylab = "Return (%)")

lines(x = datax1$Date, y = datax1$VaR_Exp, type="l", col = 4, lty=3, lwd=2) 
lines(x = datax1$Date, y = datax1$VaR_Rw, type="l", col = 6, lty=4, lwd=1) 

legend("bottomleft", box.col = 0,
       legend = c("Return","VaR (expanding window)","VaR (rolling window)"), 
       lty =c(1,3,4),
       lwd = c(1,2,1),
       col = c("gray","4","6"))


plot(x = datax1$Date, y = datax1$r, type="l", main = "報酬率-- 歷史模擬法",
     ylim = rangex,xlim =c(datax1$Date[512] ,datax1$Date[939]) , col = "gray",
     xlab = "Date", ylab = "Return (%)")

#計算有幾次的負報酬低於算出來的VaR（穿透次數）
sum(datax1$r<=datax1$VaR_Exp) #81
sum(datax1$r<=datax1$VaR_Rw) #118

## alpha=0.01 ##
kx<-250
alpha<-0.01

result1<-NULL
result2<-NULL

for(i in 1:(nrow(datax)-kx)){        
  
  ##expanding window
  varx<-VaR_samplex(datax[1:(i+kx-1), 5], 1, alpha)         
  result1<-rbind(result1,varx)
  
  ##rolling window
  varx1<-VaR_samplex(datax[i:(i+kx-1), 5], 1, alpha)          
  result2<-rbind(result2,varx1)
  
}

#報酬率、兩種VaR整合成data1
datax1<-data.frame(matrix(0, nrow(datax)-kx, 4))
datax1[,1]<-datax$Date[(kx+1):nrow(datax)]
datax1[,2]<-datax[(kx+1):nrow(datax), 5]  
datax1[,3:ncol(datax1)]<-cbind(result1, result2)
colnames(datax1)<-c("Date","r","VaR_Exp","VaR_Rw")

#畫圖
showtext_auto(enable = TRUE)
rangex<-range(datax1[, 2:ncol(datax1)])

plot(x = datax1$Date, y = datax1$r, type="l", main = "報酬率及 VaR (Alpha = 0.01) -- 歷史模擬法",
     ylim = rangex, col = "gray",
     xlab = "Date", ylab = "Return (%)")

lines(x = datax1$Date, y = datax1$VaR_Exp, type="l", col = 4, lty=3, lwd=2) 
lines(x = datax1$Date, y = datax1$VaR_Rw, type="l", col = 6, lty=4, lwd=1) 

legend("bottomleft", box.col = 0,
       legend = c("Return","VaR (expanding window)","VaR (rolling window)"), 
       lty =c(1,3,4),
       lwd = c(1,2,1),
       col = c("gray","4","6"))

#計算有幾次的負報酬低於算出來的VaR（穿透次數）
sum(datax1$r<=datax1$VaR_Exp) #16
sum(datax1$r<=datax1$VaR_Rw) #34

#LR檢定
datax1$Date[which(datax1$r<=datax1$VaR_Exp)]
datax1$Date[which(datax1$r<=datax1$VaR_Rw)]


####### 蒙地卡羅 ######

VaR_monte.carlo <- function(data,n,amount,alpha){
  ret = data
  cor = cor(ret)
  chol.cor = chol(cor)
  
  monte.value = NULL
  for ( i in c(1:n)){
    rn = matrix(rt(3,df=3),ncol = ncol(ret))
    chol.ret = rn%*%chol.cor
    monte.value = rbind(monte.value,chol.ret)
  }
  N = apply(monte.value, 1, mean)
  qx = as.numeric(quantile(N,alpha))
  qx*amount
}


kx<-250 
alpha<-0.01
result<-NULL

for(i in 1:(nrow(datax)-kx)){        
  
  varx<-VaR_monte.carlo(datax[i:(i+kx-1), 2:4], 5000, 1, alpha)         
  result<-rbind(result,varx)
  
}
length(result)
#報酬率、VaR整合成datax2
datax2<-data.frame(matrix(0, nrow(datax)-kx, 3))
datax2[,1]<-datax$Date[(kx+1):nrow(datax)]
datax2[,2]<-datax[(kx+1):nrow(datax), 5]  
datax2[,3]<-as.numeric(result)
rownames(datax2) <- c(1:2448)
colnames(datax2)<-c("Date","r","VaR_monte")

#畫圖
showtext_auto(enable = TRUE)
rangex<-range(datax2[, 2:ncol(datax2)])

plot(x = datax2$Date, y = datax2$r, type="l", main = "報酬率及 VaR (Alpha = 0.05) -- 蒙地卡羅模擬法",
     ylim = rangex, col = "gray",
     xlab = "Date", ylab = "Return (%)")

lines(x = datax2$Date, y = datax2$VaR_monte, type="l", col = 4) 
legend("bottomleft", box.col = 0,
       legend = c("Return","VaR (Monte Carlo)"), 
       lty =c(1,1),
       lwd = c(1,1),
       col = c("gray","4"))

#計算有幾次的負報酬低於算出來的VaR（穿透次數）
sum(datax2$r<=datax2$VaR_monte) #alpha=0.01 : 31   #alpha=0.05 : 153

#LR檢定
datax1$Date[which(datax2$r<=datax2$VaR_monte)]
lr_N.1 <- (-2)*log(((0.01)^1)*(0.99^(250-1)))+2*log(((1/250)^1)*((1-1/250)^(250-1)))
lr_N.2 <- (-2)*log(((0.01)^2)*(0.99^(250-2)))+2*log(((2/250)^2)*((1-2/250)^(250-2)))
lr_N.3 <- (-2)*log(((0.01)^3)*(0.99^(250-3)))+2*log(((3/250)^3)*((1-3/250)^(250-3)))
lr_N.4 <- (-2)*log(((0.01)^4)*(0.99^(250-4)))+2*log(((4/250)^4)*((1-4/250)^(250-4)))
lr_N.6 <- (-2)*log(((0.01)^6)*(0.99^(250-6)))+2*log(((6/250)^6)*((1-6/250)^(250-6)))
lr_N.7 <- (-2)*log(((0.01)^7)*(0.99^(250-7)))+2*log(((7/250)^7)*((1-7/250)^(250-7)))
lr_N.9 <- (-2)*log(((0.01)^9)*(0.99^(250-9)))+2*log(((9/250)^9)*((1-9/250)^(250-9)))


###-------
#可以畫常態（圖一）
##data, SP500 index
setwd("/Users/tengyuan/financial")
data_GSPC<-read.table("GSPC.csv",
                      header = TRUE, sep=",")
data_GSPC[,1]<-as.Date(data_GSPC[,1])
data_GSPC$ret<-c(NA, retx(data_GSPC$Adj.Close))

##from 2011-01-01
data_GSPC<-data_GSPC[data_GSPC[,1]>="2011-01-01",]

##Normal VaR
##suppose investment amount = $1
VaR_normx(datax$retN, 1, 0.01)
VaR_normx(datax$retN, 1, 0.05)

##sample VaR
VaR_samplex(datax$retN, 1, 0.01)
VaR_samplex(datax$retN, 1, 0.05)

##plot empirical density of sp500 ret*1000000
plot(density(datax$retN),
     main = "股票報酬率次數分配圖",
     xlab = "報酬率", ylab = "", 
     lwd = 3)

##plot normal density
gx<-seq(min(datax$retN), max(datax$retN), length = 1000)
yy<-dnorm(gx, mean = mean(datax$retN), sd = sd(datax$retN))
lines(x=gx, y = yy, lwd = 3, col = 2, lty =2)

#畫t分配
lines(gx, y=datax2$r)
# plot(dt(gx, df= 3,ncp = 0))


##plot sample VaR
abline(v = VaR_samplex(datax$retN, 1, 0.01), 
       col = "gray", lty = 1, lwd =1)
abline(v = VaR_samplex(datax$retN, 1, 0.05), 
       col = "blue", lty = 1, lwd =1)

##plot normal VaR
abline(v = VaR_normx(datax$retN, 1, 0.01), 
       col = "gray", lty = 2, lwd =1)
abline(v = VaR_normx(datax$retN, 1, 0.05), 
       col = "blue", lty = 2, lwd =1)

legend("topright", 
       legend = c("實際","常態","樣本VaR (0.05)","樣本VaR (0.01)","常態VaR (0.05)","常態VaR (0.01)"),
       col = c("black","red","blue","gray","blue","gray"), 
       lwd = 1, lty=c(1,2,1,1,2,2), box.col = 0)
