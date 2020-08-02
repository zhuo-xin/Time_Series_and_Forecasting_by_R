dat<-read.table("CourseworkData19_20.csv",header = T,sep=",")
dat
data_patients<-dat[,2]
data_patients
install.packages("forecast")
library(forecast)

v=as.vector(data_patients)
v  
t <- ts(data=v,frequency=7)
plot(t,type='b')

#画MA7的移动平均数图
mav <- function(x,n){filter(x,rep(1/n,n), sides=2)}     #第一步MA值，左右两侧都有值
MA7<-mav(v,7)
par(mfrow=c(1,1))
plot(v,type='l',col='black',xlab='time',ylab = 'number of patients')
lines(MA7,col='yellow')
legend('topleft',c('orignal data','MA7'),cex=0.8,col=c('black','yellow'),lwd = c(2,2),lty=c(1,1))

#乘法分解
t.md <- decompose(t,"multiplicative") #Multiplicative decomposition
mdindices <- t.md$seasonal[1:7] #weekly seasonal indices
mdindices
plot(t.md)

#加法分解
t.ad <- decompose(t,"additive") #addtive decomposition
adindices <- t.md$seasonal[1:7] #weekly seasonal indices
adindices
plot(t.ad)



t.stl <- stl(t,"per") #STL decomposition
stlindices <- t.stl$time.series[1:7,1] #Monthly seasonal indices
stlindices
plot(t.stl, main="Decomposition of STL Time Series")


mean(v)
var(v)




#建立初始集
t_validation<-t[1:1023]
t_validation <- ts(data=t_validation,frequency=7)
plot(t_validation,type='b')
v_validation<-v[1:1023]

#建立训练集
t_test<-t[1024:1461]
t_test <- ts(data=t_test,frequency=7)
v_test<-v[1024:1461]
t[[1023]]
plot(t_test,type='b')



#简单统计量
mean(v)                        # mean
mean(abs(v-mean(v)))           # Mean Absolute Deviation
sum((v-mean(v))^2)/1460        # Mean Squared Deviation
sqrt(sum((v-mean(v))^2)/1460)  # Standard Deviation
summary(v)


#naive方法预测的各种误差情况
NF1<-v[1023:1460]
errNF<-v[1024:1461]-NF1           # naive方法的误差
perrNF<-100*(errNF/v[1024:1461])  #naive方法的百分比误差
mean(errNF)                   # naive method ME
mean(abs(errNF))              # naive method MAE
mean(errNF^2)                 # naive method MSE
mean(perrNF)                  # naive method MPE
mean(abs(perrNF))             # naive method MAPE

#平均数预测的误差情况
MA7<-rep(NA,7)
for(i in 1024:(length(v)-7)){
  ma7<-mean(c(v[i], v[i+1],v[i+2],v[i+3],v[i+4],v[i+5],v[i+6]))   #7  moving average【公式转换代码】
  MA7<-c(MA7,ma7)
}
MA7
length(MA7)
err<-v[1024:1461]-MA7
err
err^2
abs(err)
perr<-err/v*100
abs(perr)
mean(err^2,na.rm=T)              #Mean method MSE
mean(abs(perr),na.rm=T)

#画图比较nf 和平均 的预测
plot(v_test,col='red')
lines(MA7,col='black')
lines(NF1,col='yellow')
legend('topleft',c('test set','MA7','NF'),cex=0.8,col=c('red','black','yellow'),lwd = c(2,2),lty=c(1,1))





#SES预测
tforecasts<-HoltWinters(t_validation,beta=FALSE,gamma=FALSE) 
tforecasts        #得到一个合适的α值，之后代入到训练集
plot(tforecasts,col='yellow')  #初始集上的预测效果
tforecasts$fitted               #初始集上拟合值

t_test_forecasts<-HoltWinters(t_test,alpha = 0.1587094,beta=FALSE,gamma=FALSE) #用训练集数据建模
t_test_forecasts
tforecasts3<- predict(t_test_forecasts, n.ahead = 7, prediction.interval = T) #训练集数据往后预测7个
plot(t_test_forecasts,tforecasts3,main='Single Exponential Smoothing(α=0.1587094)',col='yellow')
#训练集上的拟合情况以及预测区间
tforecasts3   #7个预测值

MSE_t_test <- t_test_forecasts$SSE / length(t_test_forecasts$fitted[,1])#训练集上的实际值和拟合值得到的MSE
MSE_t_test        #误差统计量，与前面的方法比较

#holt linear
tforecasts_hl <- HoltWinters(t_validation,gamma = FALSE)
tforecasts_hl #在初始集上参数被估计出来 alpha: 0.2285297 beta : 0.1620096
plot(tforecasts_hl,col='yellow')  #初始集上的预测效果

t_test_forecasts_hl<-HoltWinters(t_test,alpha = 0.2285297,beta=0.1620096,gamma=FALSE)
t_test_forecasts_hl
tforecasts222<- predict(t_test_forecasts_hl, n.ahead = 7, prediction.interval = T) #训练集数据往后预测7个
plot(t_test_forecasts_hl,tforecasts222,main='Holt’s Linear Method (α=0.2285297,β=0.1620096)',col='yellow')
#训练集上的拟合情况以及预测区间
tforecasts222   #7个预测值

MSE_t_test_hl <- t_test_forecasts_hl$SSE / length(t_test_forecasts_hl$fitted[,1])#训练集上的实际值和拟合值得到的MSE
MSE_t_test_hl        #误差统计量，与前面的方法比较


#holt winter add
tforecasts_hwa <- HoltWinters(t_validation)
tforecasts_hwa # alpha: 0.1962084 beta : 0.002539499 gamma: 0.1092765

plot(tforecasts_hwa,col='yellow')  #初始集上的预测效果

t_test_forecasts_hwa<-HoltWinters(t_test,alpha= 0.1962084,beta =0.002539499 ,gamma=0.1092765)
t_test_forecasts_hwa

MSE_t_test_hwa <- t_test_forecasts_hwa$SSE / length(t_test_forecasts_hwa$fitted[,1])#训练集上的实际值和拟合值得到的MSE
MSE_t_test_hwa       #误差统计量，与前面的方法比较

tforecasts333<- predict(t_test_forecasts_hwa, n.ahead = 7, prediction.interval = T) #训练集数据往后预测7个
plot(t_test_forecasts_hwa,tforecasts333,main='Holt Winters’ Additive Method (α=0.1962084,β=0.002539499,γ=0.1092765)',col='yellow')
#训练集上的拟合情况以及预测区间
tforecasts333   #7个预测值


#holt winter mul
tforecasts_hwm <- HoltWinters(t_validation,seasonal = 'multiplicative')
tforecasts_hwm # alpha: 0.1886014 beta : 0.003300901 gamma: 0.09117969

plot(tforecasts_hwm,col='yellow')  #初始集上的预测效果

t_test_forecasts_hwm<-HoltWinters(t_test,alpha=0.1886014, beta= 0.003300901 ,gamma=0.09117969,seasonal = 'multiplicative')
t_test_forecasts_hwm

MSE_t_test_hwm <- t_test_forecasts_hwm$SSE / length(t_test_forecasts_hwm$fitted[,1])#训练集上的实际值和拟合值得到的MSE
MSE_t_test_hwm       #误差统计量，与前面的方法比较

tforecasts444<- predict(t_test_forecasts_hwm, n.ahead = 7, prediction.interval = T) #训练集数据往后预测7个
plot(t_test_forecasts_hwm,tforecasts444,main='Holt Winters’ Multiplicative Method (α=0.1886014, β= 0.003300901 ,γ=0.09117969)',col='yellow',ylim = c(0,100))
#训练集上的拟合情况以及预测区间
tforecasts444  #7个预测值

help("matrix")
help(layout)

#arima模型
#建立新的初始集
t_validation<-t[1:1023]
t_validation <- ts(data=t_validation,frequency=1)
#建立新的训练集
t_test<-t[1024:1461]
t_test <- ts(data=t_test,frequency=1)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(t_validation, ylab="values", main="initialization set")
t_validation.acf <- acf(t_validation, lag.max = 28, main="")
t_validation.acf
t_validation.pacf <- pacf(t_validation, lag.max = 28 , main="")
t_validation.pacf 
nLag = length(t_validation.acf$lag)
coeffTable <- data.frame(t_validation.acf$lag[2:nLag], t_validation.acf$acf[2:nLag,,1], t_validation.pacf$acf[,,1])
names(coeffTable ) <- c("Lag","ACF","PACF")
coeffTable

library(tseries)
adf.test(t_validation,k=3)
help(adf.test)


#进行差分处理
par(mfrow=c(2,1))
plot(t_validation, ylab="Original")
plot(diff(t_validation), ylab="1st order diff")
plot(diff(t_validation, d=2), ylab="2nd order diff")
#可以发现一阶差分就足够了
s1<-diff(t_validation,1)
par(mfrow=c(1,1))
plot(s1)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(s1, ylab="values", main="1 order diff Time Series")
s1.acf <- acf(s1, lag.max = 35, main="")
s1.pacf <- pacf(s1, lag.max = 35, main="")

adf.test(s1)#使用adf检验，看一下是否存在单位根（验证平稳性，若存在则不平稳）p值小于0.05，所以基本平稳

#图形显示acf图存在拖尾，q=0，pacf图超出虚线较多，但从整体上看，从16阶之后截断，而上面的线可以大概看到（1，2，3，5，7）这五个数超出虚线甚多，所以，可以每个都测试一下。形成(1,1,0)(2,1,0),(3,1,0),(5,1,0),(7,1,0),然后判断一下各自的AIC值，取最小值即可。
##且acf图存在一个7阶的季节性影响因素，然后通过作滞后7阶的图看一下是否消除
s7<-diff(s1,lag=7)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(s7, ylab="values", main="7 order diff Time Series")
s7.acf <- acf(s7, lag.max = 35, main="")
s7.pacf <- pacf(s7, lag.max = 35, main="")

model_110_7<-arima(t_validation,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=7))#AIC=7316.99
model_210_7<-arima(t_validation,order=c(2,1,1),seasonal=list(order=c(1,1,1),period=7))#AIC=7193.59
model_310_7<-arima(t_validation,order=c(3,1,0),seasonal=list(order=c(1,1,1),period=7))#AIC=7152.96
model_510_7<-arima(t_validation,order=c(5,1,0),seasonal=list(order=c(1,1,1),period=7))#AIC=7099.46
model_710_7<-arima(t_validation,order=c(7,1,0),seasonal=list(order=c(1,1,1),period=7))#AIC=7090.51
auto.arima(t_validation)

model_110_7$aic
model_210_7$aic
model_310_7$aic
model_510_7$aic
model_710_7$aic
model_710_7$aic
auto.arima(t_validation)$aic

help(arima)


arima.best <- function(x, maxord = c(1,1,1))
{
  start <- Sys.time()
  best.aic <- 1e8
  nobs <- length(x)
  for (p in 0:maxord[1])
    for(d in 0:maxord[2])
      for(q in 0:maxord[3])
      {
        fit <- arima(x, order = c(p,d,q), xreg=1:nobs )
        if (fit$aic < best.aic)
        {
          best.aic <- fit$aic
          best.fit <- fit
          best.model <- c(p,d,q)
        }
      }
  z = list(model = best.model, fitted = best.fit, AIC = best.aic, Elapsed = Sys.time()-start);
  return(z)
}
arima.best(t_validation) #最优是0，1，1 AIC=7447.77

#比较AIC最小的是7，1，0 周期为7

#诊断检验
fit1<-arima(t_test,order=c(7,1,0),seasonal=list(order=c(1,1,1),period=7))
fit1$sigma2
tsdiag(fit1)

#利用选择的arima（7，1，0）（1，1，1）[7]来预测
f.p1<-forecast(fit1,h=7,level=c(99.5))
par(mfrow=c(1,1))
plot(f.p1)
f.p1

#回归方法
x_validation<-1:1023
lm( v_validation~ x_validation)
lm.r<-lm( v_validation~ x_validation)
anova(lm.r) #方差分析假设：时间对病人数没有显著影响 p值小于0.05拒绝了
summary(lm.r)
#F检验：线性关系不显著 t检验：系数为0 都被拒绝

plot(lm.r)

par(mfrow=c(1,1))
plot(x_validation, v_validation)
abline(lm.r,col='blue')
new_x_validation<-1024:1461
y_test_fitted<-predict(lm.r, data.frame(x_validation = new_x_validation), level = 0.95)
err_reg<-v[1024:1461]-y_test_fitted
mean(err_reg^2)                 # MSE

lm.r2<-lm( v_validation~ x_validation+ I(x_validation^2))
summary(lm.r2)


#总结对比MSE
mean(errNF^2)                    # naive method MSE
mean(err^2,na.rm=T)              # Simple averages method MSE
MSE_t_test                       # Single exponential smoothing method MSE
MSE_t_test_hl                    # Holt’s Linear Method MSE
MSE_t_test_hwa                   # Holt Winters’ Additive Method MSE
MSE_t_test_hwm                   # Holt Winters’ Multiplicative Method MSE
mean(err_reg^2)                  # linear regression method MSE
