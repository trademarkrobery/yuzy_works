library("quantmod") 
getwd()
setwd("C:/Users/Yuzi/OneDrive/2019(Spring)/時系列分析/WorkSpace")
getSymbols("JPNNGDP", src = "FRED")
getSymbols("AAPL", from = "2010-01-01")

# 原系列
plot(JPNNGDP) # 時系列プロット
acf(JPNNGDP) # コレログラム
Box.test (JPNNGDP, lag = 1, type = "Ljung") # かばん検定

# 成長率
jpdgp.r <- periodReturn(JPNNGDP) * 100
plot(jpdgp.r); acf(jpdgp.r)
Box.test (jpdgp.r, lag = 5, type = "Ljung")
Box.test (jpdgp.r, lag = 10, type = "Ljung")

aapl <- AAPL[,6] # 修正後終値
plot(aapl); acf(aapl)
Box.test (aapl, lag = 10, type = "Ljung")

# 日次対数収益率
aapl.dr <- dailyReturn(aapl, type = 'log')
plot(aapl.dr); acf(aapl.dr)
Box.test (aapl.dr, lag = 5, type = "Ljung")
Box.test (aapl.dr, lag = 10, type = "Ljung")
Box.test (aapl.dr, lag = 20, type = "Ljung")

ma11 <- arima.sim(list(order = c(0,0,1), ma = 0.8), n = 200)
ma12 <- 2 + arima.sim(list(order = c(0,0,1), ma = 0.5), sd = 0.5, n = 200)
ma13 <- -2 + arima.sim(list(order = c(0,0,1), ma = 0.3), sd = 2, n = 200)
ma14 <- arima.sim(list(order = c(0,0,1), ma = -0.3), n = 200)
ma15 <- 2 + arima.sim(list(order = c(0,0,1), ma = -0.5), sd = 0.5, n = 200)
ma16 <- -2 + arima.sim(list(order = c(0,0,1), ma = -0.8), sd = 2, n = 200)

par(mfrow = c(3, 2)) # プロット画面を3行2列に分割
plot(ma11); plot(ma12); plot(ma13); plot(ma14); plot(ma15); plot(ma16)
