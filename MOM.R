library(readr)
library(xts)
library(dygraphs)
library(rutils)
library(moments)
library(roll)

setwd("C:/Users/admin/Desktop/Temp")
SP500_070101190919 <- read_csv("SP500_070101190919.csv")
head(SP500_070101190919)
SP500_ohlcv <- xts(SP500_070101190919[, -1], order.by = SP500_070101190919$Date)
  plot(SP500_ohlcv$`Adj Close`, ylim = c(0, 3500))

start_date <- index(SP500_ohlcv)[1]
end_date <- index(SP500_ohlcv)[dim(SP500_ohlcv)[1]]

risk_free_rate <- read_csv("risk-free-rate.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
risk_free_rate <- risk_free_rate$value/100
risk_free_rate <- risk_free_rate[1:length(SP500_ohlcv$`Adj Close`)]
risk_free_rate <- xts(risk_free_rate, order.by = index(SP500_ohlcv$`Adj Close`))
plot(risk_free_rate, ylim = c(0, max(risk_free_rate)*1.2))

### Calculate PnLs and SR by simple moving average --------------------------------------------
simple_ma <- function(xts_ohlcv, s, t){
  AdjCl <- xts_ohlcv$`Adj Close`
  Op <- xts_ohlcv$Open
  
  MA_s <- filter(AdjCl, filter = rep(1/s, s), sides = 1)
  MA_t <- filter(AdjCl, filter = rep(1/t, t), sides = 1)
  
  # Count from the 121st days
  Op <- Op[121:length(Op)]
  AdjCl <- AdjCl[121:length(AdjCl)]
  MA_s <- MA_s[121:length(MA_s)]
  MA_t <- MA_t[121:length(MA_t)]
  
  indic <- sign(MA_s - MA_t)
  trade_dates <- c(2, which(diff_it(indic) != 0) + 1)
  trade_dates <- trade_dates[trade_dates!=(length(AdjCl)+1)] # Can't exceed the last day
  trade_YoN <- rep(0, length(AdjCl))
  trade_YoN[trade_dates] <- 1
  
  positions <- rep(NA_integer_, length(AdjCl))
  positions[1] <- 0
  positions[trade_dates] <- indic[trade_dates]
  positions <- na.locf(positions)
  positions <- xts(positions, order.by = index(AdjCl))
  
  pos_lag <- lag_it(positions)
  AdjCl_lag <- lag_it(AdjCl)
  
  pnls <- lag_it(positions) * (AdjCl - AdjCl_lag)
  pnls[trade_dates] <- 
    pos_lag[trade_dates] * (Op[trade_dates] - AdjCl_lag[trade_dates]) + 
    positions[trade_dates] * (AdjCl[trade_dates] - Op[trade_dates])
  
  # Calculate annualized Sharpe Ratio
  sr <- sqrt(252) * sum(pnls) / NROW(pnls) / sd(pnls)
  
  # Calculate cumulative pnls
  cum_pnls <- xts(cumsum(pnls), order.by=index(AdjCl))
  
  output <- list(sr = sr, cum_pnls = cum_pnls, positions = positions, pnls = pnls)
  output
}


### Calculate PnLs and SR by VWAP and simple moving average -----------------------------------
vwap_ma <- function(xts_ohlcv, vo_lookback, s, t){
  AdjCl <- xts_ohlcv$`Adj Close`
  Op <- xts_ohlcv$Open
  Volume <- xts_ohlcv$Volume
  
  vwap_AdjCl <- roll_sum(AdjCl * Volume, width = vo_lookback) / roll_sum(Volume, width = vo_lookback)
  vwap_AdjCl <- na.locf(vwap_AdjCl, fromLast = TRUE)
  
  # Calculate MA based on vwap AdjCl
  MA_s <- filter(vwap_AdjCl, filter = rep(1/s, s), sides = 1)
  MA_t <- filter(vwap_AdjCl, filter = rep(1/t, t), sides = 1)
  
  # Count from the 121st days
  Op <- Op[121:length(Op)]
  AdjCl <- AdjCl[121:length(AdjCl)]
  MA_s <- MA_s[121:length(MA_s)]
  MA_t <- MA_t[121:length(MA_t)]
  
  # Calculate trade signal and pnls
  indic <- sign(MA_s - MA_t)
  trade_dates <- c(2, which(diff_it(indic) != 0) + 1)
  trade_dates <- trade_dates[trade_dates!=(length(AdjCl)+1)] # Can't exceed the last day
  trade_YoN <- rep(0, length(AdjCl))
  trade_YoN[trade_dates] <- 1
  
  positions <- rep(NA_integer_, length(AdjCl))
  positions[1] <- 0
  positions[trade_dates] <- indic[trade_dates]
  positions <- na.locf(positions)
  positions <- xts(positions, order.by = index(AdjCl))
  
  pos_lag <- lag_it(positions)
  AdjCl_lag <- lag_it(AdjCl)
  
  pnls <- lag_it(positions) * (AdjCl - AdjCl_lag)
  pnls[trade_dates] <- 
    pos_lag[trade_dates] * (Op[trade_dates] - AdjCl_lag[trade_dates]) + 
    positions[trade_dates] * (AdjCl[trade_dates] - Op[trade_dates])
  
  # Calculate annualized Sharpe Ratio
  sr <- sqrt(252) * sum(pnls) / NROW(pnls) / sd(pnls)
  
  # Calculate cumulative pnls
  cum_pnls <- xts(cumsum(pnls), order.by=index(AdjCl))
  
  output <- list(sr = sr, cum_pnls = cum_pnls, positions = positions, pnls = pnls)
  output
}


### Plug in different MA time periods and get top 5 combos with highest SR ---------------------------
s_values <- c(10, 20, 30, 40, 60, 90)
t_values <- c(20, 30, 40, 60, 90, 120)

best_sr <- function(xts_ohlcv, s_array, t_array, top_n, lookback){
  all_sr <- matrix(NA, nrow = length(s_array), ncol = length(t_array))
  for (i in 1:length(s_values)){
    for (j in 1:length(t_values)){
      if (t_values[j] > s_values[i]){
        if (missing(lookback)) {
          all_sr[i,j] <- simple_ma(xts_ohlcv = SP500_ohlcv, s = s_values[i], t = t_values[j])$sr
        } else {
          all_sr[i,j] <- vwap_ma(xts_ohlcv = SP500_ohlcv, 
                                 s = s_values[i], t = t_values[j], vo_lookback = lookback)$sr
        }
      }
    }
  }
  
  top_sr <- sort(all_sr[!is.na(all_sr)], decreasing = TRUE)[1:top_n]
  ma_index <- sapply(1:top_n, function(iter) 
    which(all_sr == top_sr[iter], arr.ind=TRUE))
  ma_periods <- t(rbind(s_array[ma_index[1,]], t_array[ma_index[2,]]))
  
  output <- cbind(round(top_sr, 4), ma_periods)
  colnames(output) <- c("SR", "MA_s", "MA_t")
  output
}


### Compile best results
simpleMA_best <- best_sr(SP500_ohlcv, s_values, t_values, 5)
simpleMA_best
#          SR MA_s MA_t
# [1,] 0.3300   40  120
# [2,] 0.2817   20   60
# [3,] 0.2730   30  120
# [4,] 0.2485   90  120
# [5,] 0.2464   60   90


VWAP_simpleMA_best <- best_sr(SP500_ohlcv, s_values, t_values, 5, lookback = 10)
VWAP_simpleMA_best
#          SR MA_s MA_t
# [1,] 0.3971   40  120
# [2,] 0.3753   30  120
# [3,] 0.2861   60  120
# [4,] 0.2310   90  120
# [5,] 0.2175   40   90

### SP500 & MA Plot  --------------------------------------------------------------------
plot_ma <- function(xts_ohlcv, s, t){
  AdjCl <- xts_ohlcv$`Adj Close`
  
  MA_s <- filter(AdjCl, filter = rep(1/s, s), sides = 1)
  MA_t <- filter(AdjCl, filter = rep(1/t, t), sides = 1)
  
  AdjCl <- AdjCl[121:length(AdjCl)]
  MA_s <- MA_s[121:length(MA_s)]
  MA_t <- MA_t[121:length(MA_t)]
  
  SP500_MA <- cbind(AdjCl, MA_s, MA_t)
  colnames(SP500_MA) <- c("Adj.Cl", paste0("MA", s), paste0("MA", t))
  
  dygraphs::dygraph(SP500_MA, main = "SP500 Adj.Close & MA") %>% 
    dyOptions(colors=c("black","red","blue"), strokeWidth = 2) %>% 
    dyLegend(show="always")
}

choose_best <- 1
plot_ma(SP500_ohlcv, s = simpleMA_best[choose_best, 2], t = simpleMA_best[choose_best, 3])



### Simpla MA PnL Plot --------------------------------------------------------------------------
choose_best <- 1
AdjCl_st0 <- SP500_ohlcv$`Adj Close`[-(1:120),] - as.numeric(SP500_ohlcv$`Adj Close`[121,])

### Simple MA Best SR PnLs
### Run below as a whole
chart_Series(x = AdjCl_st0, name="PnLs", col="orange")
add_TA(simple_ma(SP500_ohlcv, simpleMA_best[choose_best, 2], simpleMA_best[choose_best, 3])$cum_pnls, 
       on=1, lwd=2, col="blue")
add_TA(simple_ma(SP500_ohlcv, simpleMA_best[choose_best, 2], simpleMA_best[choose_best, 3])$positions == 1,
       on=-1, col="lightgreen", border="lightgreen")
add_TA(simple_ma(SP500_ohlcv, simpleMA_best[choose_best, 2], simpleMA_best[choose_best, 3])$positions == -1,
       on=-1, col="lightgrey", border="lightgrey")
legend("topleft", 
       legend=c("SP500 Adj.Cl", 
              paste0("MOM", simpleMA_best[choose_best, 2], "_", simpleMA_best[choose_best, 3], " Strat")),
       inset=0.1, bg="white", lty=1, lwd=6,
       col=c("orange", "blue"), bty="n")

### VWAP MA Best SR PnLs
### Run below as a whole
chart_Series(x = AdjCl_st0, name="PnLs", col="orange")
add_TA(vwap_ma(SP500_ohlcv, VWAP_simpleMA_best[choose_best, 2], 
               VWAP_simpleMA_best[choose_best, 3], vo_lookback = 10)$cum_pnls, 
       on=1, lwd=2, col="blue")
add_TA(vwap_ma(SP500_ohlcv, VWAP_simpleMA_best[choose_best, 2], 
               VWAP_simpleMA_best[choose_best, 3], vo_lookback = 10)$positions == 1, 
       on=-1, col="lightgreen", border="lightgreen")
add_TA(vwap_ma(SP500_ohlcv, VWAP_simpleMA_best[choose_best, 2], 
               VWAP_simpleMA_best[choose_best, 3], vo_lookback = 10)$positions == -1, 
       on=-1, col="lightgrey", border="lightgrey")
legend("topleft", 
       legend=c("SP500 Adj.Cl", 
                paste0("MOM_vwap", VWAP_simpleMA_best[choose_best, 2], "_", 
                       VWAP_simpleMA_best[choose_best, 3], " Strat")),
       inset=0.1, bg="white", lty=1, lwd=6,
       col=c("orange", "blue"), bty="n")




### sp500 daily percentage return
SP500_returns <- log(SP500_ohlcv$`Adj Close`/lag_it(SP500_ohlcv$`Adj Close`))[-1]
rbind(head(SP500_returns), tail(SP500_returns))
#                Adj Close
# 2007-01-04  0.0012275323
# 2007-01-05 -0.0061031679
# 2007-01-08  0.0022178572
# 2007-01-09 -0.0005168099
# 2007-01-10  0.0019384723
# 2007-01-11  0.0063198611
# 2019-09-11  0.0072036723
# 2019-09-12  0.0028750160
# 2019-09-13 -0.0007246766
# 2019-09-16 -0.0031405130
# 2019-09-17  0.0025784253
# 2019-09-18  0.0003426332

# sp500 rolling n-days percentage return
roll_width <- 252
sp500_roll_ret <- diff_it(SP500_ohlcv$`Adj Close`, lagg = roll_width) / 
  lag_it(SP500_ohlcv$`Adj Close`, lagg = roll_width)
# plot(sp500_roll_ret, main = paste0("sp500_roll", roll_width, "_ret"))

# sp500 rolling 90-day volatility (annualized)
sp500_rollvar <- roll::roll_var(SP500_returns, width = 90)
sp500_rollvar[is.na(sp500_rollvar)] <- 0
sp500_rollsd <- sqrt(sp500_rollvar) * sqrt(252)
plot(sp500_rollsd, main = paste0("sp500_90d.ann.sd"))

# sp500 rolling SR
sp500_roll_sr <- sp500_roll_ret / sp500_rollsd
colnames(sp500_roll_sr) <- "SP500 rollSR"
plot(sp500_roll_sr, main = paste0("sp500_roll", roll_width, "_SR"))


### Simple MA PnLs analysis
simpleMA_pnls <- matrix(NA, nc = 5, nr = length(SP500_ohlcv$`Adj Close`) - 120)

for (i in 1:5){
  simpleMA_pnls[, i] <- simple_ma(SP500_ohlcv, simpleMA_best[i, 2], simpleMA_best[i, 3])$pnls
}

simpleMA_pnls <- xts(simpleMA_pnls, order.by = index(SP500_ohlcv[-(1:120),]))[-1,]
colnames(simpleMA_pnls) <- sapply(1:5, function(i) 
  paste0("MA", simpleMA_best[i, 2], "_", simpleMA_best[i, 3]))

# rolling SR
roll_width <- 252
simpleMA_rollSR <- roll_sum(simpleMA_pnls, width = roll_width) / roll_width / 
  roll_sd(simpleMA_pnls, width = roll_width) * sqrt(252)
plot(simpleMA_rollSR, lwd = 1, legend.loc = "topleft")

plot(cbind(simpleMA_rollSR[,1], sp500_roll_sr), legend.loc = "topleft", main = "1 year rolling SR")

# Normalize pnls and check skewness and kurtosis
simpleMA_Npnls <- apply(simpleMA_pnls, 2, function(x) (x-mean(x))/sd(x))
simpleMA_Npnls_stats <- apply(simpleMA_Npnls, 2, function(x) cbind(mean(x), sd(x), skewness(x), kurtosis(x)))
simpleMA_Npnls_stats <- round(simpleMA_Npnls_stats, 4)
simpleMA_Npnls_stats

#      MA40_120 MA20_60 MA30_120 MA90_120 MA60_90
# [1,]   0.0000  0.0000   0.0000   0.0000  0.0000
# [2,]   1.0000  1.0000   1.0000   1.0000  1.0000
# [3,]  -0.3496 -0.2173  -0.3646  -0.4884 -0.3246
# [4,]   7.6973  7.6699   7.6650   7.7036  7.6878


### VWAP MA rolling SR
VWAP_simpleMA_pnls <- matrix(NA, nc = 5, nr = length(SP500_ohlcv$`Adj Close`) - 120)

for (i in 1:5){
  VWAP_simpleMA_pnls[, i] <- vwap_ma(SP500_ohlcv, simpleMA_best[i, 2], simpleMA_best[i, 3], 
                                     vo_lookback = 10)$pnls
}

VWAP_simpleMA_pnls <- xts(VWAP_simpleMA_pnls, order.by = index(SP500_ohlcv[-(1:120),]))[-1,]
colnames(VWAP_simpleMA_pnls) <- sapply(1:5, function(i) 
  paste0("MA", simpleMA_best[i, 2], "_", simpleMA_best[i, 3]))

VWAP_simpleMA_rollSR <- roll_sum(VWAP_simpleMA_pnls, width = roll_width) / roll_width / 
  roll_sd(VWAP_simpleMA_pnls, width = roll_width) * sqrt(252)
plot(VWAP_simpleMA_rollSR, lwd = 1, legend.loc = "topleft")

plot(cbind(VWAP_simpleMA_rollSR[,1], sp500_roll_sr), legend.loc = "topleft", 
     main = "1 year rolling SR by VWAP")




### BSM Call Put Price
bsm_cp <- function(S, K, r, sigma, tau, cp){
  tau <- tau/252
  d1 <- (log(S/K) + (r + sigma^2)/2 * tau) / (sigma * sqrt(tau))
  d2 <- d1 - sigma * sqrt(tau)
  price <- cp * (S * pnorm(cp*d1) - K * exp(-r*tau) * pnorm(cp*d2))
  price
}
# bsm_cp(S = 100, K = 100, r = 0.02, sigma = 0.15, t = 90/252, cp = 1)


### Daily call/put trading strategy: Long call or put at day opens. Sell it at day closes.
daily_cp <- function(xts_ohlcv, xts_rf, xts_vol, s, t){
  AdjCl <- xts_ohlcv$`Adj Close`
  Op <- xts_ohlcv$Open
  rf <- as.numeric(xts_rf) # have to do this. Same length. Assume dates align.
  vol <- as.numeric(xts_vol) # same reason.
  
  MA_s <- filter(AdjCl, filter = rep(1/s, s), sides = 1)
  MA_t <- filter(AdjCl, filter = rep(1/t, t), sides = 1)
  
  # Count from the 121st days
  Op <- Op[120:length(Op)]
  AdjCl <- AdjCl[120:length(AdjCl)]
  MA_s <- MA_s[120:length(MA_s)]
  MA_t <- MA_t[120:length(MA_t)]
  rf <- rf[120:length(rf)]
  vol <- vol[119:length(vol)]
  
  indic <- sign(MA_s - MA_t)
  
  pnls <- rep(NA_integer_, length(AdjCl))
  pnls[1] <- 0
  pnls[2:length(AdjCl)] <- sapply(2:length(AdjCl), function(d){
    bsm_cp(S = AdjCl[d], K = 5*round(Op[d]/5), r = rf[d], sigma = vol[d], 
           tau = (s+t)/2, cp = indic[d]) -
    bsm_cp(S = Op[d], K = 5*round(Op[d]/5), r = rf[d], sigma = vol[d-1], 
           tau = (s+t)/2, cp = indic[d])})
  
  # Calculate annualized Sharpe Ratio
  sr <- sqrt(252) * sum(pnls) / NROW(pnls) / sd(pnls)
  
  # Calculate cumulative pnls
  cum_pnls <- xts(cumsum(pnls), order.by=index(AdjCl))
  
  output <- list(sr = sr, cum_pnls = cum_pnls, indic = indic, pnls = pnls)
  output
}

# Examples
# daily_cp(xts_ohlcv = SP500_ohlcv, xts_rf = risk_free_rate, xts_vol = sp500_rollsd, s = 60, t = 120)$sr
# daily_cp_pnls <- daily_cp(xts_ohlcv = SP500_ohlcv, xts_rf = risk_free_rate, xts_vol = sp500_rollsd, 
#                           s = 60, t = 120)$cum_pnls
# plot(daily_cp_pnls)


# All SR for combinations of s and t
dailycp_allsr <- matrix(NA, nrow = length(s_values), ncol = length(t_values))
for (i in 1:length(s_values)){
  for (j in 1:length(t_values)){
    if (t_values[j] > s_values[i]){
      dailycp_allsr[i, j] <- daily_cp(SP500_ohlcv, risk_free_rate, sp500_rollsd,
                                      s_values[i], t_values[j])$sr
    }
  }
}
colnames(dailycp_allsr) <- paste0("MA", t_values)
rownames(dailycp_allsr) <- paste0("MA", s_values)
dailycp_allsr
# 
#          MA20     MA30      MA40      MA60      MA90     MA120
# MA10 1.636086 1.629390 1.3993189 1.2077416 1.1264405 0.9272158
# MA20       NA 1.597224 1.3132654 1.3905679 0.9268716 0.7269863
# MA30       NA       NA 0.9323397 0.9174019 0.7015881 0.7614292
# MA40       NA       NA        NA 0.8612109 0.7424082 0.8427724
# MA60       NA       NA        NA        NA 0.7283756 0.7222919
# MA90       NA       NA        NA        NA        NA 0.6574844

sort_matrix <- function(matrix, top_n){
  top <- sort(matrix[!is.na(matrix)], decreasing = TRUE)[1:top_n]
  index <- sapply(1:top_n, function(iter) which(matrix == top[iter], arr.ind=TRUE))
  
  s_lookup <- as.numeric(gsub("[^[:digit:]]", "", rownames(matrix)))
  t_lookup <- as.numeric(gsub("[^[:digit:]]", "", colnames(matrix)))
  ma_periods <- t(rbind(s_lookup[index[1,]], t_lookup[index[2,]]))
  
  output <- cbind(round(top, 4), ma_periods)
  colnames(output) <- c("SR", "MA_s", "MA_t")
  output
}

sort_matrix(matrix = dailycp_allsr, top_n = 10)
#           SR MA_s MA_t
#  [1,] 1.6361   10   20
#  [2,] 1.6294   10   30
#  [3,] 1.5972   20   30
#  [4,] 1.3993   10   40
#  [5,] 1.3906   20   60
#  [6,] 1.3133   20   40
#  [7,] 1.2077   10   60
#  [8,] 1.1264   10   90
#  [9,] 0.9323   30   40
# [10,] 0.9272   10  120

daily_cp_pnls <- daily_cp(xts_ohlcv = SP500_ohlcv, xts_rf = risk_free_rate, xts_vol = sp500_rollsd, 
                          s = 60, t = 120)$cum_pnls
long_cp <- xts(daily_cp(xts_ohlcv = SP500_ohlcv, xts_rf = risk_free_rate, xts_vol = sp500_rollsd, 
                    s = 60, t = 120)$indic, order.by = index(daily_cp_pnls))
plot(daily_cp_pnls)

### Run below as a whole
chart_Series(x = AdjCl_st0, name="PnLs", col="orange")
add_TA(daily_cp_pnls, on=1, lwd=2, col="blue")
add_TA(long_cp == 1, on=-1, col="lightgreen", border="lightgreen")
add_TA(long_cp == -1, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", 
       legend=c("SP500 Adj.Cl", "LongCP_MA60&120"),
       inset=0.1, bg="white", lty=1, lwd=6,
       col=c("orange", "blue"), bty="n")

