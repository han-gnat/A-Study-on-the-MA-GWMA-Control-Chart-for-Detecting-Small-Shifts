library(readr)
library(ggplot2)

# Load data
Golden <- read_csv("price.csv")
price <- Golden$price
Date <- as.Date(Golden$Date)

x <- price
mu <- mean(x[1:337])
sigma <- sd(x)
n <- length(x)

# set parameter
q = 0.8
alpha = 1
window = 3
L = 4.2856

# weight of MA-EMWA
wt = vj = c()
for(i in 1:n){ wt[i] = q^((i-1)^alpha)-q^(i^alpha) }
for(j in 1:n){
  if(j >= window){
    vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2) / window
  }
  else{
    vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2) / j 
  }
}

# compute statistic of MA-EWMA, UCL and LCL
ma_ewma = ucl = lcl = y = c()
for(j in 1:n){
  zsum = sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  y = c(y, zsum)
  if(j < window){
    ma = mean(y[1:j])
  }
  else{
    ma = mean(y[(j-window+1):j])
  }
  ma_ewma[j] = ma
  ucl[j] = mu+L*sqrt(vj[j])
  lcl[j] = mu-L*sqrt(vj[j])
}

# Find the first index that exceeds the control limit
exceed_indices <- which(ma_ewma > ucl)[1]

ma_ewma_df <- data.frame(Date = Date, MA_EWMA = ma_ewma, UCL = ucl, LCL = lcl)

# plot result
ggplot(ma_ewma_df, aes(x = Date)) +
  geom_line(aes(y = MA_EWMA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "MA-EWMA", title = "MA-EWMA Control Chart") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = ma_ewma_df[exceed_indices, ], aes(x = Date, y = MA_EWMA), color = "blue", size = 1.5) +
  geom_text(data = ma_ewma_df[exceed_indices, ], aes(x = Date,y = MA_EWMA, label = format(Date,"%Y-%m")), vjust = -1.2, hjust = 0.8, color = "blue", size = 3.3)
