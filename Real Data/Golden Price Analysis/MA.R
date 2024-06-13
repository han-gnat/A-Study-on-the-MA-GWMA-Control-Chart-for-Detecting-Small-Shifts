library(readr)
library(ggplot2)

Golden <- read_csv("price.csv")
price <- Golden$price
Date <- as.Date(Golden$Date)

x <- price
mu <- mean(x[1:337])
sigma <- sd(x)
n <- length(x)

window=3
L=2.949

RL=wt=vj=c()
for(i in 1:n){ 
  if(i >= window){
    wt[i] = 1 / window
  }
  else{
    wt[i] = 1 / i
  }
}
for(j in 1:n){ 
  if(j >= window){
    vj[j] = (sigma^2) / window
  }
  else{
    vj[j] = (sigma^2) / j 
  }
}

ma=ucl=lcl=c()

for(j in 1:n){
  if(j < window){
    ma[j] = mean(x[1:j])
  }
  else{
    ma[j] = mean(x[(j-window+1):j])
  }
  ucl[j]=mu+L*sqrt(vj[j])
  lcl[j]=mu-L*sqrt(vj[j])
}

exceed_indices <- which(ma > ucl)[1]
print(exceed_indices)

ma_df <- data.frame(Date = Date, MA = ma, UCL = ucl, LCL = lcl)

ggplot(ma_df, aes(x = Date)) +
  geom_line(aes(y = MA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "MA", title = "MA Control Chart") +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = ma_df[exceed_indices, ], aes(x = Date, y = MA), color = "blue", size = 1.5) +
  geom_text(data = ma_df[exceed_indices, ], aes(x = Date, y = MA, label = format(Date, "%Y-%m")), vjust = -1.5,hjust=0.95, color = "blue",size=3.3)

