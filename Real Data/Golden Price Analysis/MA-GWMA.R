library(readr)
library(ggplot2)

Golden <- read_csv("price.csv")
price <- Golden$price
Date <- as.Date(Golden$Date)

x <- price
mu <- mean(x[1:337])
sigma <- sd(x)
n <- length(x)

q=0.8
alpha=0.5
window=3
L=3.626

wt=vj=c()
for(i in 1:n){ wt[i] = q^((i-1)^alpha)-q^(i^alpha) }
for(j in 1:n){
  if(j >= window){
    vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2) / window
  }
  else{
    vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2) / j 
  }
}

ma_gwma=ucl=lcl=y=c()
for(j in 1:n){
  zsum = sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  y = c(y, zsum)
  if(j < window){
    ma = mean(y[1:j])
  }
  else{
    ma = mean(y[(j-window+1):j])
  }
  ma_gwma[j]=ma
  ucl[j]=mu+L*sqrt(vj[j])
  lcl[j]=mu-L*sqrt(vj[j])
}

exceed_indices <- which(ma_gwma > ucl)[1]
print(exceed_indices)

ma_gwma_df <- data.frame(Date = Date, MA_GWMA = ma_gwma, UCL = ucl, LCL = lcl)

ggplot(ma_gwma_df, aes(x = Date)) +
  geom_line(aes(y = MA_GWMA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "MA-GWMA", title = "MA-GWMA Control Chart") +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = ma_gwma_df[exceed_indices, ], aes(x = Date, y = MA_GWMA), color = "blue", size = 1.5) +
  geom_text(data = ma_gwma_df[exceed_indices, ], aes(x = Date, y = MA_GWMA, label = format(Date, "%Y-%m")), vjust = -1,hjust=0.8, color = "blue",size=3.3)
