library(readr)
library(ggplot2)

# Load data
Global <- read_csv("Global.csv")
Global <- Global[841:nrow(Global),]

month_map <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
               "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)

Global$Month <- month_map[Global$Month]
Global$Date <- as.Date(paste(Global$Year, Global$Month, "1", sep = "-"))

x <- Global$value
mu <- 0
sigma <- sd(x)
n <- length(x)

# set parameter
q <- 0.8
alpha <- 0.5
window <- 3
L <- 3.826

# weight of GWMA
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

# compute statistic of MA-GWMA , UCL and LCL
ma_gwma = ucl = lcl = y = c()
for(j in 1:n){
  zsum = sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  y = c(y, zsum)
  if(j < window){
    ma = mean(y[1:j])
  }
  else{
    ma = mean(y[(j-window+1):j])
  }
  ma_gwma[j] = ma
  ucl[j] = mu+L*sqrt(vj[j])
  lcl[j] = mu-L*sqrt(vj[j])
}

# Find the first index that exceeds the control limit
exceed_indices <- which(ma_gwma > ucl)[1]

ma_gwma_df <- data.frame(Date = Global$Date, MA_GWMA = ma_gwma, UCL = ucl, LCL = lcl)

# plot result
ggplot(ma_gwma_df, aes(x = Date)) +
  geom_line(aes(y = MA_GWMA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "MA-GWMA", title = "MA-GWMA Control Chart") +
  theme_bw() + 
  theme(panel.grid=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data = ma_gwma_df[exceed_indices, ], aes(x = Date, y = MA_GWMA, label = format(Date, "%Y-%m")), vjust = -1.5,hjust=0.7, color = "blue",size = 3.3)+
  geom_point(data = ma_gwma_df[exceed_indices, ], aes(x = Date, y = MA_GWMA), color = "blue", size = 1.5)
