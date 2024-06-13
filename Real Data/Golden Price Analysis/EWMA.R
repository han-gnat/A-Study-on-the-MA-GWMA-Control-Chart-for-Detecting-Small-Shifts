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
L = 2.8643

# weight of EWMA
wt=vj=c()
for(i in 1:n){wt[i] = q^((i-1)^alpha)-q^(i^alpha)}
for(j in 1:n){vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2)}

# compute statistic of EWMA, UCL and LCL
ewma = ucl = lcl = c()
for(j in 1:n){
  zsum = sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  ewma[j] = zsum
  ucl[j] = mu+L*sqrt(vj[j])
  lcl[j] = mu-L*sqrt(vj[j])
}

# Find the first index that exceeds the control limit
exceed_indices <- which(ewma > ucl)[1]

ewma_df <- data.frame(Date = Date, EWMA = ewma, UCL = ucl, LCL = lcl)

# plot result
ggplot(ewma_df, aes(x = Date)) +
  geom_line(aes(y = EWMA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "EWMA", title = "EWMA Control Chart") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = ewma_df[exceed_indices, ], aes(x = Date, y = EWMA), color = "blue", size = 1.5) +
  geom_text(data = ewma_df[exceed_indices, ], aes(x = Date, y = EWMA, label = format(Date, "%Y-%m")), vjust = -1, hjust = 0.95, color = "blue", size = 3.3)

