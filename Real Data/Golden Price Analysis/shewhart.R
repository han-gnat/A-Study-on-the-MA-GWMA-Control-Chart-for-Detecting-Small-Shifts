library(readr)
library(ggplot2)

Golden <- read_csv("price.csv")
price <- Golden$price
Date <- as.Date(Golden$Date)

x <- price
mu <- mean(x[1:337])
sigma <- sd(x)
n <- length(x)

q <- 0
alpha <- 1
L <- 3

wt <- vj <- c()
for(i in 1:n) {
  wt[i] <- q^((i-1)^alpha)-q^(i^alpha)
}
for(j in 1:n) {
  vj[j] <- sum(wt[1:j]*wt[1:j])*(sigma^2)
}

shewhart <- ucl <- lcl <- c()
for(j in 1:n) {
  zsum <- sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  shewhart[j] <- zsum
  ucl[j] <- mu + L*sqrt(vj[j])
  lcl[j] <- mu - L*sqrt(vj[j])
}

exceed_indices <- which(shewhart > ucl)[1]
print(exceed_indices)

shewhart_df <- data.frame(Date = Date, Shewhart = shewhart, UCL = ucl, LCL = lcl)

ggplot(shewhart_df, aes(x = Date)) +
  geom_line(aes(y = Shewhart), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "Shewhart", title = "Shewhart Control Chart") +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = shewhart_df[exceed_indices, ], aes(x = Date, y = Shewhart), color = "blue", size = 1.5) +
  geom_text(data = shewhart_df[exceed_indices, ], aes(x = Date, y = Shewhart, label = format(Date, "%Y-%m")), vjust = -1,hjust=0.4, color = "blue",size=3.3)

