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
L=2.605

wt=vj=c()
for(i in 1:n){wt[i] = q^((i-1)^alpha)-q^(i^alpha)}
for(j in 1:n){vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2)}

gwma=ucl=lcl=c()

for(j in 1:n){
  zsum = sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  gwma[j]=zsum
  ucl[j]=mu+L*sqrt(vj[j])
  lcl[j]=mu-L*sqrt(vj[j])
}

exceed_indices <- which(gwma > ucl)[1]
print(exceed_indices)


gwma_df <- data.frame(Date = Date, GWMA = gwma, UCL = ucl, LCL = lcl)

ggplot(gwma_df, aes(x = Date)) +
  geom_line(aes(y = GWMA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "GWMA", title = "GWMA Control Chart") +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = gwma_df[exceed_indices, ], aes(x = Date, y = GWMA), color = "blue", size = 1.5) +
  geom_text(data = gwma_df[exceed_indices, ], aes(x = Date, y = GWMA, label = format(Date, "%Y-%m")), vjust = -1,hjust=0.95, color = "blue",size=3.3)



