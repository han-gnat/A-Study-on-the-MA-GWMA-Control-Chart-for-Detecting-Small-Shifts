library(ggplot2)
library(readr)

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
L <- 2.955

# weight of GWMA
wt = vj = c()
for(i in 1:n){wt[i] = q^((i-1)^alpha)-q^(i^alpha)}
for(j in 1:n){vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2)}

# compute statistic of GWMA, UCL and LCL
gwma = ucl = lcl = c()
for(j in 1:n){
  zsum = sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  gwma[j] = zsum
  ucl[j] = mu+L*sqrt(vj[j])
  lcl[j] = mu-L*sqrt(vj[j])
}

# Find the first index that exceeds the control limit
exceed_indices <- which(gwma > ucl)[1]

gwma_df <- data.frame(Date = Global$Date, GWMA = gwma, UCL = ucl, LCL = lcl)

# plot result
ggplot(gwma_df, aes(x = Date)) +
  geom_line(aes(y = GWMA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "GWMA", title = "GWMA Control Chart") +
  theme_bw() + 
  theme(panel.grid=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data = gwma_df[exceed_indices, ], aes(x = Date, y = GWMA, label = format(Date, "%Y-%m")), vjust = -1.9, hjust = 0.7, color = "blue", size = 3.3)+
  geom_point(data = gwma_df[exceed_indices, ], aes(x = Date, y = GWMA), color = "blue", size = 1.5)
