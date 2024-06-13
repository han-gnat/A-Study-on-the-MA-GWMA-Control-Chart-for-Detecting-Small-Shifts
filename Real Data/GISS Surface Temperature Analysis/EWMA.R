library(readr)
Global <- read_csv("Global.csv")
Global <- Global[841:nrow(Global),]

month_map <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
               "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)

Global$Month <- month_map[Global$Month]

Global$Date <- as.Date(paste(Global$Year, Global$Month, "1", sep = "-"))

x = Global$value
mu = 0
sigma=sd(x)
n=length(x)
q=0.8
alpha=1
L=2.8643

wt=vj=c()
for(i in 1:n){wt[i] = q^((i-1)^alpha)-q^(i^alpha)}
for(j in 1:n){vj[j] = sum(wt[1:j]*wt[1:j])*(sigma^2)}

ewma=ucl=lcl=c()

for(j in 1:n){
  zsum = sum(wt[1:j]*x[j:1]) + mu*(q^(j^alpha))
  ewma[j]=zsum
  ucl[j]=mu+L*sqrt(vj[j])
  lcl[j]=mu-L*sqrt(vj[j])
}

exceed_indices <- which(ewma > ucl)[1]
print(exceed_indices)

library(ggplot2)

ewma_df <- data.frame(Date = Global$Date, EWMA=ewma, UCL = ucl, LCL = lcl)

ggplot(ewma_df, aes(x = Date)) +
  geom_line(aes(y = EWMA), color = "black") +
  geom_line(aes(y = UCL), color = "red", linetype = "solid") +
  geom_line(aes(y = LCL), color = "red", linetype = "solid") +
  labs(x = "Date", y = "EWMA", title = "EWMA Control Chart") +
  theme_bw() + 
  theme(panel.grid=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data = ewma_df[exceed_indices, ], aes(x = Date, y = EWMA, label = format(Date, "%Y-%m")), vjust = -1.8,hjust=0.7, color = "blue",size=3.3)+
  geom_point(data = ewma_df[exceed_indices, ], aes(x = Date, y = EWMA), color = "blue", size = 1.5)

