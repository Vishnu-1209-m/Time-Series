rm(list = ls())
df = read.csv('Time_Series_data.csv')
df
View(df)
#plot((1:200), df$Average.Sale[1:200], type = 'l')
library(ggplot2)
library(tidyverse)
library(forecast)
library(zoo)
ndiffs(df$Average.Sale)

acf(df$Average.Sale, main = 'ACF plot of Average Daily Sale')
pacf(df$Average.Sale)

pacf(diff(df$Average.Sale, differences = 1), main = 'PACF plot for First Difference Average Sales')
acf(diff(df$Average.Sale, differences = 1), main = 'ACF plot for First Difference Average Sales')


df$Date = as.Date(df$Date, format = "%d-%m-%Y")
df$Date

df_final = data.frame(date = df$Date, Average_Sales = df$Average.Sale)
df_final

df_final = df_final %>%
  mutate(avg_sales3 = rollmean(Average_Sales, k=51, fill=NA, align='center'))


ggplot(df_final[1:346, ], aes(x=date, y=Average_Sales)) +
  geom_line(color = 'turquoise4') + 
  geom_line(data = df_final[1:346, ], aes(x = date, y = avg_sales3)) +
  labs(x="", y="Average Sales", title="Average Sale per day in for 2016-17") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

ggplot(df_final, aes(x=date, y=Average_Sales)) +
  geom_line(color = 'turquoise4') + 
  geom_line(data = df_final, aes(x = date, y = avg_sales3), size = 1) +
  labs(x="", y="Average Sales", title="Average Daily Sale from April 2016 - March 2020") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

arima_model = Arima(df_final$Average_Sales, order = c(0, 1, 2))
summary(arima_model)

residuals = resid(arima_model)
time_index <- time(df_final$Average_Sales)
plot(time_index, residuals, type = "l",
     main = "Error Values vs Time",
     xlab = "Time", ylab = "Residuals", col = "pink")
lines(x = 1:length(ma_resid), y = ma_resid, col = 'black', lwd = 2)


ma_resid = rollmean(residuals, k=101, fill=NA, align='center')
ma_resid_sd = rollapply(residuals, width = 101, FUN = sd, align = "right", fill = NA)

acf(residuals, main = 'ACF Plot of Residuals')

ggplot(df_final, aes(x=date, y=Average_Sales)) +
  geom_line(color = 'turquoise4') + 
  labs(x="", y="Average Sales", title="Average Daily Sale from April 2016 - March 2020") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

Box.test(df_final$Average_Sales, lag = 10, type = "Ljung-Box")
