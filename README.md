# R Packages
library('ggplot2')
library('forecast')
library('tseries')
library(e1071)
library('Metrics')
library('extrafont')
loadfonts(device = "win")
library(survminer)
library(survival)
library(hydroGOF)
library(lmtest)

# Insert Data
library(readxl)
data <- read_excel("C:/Users/coding/data.xlsx")
View(data)
plot(data,type='l')

data$Date <- as.Date(data$Date)

train <- data.frame(data[1:612,])
test <- data.frame(data[613:765,])

graph <- ggplot(data = data) +
  ggtitle("Plot Original Series of Daily New_Recovered Cases") +
  theme(text=element_text(size=12,  family="serif"))+
  geom_line(aes(Date, Covid)) + scale_x_date('month')

ggsave(
  filename = "original.png",
  plot = graph,
  width = 6,
  height = 4
)
plot(graph)

# Time series data
ts_data <- ts(train[, c('covid')])

# Check stationarity of time series data
adf.test(ts_data,"stationary")

# Turning data into stationary
ts_stationary<-diff(ts_data)
plot(ts_stationary)
adf.test(ts_stationary)

# Determine the p and q parameter
ts_data %>% diff() %>% ggtsdisplay(main="") # plot diff(),acf,pacf

# Fit best ARIMA
arima_seasonal <- auto.arima(ts_data)
arima_seasonal_pred <- fitted(arima_seasonal)

# Diagonistic Checking
res <-residuals(arima_seasonal)
res_plot <- autoplot(res) + ggtitle("Residuals from ARIMA") +
 geom_point() + xlab("Day") + ylab("") +
 theme(text = element_text(family = "Times New Roman", size = 12))
res_histogram <- gghistogram(res) + ggtitle("Histogram of residuals") +
 xlab("residuals") + theme(text = element_text(family = "Times New Roman", size = 12))
res_acf <- ggAcf(res) + ggtitle("ACF of residuals") +
 theme(text = element_text(family = "Times New Roman", size = 12))

coeftest(arima_seasonal)

# Ljung-Box test to check residuals
Box.test(res,lag = 10, fitdf = 0, type = "Lj")


# SVM model
svm_tune <- tune.svm(
  ts_data ~ data$Date,
  data = data,
  gamma = 10 ^ (-1:1),
  cost = 10 ^ (2:8),
  epsilon = seq(0, 1, 0.1),
  kernel = "radial"
)

svm_model <- svm_tune$best.model

svm_pred <- fitted(svm_model)

# Calculate residuals
arima_residuals = residuals(arima_seasonal)
plot(arima_residuals)

r=residuals(arima_seasonal)

autoplot(r)+ggtitle("Residuals from ARIMA")+geom_point()+xlab("Day")+ylab("")+ theme(text=element_text(family="Times New Roman", size=12))
gghistogram(r)+ggtitle("Histogram of residuals")+xlab("residuals")+theme(text=element_text(family="Times New Roman", size=12))
ggAcf(r)+ggtitle("ACF of residuals")+theme(text=element_text(family="Times New Roman", size=12))


# Regress ARIMA residuals using SVMs
arima_res_svm_tune <- tune.svm(
  arima_residuals ~ arima_seasonal_pred,
  data = data,
  gamma = 2 ^ (-1:1),
  cost = 2 ^ (2:11),
  epsilon = seq(0, 1, 0.1),
  kernel = "radial"
)

# Create the best SVM model for residuals
arima_res_svm = arima_res_svm_tune$best.model

# Implementation of the model - fit regressed points
arima_res_svm_pred <- predict(arima_res_svm)

# Key formula
hybrid_pred <- ts_data + arima_res_svm_pred

# Plot residuals
graph <- ggplot() +
  geom_point(aes(x = arima_seasonal_pred, y = arima_residuals, colour = "ARIMA residuals")) +
  geom_point(aes(x = arima_seasonal_pred, y = arima_res_svm_pred, colour = "SVM of residuals"))

ggsave(
  filename = "residuals_arima.png",
  plot = graph,
  width = 10,
  height = 6
)
plot(graph)
plot(residuals_arima)


# Plot the graph
label1 <- "Actual data"
label9 <- "ARIMA model"
label3 <- "SVMs"
label14 <- "ARIMA-SVMs"


graph <- ggplot(data) +
  ggtitle("                      Covid Cases") +
  theme(text=element_text(size=12,  family="serif"))+
  geom_line(aes(x = Date, y = covid, colour = label1), size = .4) +
  geom_line(aes(x = Date, y = arima_seasonal_pred, colour = label9), linetype = 2) +
  geom_point(aes(x = Date, y = arima_seasonal_pred, colour = label9), size = 0.5) +
  geom_line(aes(x = Date, y = svm_pred, colour = label3), linetype = 2) +
  geom_point(aes(x = Date, y = svm_pred, colour = label3), size = 0.5) +
  geom_line(aes(x = Date, y = hybrid_pred, colour = label14), linetype = 2) +
  geom_point(aes(x = Date, y = hybrid_pred, colour = label14), size = 0.5) +

  scale_x_date("Date") +
  ylab("covid")
graph$labels$colour <- "Legend"


ggsave(
  filename = "arima_svm_pred_2.png",
  plot = graph,
  width = 10,
  height = 6
)

plot(graph)


# Calculate MAE, MAPE, MSE, RMSE for each model
arima_mae <- mae(data$covid, arima_seasonal_pred)
arima_mape <- mape(data$covid,, arima_seasonal_pred)
arima_mse <- mse(data$covid,, arima_seasonal_pred)
arima_rmse <- rmse(data$covid,, arima_seasonal_pred)

svm_mae <- mae(data$covid,, svm_pred)
svm_mape <- mape(data$covid,, svm_pred)
svm_mse <- mse(data$covid,, svm_pred)
svm_rmse <- rmse(data$covid,, svm_pred)

hybrid_mae <- mae(data$covid,, hybrid_pred)
hybrid_mape <- mape(data$covid,, hybrid_pred)
hybrid_mse <- mse(data$covid,, hybrid_pred)
hybrid_rmse <- rmse(data$covid,, hybrid_pred)

indices <- data.frame(
  "MAE" = c(arima_mae, svm_mae, hybrid_mae),
  "MAPE" = c(arima_mape, svm_mape, hybrid_mape),
  "MSE" = c(arima_mse, svm_mse, hybrid_mse),
  "RMSE" = c(arima_rmse, svm_rmse, hybrid_rmse)
)

print(indices, class = TRUE)
