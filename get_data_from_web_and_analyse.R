library(RCurl)
library(dplyr)

x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
dat <- read.csv(text = x)

dat_country <-  dat %>%
                group_by(location) %>%
                  mutate(number_of_test_conducted = dplyr::lag(new_tests, n = 1, default = NA))

dat_country$infection_ratio <- (dat_country$new_cases/dat_country$number_of_test_conducted)*100


#### Infection Rate correlation analysis:
dat_country$date <- as.Date(dat_country$date)

summary(dat_country$date)

corr_dat <- dat_country[,c("location","date","infection_ratio")]

head(corr_dat)

library(tidyr)
data_wide <- spread(corr_dat, location, infection_ratio)
#View(data_wide)


data_wide$date <- NULL
View(cor(data_wide, use = "pairwise.complete.obs"))


library(ggplot2)
library(reshape2)
qplot(x = Var1, y = Var2,
      data = melt(cor(data_wide, use = "pairwise.complete.obs")),
      fill = value,
      geom = "tile")


selected <- c("BGD","QAT","USA","IND","PAK")

south_asia <- dat_country[dat_country$iso_code %in% selected,]

library(ggplot2)
# Multiple line plot
ggplot(south_asia, aes(x = as.Date(date), y = infection_ratio)) + 
  geom_line(aes(color = location), size = 1) +
  theme_minimal()+
  labs(x = "Date", y = "Number of infection per 100 test") 




####### Data preparation:


number_of_country <- nlevels(as.factor(dat_country$iso_code))
country <- levels(as.factor(dat_country$iso_code))
required_data <- c("location" = NULL,"population" = NULL,"day_passing" = NULL, "infection_ratio" = NULL)



for (i in 2:number_of_country) {
  
  subsetted_data <- subset(dat_country, iso_code==country[i])
  subsetted_data <- subset(subsetted_data, total_cases > 0)
  
  if (nrow(subsetted_data) > 0){
    subsetted_data$day_passing <- 1:nrow(subsetted_data)
    subsetted_data <- subsetted_data[,c("location","population","day_passing","infection_ratio")]
    required_data <- rbind(required_data,subsetted_data)
    required_data
  }
  else 
    required_data
}

library(reshape2)

data_wide <- dcast(required_data,day_passing ~ location, value.var="infection_ratio")
data_wide <- data_wide[, !apply(is.na(data_wide), 2, all)]


data_wide$day_passing <- NULL
View(cor(data_wide, use = "pairwise.complete.obs"))


library(ggplot2)
library(reshape2)
qplot(x = Var1, y = Var2,
      data = melt(cor(data_wide, use = "pairwise.complete.obs")),
      fill = value,
      geom = "tile")

corr_dat <- cor(data_wide, use = "pairwise.complete.obs")
corr_dat <- data.frame(corr_dat)

####
probable_winner = c("Vietnam","Canada","Austria")
probable_loser = c("Bangladesh","South Africa","Bolivia")



probable_loser <- dat_country[dat_country$location %in% probable_loser,]

library(ggplot2)
# Multiple line plot
p <- ggplot(probable_loser, aes(x = as.Date(date), y = infection_ratio)) + 
      geom_line(aes(color = location), size = 1) +
      theme_minimal()+
      labs(x = "Date", y = "Number of infection per 100 test") 


ggplotly(p)
#install.packages("plotly")
library(plotly)


View(aggregate(dat_country$infection_ratio,by = list(dat_country$location),FUN = "mean",na.rm = TRUE))
probable_winner <- dat_country[dat_country$location %in% probable_winner,]

library(ggplot2)
# Multiple line plot
ggplot(probable_winner, aes(x = as.Date(date), y = infection_ratio)) + 
  geom_line(aes(color = location), size = 1) +
  theme_minimal()+
  labs(x = "Date", y = "Number of infection per 100 test") 


######################################################################### Time series models
dat_country$date <- as.Date(dat_country$date)
bgd <- dat_country[dat_country$iso_code %in% c("BGD"),]

bgd[is.na(bgd)] <- 0

dim(bgd)


library(forecast)
library(ggplot2)


train <- bgd$infection_ratio

h <- 14
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train), h=h)
X <- cbind(ETS=ETS$mean, ARIMA=ARIMA$mean)
X



library(forecastHybrid)
fit1 <- hybridModel(train)
fit2 <- hybridModel(train, weights="insample")
fc1 <- forecast(fit1, h=h)
fc2 <- forecast(fit2, h=h)
autoplot(fc2) + ggtitle("Hybrid 1") + xlab("Year") +
  ylab(expression("Atmospheric concentration of CO"[2]))

#########################################################################
# Correlation with populaiton density and infection ratio:
cor.test(dat_country$population_density,dat_country$infection_ratio)

# South Asia:

dat_country$date <- as.Date(dat_country$date)
#dat_country$infection_ratio <- ifelse(dat_country$infection_ratio>100,NA,dat_country$infection_ratio)
#selected <- c("BGD","CHE","GBR","IND","IRN","ITA","PAK","USA")
selected <- c("BGD","CHL","QAT")

south_asia <- dat_country[dat_country$iso_code %in% selected,]

library(ggplot2)
# Multiple line plot
ggplot(south_asia, aes(x = as.Date(date), y = infection_ratio)) + 
  geom_line(aes(color = location), size = 1) +
  theme_minimal()+
  labs(x = "Date", y = "Number of infection per 100 test") 

### Median Age of the populaiton:

aggregate(south_asia$median_age,by=list(south_asia$location),FUN = mean)


# Share of the population that is 65 years and older, most recent year available

aggregate(south_asia$aged_65_older,by=list(south_asia$location),FUN = mean)

# Share of the population that is 70 years and older in 2015

aggregate(south_asia$aged_70_older,by=list(south_asia$location),FUN = mean)

























# #overall summary
# ggplot(south_asia, aes( date,location)) + geom_tile(aes(fill = infection_ratio),colour = "white") +
#   scale_fill_gradient(low = "white", high = "red") +  
#   guides(fill = guide_legend(title="Case per 100 test")) +
#   labs(title = "",x = "Date", y = "Country") +
#   theme_bw() + theme_minimal() + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# 
# 
# 
# #overall summary
# ggplot(south_asia, aes(date,location)) + geom_tile(aes(fill = new_cases_per_million),colour = "white") +
#   scale_fill_gradient(low = "white", high = "red") +  
#   guides(fill = guide_legend(title="Total new case")) +
#   labs(title = "",x = "Date", y = "Country") +
#   theme_bw() + theme_minimal() + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# 
# # Mostly infected:
# 
# 
# selected <- c("BGD","IND","IRN","ITA","USA")
# worse <- dat_country[dat_country$iso_code %in% selected,]
# 
# library(ggplot2)
# library(scales)
# 
# # Multiple line plot
# ggplot(worse, aes(x = date, y = abs(infection_ratio))) + 
#   geom_line(aes(color = iso_code), size = 1) +
#   theme_minimal()+
#   labs(x = "Date", y = "Number of infection per 100 test") +
#   scale_x_date(labels = date_format("%d-%m-%Y"))+
#   theme(axis.text.x=element_text(angle=90, hjust=1))
# 
# 
# 
# 
# # Fitting forecasting model for BGD:
# 
library(forecast)
library(tseries)

bgd <- dat_country[dat_country$iso_code %in% c("BGD"),]
fit <- auto.arima(bgd$infection_ratio)


library(ggplot2)
bgd$infection_ratio %>%
  Arima(order=c(2,1,2)) %>%
  forecast(h=30) %>%
  autoplot
# 
# 
# 
holt.bgd <- holt(bgd$infection_ratio, h = 30)
autoplot(holt.bgd)+
  labs(x = "Days passed", y = "Number of infection per 100 test")
  
  

qat <- dat_country[dat_country$location %in% c("India"),]

holt.qat <- holt(qat$infection_ratio, h = 30)
autoplot(holt.qat)

# 
# #### USA
# usa <- dat_country[dat_country$iso_code %in% c("USA"),]
# fit <- auto.arima(usa$infection_ratio)
# fit
# 
# library(ggplot2)
# usa$infection_ratio %>%
#   Arima(order=c(0,1,1)) %>%
#   forecast(h=30) %>%
#   autoplot
# 
# 
# ################################
# library(keras)
# library(tensorflow)
# library(ggplot2)
# library(stats)
# library(readr)
# library(dplyr)
# library(forecast)
# library(Metrics)
# 
# dat_country$date <- as.Date(dat_country$date)
# bgd <- dat_country[dat_country$iso_code %in% c("BGD"),]
# 
# bgd[is.na(bgd)] <- 0
# 
# 
# ###
# ggplot(bgd, aes(x=date, y = infection_ratio)) + geom_line()
# 
# 
# 
# #Remove first row as we cannot difference
# dateID <- bgd$date[2:nrow(bgd)]
# diff <- diff(bgd$infection_ratio, differencs = 1)
# 
# supervised <- as.data.frame(cbind(lag(diff,1), diff))
# supervised[is.na(supervised)] <- 0
# 
# n_ <- round(nrow(bgd) * .85, digits = 0)
# train <- supervised[1:n_, ]
# test <- supervised[(n_+1):87,]
# train_id <- dateID[1:n_]
# test_id <- dateID[(n_+1):87]
# 
# 
# ### Normalization:
# 
# scale_data <- function(train, test, feature_range = c(0,1)) {
#   x = train
#   fr_min = feature_range[1]
#   fr_max = feature_range[2]
#   std_train = (x - min(x)) / (max(x) - min(x))
#   std_test = (test - min(x)) / (max(x) - min(x))
#   
#   scaled_train = std_train * (fr_max - fr_min) + fr_min
#   scaled_test = std_test * (fr_max - fr_min) + fr_min
#   
#   return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
# }
# 
# 
# #Function to reverse scale data for prediction
# reverse_scaling <- function(scaled, scaler, feature_range = c(0,1)) {
#   min = scaler[1]
#   max = scaler[2]
#   t = length(scaled)
#   mins = feature_range[1]
#   maxs = feature_range[2]
#   inverted_dfs = numeric(t)
#   
#   for(i in 1:t) {
#     X = (scaled[i] - mins) / (maxs - mins)
#     rawValues = X * (max - min) + min
#     inverted_dfs[i] <- rawValues
#   }
#   return(inverted_dfs)
# }
# 
# 
# ###
# 
# Scaled <- scale_data(train, test, c(-1,1))
# 
# x_train <- Scaled$scaled_train[,1]
# y_train <- Scaled$scaled_train[,2]
# 
# x_test <- Scaled$scaled_test[,1]
# y_test <- Scaled$scaled_test[,2]
# 
# 
# ### LSTM
# 
# 
# dim(x_train) <- c(length(x_train), 1,1)
# X_shape2 <- dim(x_train)[2]
# X_shape3 <- dim(x_train)[3]
# batch_size <- 1
# units <- 100
# n_timesteps <- 12
# n_predictions <- n_timesteps
# 
# build_matrix <- function(tseries, overall_timesteps) {
#   t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
#     tseries[x:(x + overall_timesteps - 1)]))
# }
# reshape_X_3d <- function(X) {
#   dim(X) <- c(dim(X)[1], dim(X)[2], 1)
#   X
# }
# #install_tensorflow()
# #devtools::install_github("rstudio/tensorflow")
# library(tensorflow)
# #install_tensorflow()
# hello <- tf$constant("Hello")
# #tflow <- import("tensorflow")
# 
# 
# 
# model <- keras_model_sequential()
# model %>% 
#   layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful = TRUE) %>%
#   layer_dense(units = 1)
# 
