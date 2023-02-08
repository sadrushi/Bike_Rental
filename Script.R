library(tidyverse)
library(magrittr)
library(GGally)
library(corrplot)
library(rcompanion)

## Data
# Importing data
data <- read.csv("day data.csv")
# training data set
train <- data %>% filter(yr == 0)
# testing data set
test <- data %>% filter(yr == 1)

# converting int type to factor
colmn <- c(3:9)
train[,colmn] <- lapply(train[,colmn], factor)
str(train)

# converting int type to factor
col <- c(3:9)
test[,col] <- lapply(test[,col], factor)
str(test)

# assigning factor level names
train_levels <- train %>% select(season, mnth, holiday, weekday, workingday, 
                                 weathersit)
Season <- train_levels$season %>% recode_factor("1" = "Spring", "2" = "Summer", 
                                                "3" = "Fall", "4" = "Winter")
Month <- train_levels$mnth %>% recode_factor("1" = "Jan", "2" = "Feb", "3" = "Mar",
                                             "4" = "Apr", "5" = "May", "6" = "Jun",
                                             "7" = "Jul", "8" = "Aug", "9" = "Sep",
                                            "10" = "Oct", "11" = "Nov", "12" = "Dec")
Holiday <- train_levels$holiday %>% recode_factor("1" = "Holiday", 
                                                  "0" = "Not a holiday")
Weekday <- train_levels$weekday %>% recode_factor("0" = "Sunday", "1" = "Monday", 
                                                  "2" = "Tuesday", 
                                       "3" = "Wednesday", "4" = "Thursday", 
                                       "5" = "Friday", 
                                       "6" = "Saturday")
Workingday <- train_levels$workingday %>% recode_factor("1" = "Working day", 
                                                        "0" = "Not a working day")
Weathersit <- train_levels$weathersit %>% recode_factor("1" = "Clear, Few clouds,
                                                        Partly cloudy", 
                                          "2" = "Mist + Cloudy, Mist + Broken 
                                          clouds, Mist + Few clouds, Mist", 
                                          "3" = "Light Snow, Light Rain + 
                                          Thunderstorm + Scattered clouds, 
                                          Light Rain + Scattered clouds", 
                                          "4" = "Heavy Rain + Ice Pallets + 
                                          Thunderstorm + Mist, Snow + Fog")
train_levels <- data.frame(Season, Month, Holiday , Weekday, Workingday, Weathersit, 
                           train$cnt)

## Exploratory data analysis
# Qualitative vs. Count
# season
ggplot(train_levels, aes(x = Season, y = (train.cnt/sum(train.cnt))*100,
                         fill = Season)) + 
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) + 
  labs(title = "Figure 1: Percentage of total rental bikes according to season", x = "Season", 
       y = "Percent") + scale_y_continuous(labels = function(y) paste0(y, "%"))
# month
ggplot(train_levels, aes(x = Month, y = train.cnt, fill = Month)) + 
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) + 
  labs(title = "Figure 2: Total count of rental bikes according to month", x = "Month", 
       y = "Bike count")
# holiday
ggplot(train_levels, aes(x = Holiday, y = train.cnt, fill = Holiday)) + 
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) + 
  labs(title = "Total count of rental bikes according to day is holiday or not", 
       x = "Holiday", 
       y = "Bike count")
# weekday
ggplot(train_levels, aes(x = reorder(Weekday, -train.cnt), y = train.cnt, fill = Weekday)) + 
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) + 
  labs(title = "Total count of rental bikes according to day of the week", 
       x = "Day of the week", 
       y = "Bike count")
# working day
ggplot(train_levels, aes(x = Workingday, y = train.cnt, fill = Weekday)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5) + 
  labs(title = "Total count of rental bikes according to day is working day or not", 
       x = "Working day or not", y = "Bike count")
# holiday + working day
t <- train_levels %>% pivot_longer(c(3,5), names_to = "variable", values_to = "val")
ggplot(t, aes(x = val, y = train.cnt, fill = val)) + 
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) + 
  labs(title = "Figure 3: Total count of rental bikes according to holiday and working day", 
       x = " ", y = "Bike count") + 
  facet_wrap(~variable, scales = "free")
# weather
ggplot(train_levels, aes(x = Weathersit,
                         y = (train.cnt/sum(train.cnt))*100, fill = Weathersit)) + 
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) + 
  labs(title = "Figure 4: Percentage of total rental bikes according to 
       weather situation", 
       x = "Weather situation", y = "Percent") + theme_grey(base_size = 17) +
  scale_y_continuous(labels = function(y) paste0(y, "%")) + coord_flip()


# Quantitative vs Count
ggplot(train, aes(x = temp, y = cnt)) + geom_point(color = "forestgreen") +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Figure 5: Scatter plot of total rental bike count vs normalised temperature",
       x = "Normalised temperature in Celsius", y = "Bike count") 

ggplot(train, aes(x = atemp, y = cnt)) + geom_point(color = "purple") + 
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Figure 6: Scatter plot of total rental bike count vs normalised feeling temperature",
       x = "Normalised feeling temperature in Celsius", y = "Bike count") 
    
ggplot(train, aes(x = hum, y = cnt)) + geom_point(color = "blue") + 
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Figure 7: Scatter plot of total rental bike count vs normalised humidity",
       x = "Normalised humidity", y = "Bike count") 

ggplot(train, aes(x = windspeed, y = cnt)) + geom_point(color = "#A3A533") + 
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Figure 8: Scatter plot of total rental bike count vs normalised wind speed",
       x = "Normalised wind speed", y = "Bike count") 

train %>% select(temp, atemp, hum, windspeed, cnt) %>% 
  ggpairs(lower = list(continous = "smooth"))

train %>% select(temp, atemp, hum, windspeed, cnt) %>%
  ggpairs()


# Outlier detection
ggplot(train, aes(y = temp)) + geom_boxplot(outlier.colour = "red", color = "forestgreen",
                                            fill = "forestgreen", alpha = 0.7) + 
  labs(y = "Normalised temperature in Celsius") + theme(axis.text.x = element_blank(), 
                                                        axis.ticks.x = element_blank())
ggplot(train, aes(y = atemp)) + 
  geom_boxplot(outlier.colour = "red", color = "purple",
               fill = "purple", alpha = 0.7) + 
  labs(y = "Normalised feeling temperature in Celsius") + theme(axis.text.x = element_blank(), 
                                                                axis.ticks.x = element_blank())

ggplot(train, aes(y = hum)) + geom_boxplot(outlier.colour = "red", color = "blue",
                                           fill = "blue", alpha = 0.7) + 
  labs(y = "Normalised humidity") + theme(axis.text.x = element_blank(), 
                                          axis.ticks.x = element_blank())

ggplot(train, aes(y = windspeed)) + geom_boxplot(outlier.colour = "red", color = "#A3A533",
                                                 fill = "#A3A533", alpha = 0.7) + 
  labs(y = "Normalised wind speed") + theme(axis.text.x = element_blank(), 
                                            axis.ticks.x = element_blank())

ggplot(train, aes(y = cnt)) + geom_boxplot(outlier.colour = "red", color = "orange",
                                                 fill = "orange", alpha = 0.7) + 
  labs(y = "Count of total rental bikes") + theme(axis.text.x = element_blank(), 
                                                  axis.ticks.x = element_blank())


# correlation analysis
# quantitative
c <- train %>% select(temp, atemp, hum,windspeed, cnt) %>% cor()
corrplot(c, method = 'number',  bg = "#A3A533", type = "upper")
# qualitative
cramerV(train_levels$Weathersit, train_levels$Season)
cramerV(train_levels$Weathersit, train_levels$Month)
cramerV(train_levels$Weathersit, train_levels$Holiday)
cramerV(train_levels$Weathersit, train_levels$Weekday)
cramerV(train_levels$Weathersit, train_levels$Workingday)
cramerV(train_levels$Weathersit, train_levels$Weathersit)

cramerV(train_levels$Workingday, train_levels$Season)
cramerV(train_levels$Workingday, train_levels$Month)
cramerV(train_levels$Workingday, train_levels$Holiday)
cramerV(train_levels$Workingday, train_levels$Weekday)
cramerV(train_levels$Workingday, train_levels$Workingday)
cramerV(train_levels$Workingday, train_levels$Weathersit)

# Normality of response
ggplot(train, aes(x = cnt)) + geom_histogram(aes(y = ..density..), color = "black",
                                             fill = "white") +
  geom_density(alpha = 0.5, fill = "purple")

qqnorm(train$cnt, pch = 1)
qqline(train$cnt, col = "red", lwd = 2)

shapiro.test(train$cnt)

# building the model
model1 <- lm(cnt~factor(season)+factor(holiday)+factor(workingday)+
               factor(weathersit)+temp+hum+windspeed, data = train)
summary(model1)

model2 <- lm(cnt~factor(season)+factor(holiday)+factor(weathersit)+
               temp+hum+windspeed, data = train)
summary(model2)

anova(model2)

library(olsrr)
k <- ols_step_backward_p(model1, prem = 0.15, progress = TRUE)
k$model

# check for multicollinearity
library(car)
vif(model2)

# identifying influential points
library(lindia)
p <- gg_cooksd(model2) ; p
library(plotly)
ggplotly(p)


# residuals
res <- residuals(model2)

# fitted
fit <- predict(model2)

# residual analysis
res.analysis <- function(model){
  res <- residuals(model)
  hist(res, main = "Histogram of residuals")
  qqnorm(res, pch = 1, main = "QQ plot of residuals")
  qqline(res, col = "red", lwd = 2)
  print(shapiro.test(res))
}
res.analysis(model2)

library(broom)
model3_fitresid <- augment(model2)
model3_fitresid

# residual vs. fitted
ggplot(model3_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1)

# histogram of residuals
ggplot(model3_fitresid, aes(x = .std.resid)) + geom_histogram(color = "white") +
  ggtitle("Histogram of residuals")

# QQ plot
ggplot(model3_fitresid, aes(sample = .std.resid)) + stat_qq() + stat_qq_line()
  ggtitle("QQ plot of residuals")

# shapiro wilk
shapiro.test(model3_fitresid$.std.resid)

# residual autocorrelation
acf(model1$residuals, type = "correlation")
pacf(model1$residuals)

library(lmtest)
bgtest(model1, data = train, order = 2)

library(car)
durbinWatsonTest(model1)




# ------------------------------------------------------------------------------
# removing influential points
influetial <-  read.csv("influential.csv")

# building the model
model21 <- lm(cnt~factor(season)+factor(holiday)+factor(workingday)+
               factor(weathersit)+temp+hum+windspeed, data = influetial)
summary(model21)

model22 <- lm(cnt~factor(season)+factor(holiday)+
               factor(weathersit)+temp+hum+windspeed, data = influetial)
summary(model22)
anova(model22)



library(olsrr)
f <- ols_step_backward_aic(model21, prem = 0.15, progress = TRUE)
f$model

# residual autocorrelation
res2 <- residuals(model22)
acf(res2, type = "correlation")

library(lmtest)
bgtest(model22, data = train, order = 4)

library(car)
durbinWatsonTest(model22)

library(broom)
model23_fitresid <- augment(model22)
model23_fitresid

# residual analysis
res <- residuals(model22)
fit <- predict(model22)
df <- data.frame(res,fit)

# residual vs. fitted
ggplot(df, aes(x = fit, y = res)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1) + 
  labs(title = "Residual plot against the fitted values", x = "Fitted values", 
       y = "Residuals")
# boxplot of residuals
library(car)
Boxplot(res)

# nonindependence of residuals
ggplot(df, aes(x = influetial$instant, y = res)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1)
  
# histogram of residuals
ggplot(df, aes(x = res)) + geom_histogram(aes(y = ..density..), color = "black",
                                             fill = "white") +
  geom_density(alpha = 0.5, fill = "purple") + ggtitle("Histogram of residuals") +
  xlab("Residuals")

# QQ plot
ggplot(df, aes(sample = res)) + stat_qq() + stat_qq_line(color = "red") +
labs(title = "Normal probability plot of residuals", x = "Expected", y = "Residuals")

# shapiro wilk
shapiro.test(df$res)

## Autocorrelation
acf(df$res, type = "correlation") 
pacf(df$res)
library(lmtest)
bgtest(model22, data = influetial)

# testing data set
test <- data %>% filter(yr == 1)


# in sample accuracy and out of sample accuracy
# test MSE
test %>% add_predictions(model22) %>% 
  summarise(MSE = mean((cnt - pred))^2)

# training MSE
influetial %>% add_predictions(model22) %>% 
  summarise(MSE = mean((cnt - pred))^2)

# ------------------------------------------------------------------------------

# making predictions
# testing data set
test <- data %>% filter(yr == 1)
# converting int type to factor
col <- c(3:9)
test[,col] <- lapply(test[,col], factor)
str(test)

test_predictions <- predict(model22, test)
df2 <- data.frame(test$cnt, test_predictions)

MSPR <- sum((df2$test.cnt - df2$test_predictions)^2)/366

library(modelr)
test_pred <- test %>% add_predictions(model22)

test_pred <- test_pred %>% mutate(error = (cnt - pred)^2)

MSPR <- sum(test_pred$error)/366

(sum((test_pred$cnt - test_pred$pred)^2))/366

model <- lm(cnt~factor(season)+factor(holiday)+
                factor(weathersit)+temp+hum+windspeed, data = test)
anova(model)
# in sample accuracy and out of sample accuracy
# test MSE
test %>% add_predictions(model22) %>% 
  summarise(MSE = mean((cnt - pred))^2)

# training MSE
train %>% add_predictions(model22) %>% 
  summarise(MSPR = (sum((cnt - pred))^2)/366)

# ------------------------------------------------------------------------------
out <- Boxplot(train$hum, id.method = "y")
out

out <- Boxplot(train$windspeed, id.method = "y")
out


d <- read.csv("outremoved.csv")

model1 <- lm(cnt~factor(season)+factor(holiday)+factor(workingday)+
               factor(weathersit)+temp+hum+windspeed, data = d)
summary(model1)

model2 <- lm(cnt~factor(season)+factor(holiday)+factor(weathersit)+
               temp+hum+windspeed, data = d)
summary(model2)

anova(model2)

r <- residuals(model2)
# residual autocorrelation
acf(r, type = "correlation")
pacf(r)

library(lmtest)
bgtest(model2, data = train, order = 2)

library(car)
durbinWatsonTest(model2)

library(orcutt)
cochrane.orcutt(model2)

# **********************************************
Box.test(residuals(model22), type = "Ljung-Box")
bgtest(model22)










