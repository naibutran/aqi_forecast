install.packages('caTools')
install.packages('Metrics')
library(caTools)
library(Metrics)

set.seed(123)
split = sample.split(df2wanova$pm25, SplitRatio = 2/3)
training_set = subset(df2wanova, split == TRUE)
test_set = subset(df2wanova, split == FALSE)


regressor = lm(formula = pm25 ~ YEAR + MO + DY + Temperature+ Relative_Humidity+
                 Specific_Humidity+Precipitation+Pressure+Wind_Speed+Wind_Direction,
               data = training_set)

y_pred = predict(regressor, newdata = test_set)

rmse(test_set$pm25, y_pred)

summary(regressor)




regressor_tn = lm(formula = pm25 ~ MO * (Relative_Humidity + Specific_Humidity + Precipitation
                                        + Pressure + Wind_Speed),
               data = training_set)

y_pred_tn = predict(regressor_tn, newdata = test_set)

rmse(test_set$pm25, y_pred_tn)

summary(regressor_tn)



