
install.packages("lmvar")
library(dplyr)
library(readxl)
library(xgboost)
library(corrplot)
library(caret)

library(lmvar)
mse <- function(y,y_pred){
  mean((y-y_pred)**2)
}


rmse <- function(y,y_pred){
  sqrt(mse(y,y_pred))
}

my_data <- read_excel("0804 daily.xlsx")
my_data %>% head()

colnames(my_data)

sep_col <- c("yyyymmdd")
my_data$yyyymmdd <- as.Date(as.character(my_data$yyyymmdd)
                            ,format = "%Y%m%d")

# 나머지 시도, 성별에 대해서도 같은 방식으로 데이터 분할하여 진행
train_data <- my_data %>% 
  filter(
    yyyymmdd < "2015-01-01" &
      add == "서울" &
      sex == 1
  )
test_data <- my_data %>% 
  filter(
    yyyymmdd >= "2015-01-01" &
      add == "서울" &
      sex == 1,
  )


remove_column <- c("yyyymmdd")

colnames(train_data)

train_data <- train_data %>%
  select(-remove_column)

test_data <- test_data %>%
  select(-remove_column)

train_data <- train_data %>%
  select(frequency:dd)

test_data <- test_data %>%
  select(frequency:dd)


y_column <- c("frequency")

x_columns <-c(
  "avg_ps"  ,               "max_ps" ,                "min_ps"   ,             
  "avg_td"      ,           "max_td"   ,              "max_td_hrmt"  ,          "min_td" ,               
  "min_td_hrmt"   ,         "avg_ta_x"   ,            "max_ta_x"   ,            "min_ta_x" ,             
  "so2"     ,               "co"    ,                 "o3"    ,                 "no2" ,                  
  "pm10"   ,                "avg_ta_y"  ,             "max_ta_y"  ,             "min_ta_y"  ,            
  "min_mrng_ta"   ,         "max_dytm_ta"  ,          "min_nght_ta",            "holiday"    ,           
  "over_SO2_WHO_crit",      "over_PM10_WHO_crit" ,    "over_O3_WHO_crit" ,      "한파" ,                 
  "폭염"      ,             "season_fall" ,           "season_spring"   ,       "season_summer"  ,       
  "season_winter"  ,        "weekday_Fri"  ,          "weekday_Mon" ,           "weekday_Sat"  ,         
  "weekday_Sun" ,           "weekday_Thu",            "weekday_Tue" ,           "weekday_Wed"  ,         
  "month_1",                "month_2" ,               "month_3"  ,              "month_4" ,              
  "month_5" ,               "month_6" ,               "month_7" ,               "month_8",               
  "month_9"   ,             "month_10"  ,             "month_11"  ,             "month_12" ,             
  "min_mrng_ta_lag_1" ,     "min_mrng_ta_lag_2",      "min_mrng_ta_lag_3" ,     "min_mrng_ta_lag_4",     
  "min_mrng_ta_lag_5"  ,    "min_mrng_ta_lag_6"  ,    "min_mrng_ta_lag_7" ,     "min_ps_lag_1",         
  "min_ps_lag_2",           "min_ps_lag_3"  ,         "min_ps_lag_4" ,          "min_ps_lag_5" ,         
  "min_ps_lag_6" ,          "min_ps_lag_7",           "min_td_hrmt_lags_lag_1" ,"min_td_hrmt_lags_lag_2",
  "min_td_hrmt_lags_lag_3" ,"min_td_hrmt_lags_lag_4", "min_td_hrmt_lags_lag_5", "min_td_hrmt_lags_lag_6",
  "min_td_hrmt_lags_lag_7" ,"value"  ,                "mmdd" ,                 "mm",                   
  "dd"  )


# Normalization--선형 회귀에서는 normalization 데이터가 필요함.

# train_data 에서 x_columns 에 해당하는 것만 normalization
normParam <- preProcess(
  train_data %>% select(x_columns)
)

norm.train_data <- predict(normParam, train_data %>% select(x_columns)) %>%
  as.data.frame()

norm.test_data <- predict(normParam, test_data %>% select(x_columns)) %>%
  as.data.frame()

norm.train_data <-  mutate(
  norm.train_data,frequency = train_data$frequency
)

norm.test_data <- mutate(
  norm.test_data,frequency = test_data$frequency
)

#선형회귀 공식

f <- as.formula(
  paste(y_column, 
        paste(x_columns, collapse = " + "), 
        sep = " ~ "))
print(f)
model <- lm(f, data = norm.train_data,x=TRUE,y=TRUE)

rmse_f = function(object, y, X){
  mu = predict(object, as.data.frame(X))
  residuals = y - mu
  return(sqrt(mean(residuals**2)))
}
cv <- cv.lm(model, 
            fun = rmse_f , 
            k=5, 
            seed = 5483,
            log=FALSE,
            max_cores = 4)
## 출력하게 되면 각 메트릭별 성능이 나옴 
print(cv)

# rmse 가 동일하게 나온 것을 확인
cv$MSE_sqrt
cv$fun
# test 데이터 예측값
te_y_pred <- predict(model ,newdata=norm.test_data)

# rmse
rmse <- function(y,y_pred){
  sqrt(mse(y,y_pred))
}

te_rmse <- rmse(y=test_data$frequency,
                y_pred=te_y_pred)
print(sprintf("BEST RMSE : %f",te_rmse))
