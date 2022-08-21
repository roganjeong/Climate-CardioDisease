# XGBoost
# 5-fold Cross Validation

library(dplyr)
library(readxl)
library(xgboost)
library(corrplot)
library(caret)


mse <- function(y,y_pred){
  mean((y-y_pred)**2)
}
# rmse 계산 공식
rmse <- function(y,y_pred){
  sqrt(mse(y,y_pred))
}

# 데이터 불러오기
my_data <- read_excel("0804 daily.xlsx")

#변수명 확인
colnames(my_data)

#날짜 변수 변경
sep_col <- c("yyyymmdd")
my_data$yyyymmdd <- as.Date(as.character(my_data$yyyymmdd)
                            ,format = "%Y%m%d")

# train, test 데이터 시도별, 성별로 나누기
# 나머지 시도, 성별에 대해서도 같은 방식으로 진행
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

#yyyymmdd 데이터만 remove_column에 넣기
remove_column <- c("yyyymmdd")

#train_data 에서 remove_column 제거
train_data <- train_data %>%
  select(-remove_column)

#test_data 에서 remove_column 제거
test_data <- test_data %>%
  select(-remove_column)

train_info <- train_data %>% 
  select(add , sex)
train_data <- train_data %>%
  select(frequency:dd)


test_info <- test_data %>% 
  select(add , sex)
test_data <- test_data %>%
  select(frequency:dd)


target_column_1 <- c("frequency")
target_column_2 <- c("frequency_bin")

#시도별로 범주화 진행

#0-2,3-4,5-12
train_data$frequency_bin <- cut(
  train_data$frequency,
  breaks = c(0,3,5,13),
  labels  = c("0-2","3-4","5-12"),
  include.lowest = TRUE,
  right=FALSE,
)


test_data$frequency_bin <- cut(
  test_data$frequency,
  breaks = c(0,3,5,13),
  labels  = c("0-2","3-4","5-12"),
  include.lowest = TRUE,
  right=FALSE,
)

# 가중평균 내기
cal_weighted_mean <- function(x){
  freq <- as.integer(names(table(x)))
  count <- table(x) %>% as.vector()
  w_mean <-  sum(freq*count) / sum(count)
  return(w_mean)
}

train_data <- train_data %>% 
  group_by(frequency_bin) %>%
  mutate(freq_weight_mean=cal_weighted_mean(frequency)) %>% ungroup()


train_data %>% 
  select(frequency_bin , frequency , freq_weight_mean) %>% head()

test_data <- test_data %>% 
  group_by(frequency_bin) %>%
  mutate(freq_weight_mean=cal_weighted_mean(frequency)) %>% ungroup()

x_columns <- train_data %>% 
  select(avg_ps:dd) %>% 
  colnames()
length(x_columns)

x_columns

# xgboost 하기 위해 행렬화
xgb_train = xgb.DMatrix(
  data = data.matrix(
    train_data %>% select(x_columns)),
  label = train_data$freq_weight_mean)
xgb_test = xgb.DMatrix(
  data = data.matrix(
    test_data %>% select(x_columns)),
  label = test_data$freq_weight_mean)
watchlist = list(train=xgb_train,
                 test=xgb_test)

#parameter 정하기
param <- list(
  eta = 0.1,
  max_depth = 8, 
  min_child_weight=30,
  subsample = 0.5,
  colsample_bytree = 0.5,
  booster="gbtree",
  objective = "reg:squarederror",
  max_bin=128,
  lambda= 0.3,
  alpha=0.1
)
# 파라미터 1개를 가지고 cross-validation(cv)
xgbcv <- xgb.cv(data = xgb_train, nfold = 5, metrics = list("rmse"),
             nrounds=100,nthread = 6,verbose=0,
             params=param, early_stopping_rounds = 20
             )
xgbcv$evaluation_log$test_rmse_mean

# 학습 시 파라미터에 대한 best n round 찾기 
best_n_rounds <-  which.min(xgbcv$evaluation_log$test_rmse_mean)
test_best_rmse <- xgbcv$evaluation_log$test_rmse_mean[best_n_rounds]

# 해당 파라미터를 가지고 다시 xgboost 학습 
xgb_model <- xgboost(
  param=param,
  nrounds=best_n_rounds,
  data=xgb_train,
  nthread = 6,
  verbose=0
)

# 랜덤 서치 방식 
# 파라미터를 랜덤하게 설정해서 학습시키기 
# 100개의 파라미터를 생성하면서 최고의 파라미터 찾는 것으로 함

best_score = 100;count = 1;best_n_rounds = 100
for (i in 1:100){
  depth <-  sample(6:9,size=1)
  param$eta <- runif(1,0.01,0.1)
  param$max_depth = depth
  param$colsample_bytree = runif(1,0.5,1.0)
  param$subsample = runif(1,0.5,1.0)
  param$lambda = runif(1,0.0,1.0)
  param$alpha = runif(1,0.0,1.0)
  xgbcv <- xgb.cv(data = xgb_train, nfold = 5, metrics = list("rmse"),
                  nrounds=100,nthread = 6,verbose=0,
                  params=param, early_stopping_rounds = 20
  )
  cv_best_n_rounds <-  which.min(xgbcv$evaluation_log$test_rmse_mean)
  test_best_rmse <- xgbcv$evaluation_log$test_rmse_mean[cv_best_n_rounds]
  if(count == 1){
    best_params = param
    best_score = test_best_rmse
    count = count + 1
  }
  else if(test_best_rmse < best_score){
    best_params = param
    best_n_rounds = cv_best_n_rounds
    best_score = test_best_rmse
    save(best_params, file="best_params.RData")
    save(best_score, file="best_score.RData")
    save(best_n_rounds, file="best_n_rounds.RData")
    print(sprintf("CV TEST BEST RMSE : %f",test_best_rmse))
  }
}

# 저장해 놓은 파라미터 가져오기 
load(file="best_params.RData")
load(file="best_n_rounds.RData")
load(file="best_score.RData")
print(best_params)
print(best_score)

# CV 진행 후 베스트 파라미터를 가지고 다시 xgboost 학습
xgb_model <- xgboost(
  param=best_params,
  nrounds=best_n_rounds,
  data=xgb_train,
  nthread = 6,
  verbose=0
)
te_y_pred <- predict(xgb_model,
                     newdata=xgb_test)
te_rmse <- rmse(y=test_data$frequency,
                y_pred=te_y_pred)


test_data %>% 
  select(frequency , )

test_data %>% head()
print(sprintf("TEST RMSE : %f",te_rmse))
print(test_data)

xgb.save(xgb_model,"xgboost_best_cv.model")

best_params

rm(xgb_model)
xgb_model <- xgb.load("xgboost.model")
te_y_pred <- predict(xgb_model,
                     newdata=xgb_test)


te_rmse <- rmse(y=test_data$frequency,
                y_pred=te_y_pred)
print(sprintf("BEST RMSE : %f",te_rmse))

