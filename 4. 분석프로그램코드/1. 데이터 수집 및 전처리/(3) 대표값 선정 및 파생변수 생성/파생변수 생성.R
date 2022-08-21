#######################################
# 요일, 월, 계절, 일 등 날짜 변수 생성
#   및 EDA
#######################################
# 패키지 부착
library(lubridate)
library(tidyverse)
library(itsmr)
library(aTSA)
library(forecast)
library(fastDummies)

# 백병원 데이터 로딩
hp = read.csv("C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/back_hospital.csv")
colnames(hp)

# 요일, 월, 연도, 계절 변수 추가
hp$numeric_freq = as.numeric(hp$back_hospital.frequency)
hp$weekday = as.factor(wday(as.Date(as.character(hp$back_hospital.yyyymmdd),'%Y%m%d')))
hp$date = as.Date(as.character(hp$back_hospital.yyyymmdd),'%Y%m%d')
hp$month = as.factor(month(hp$date))
hp$year = as.factor(year(hp$date))
hp$season = as.factor(ifelse(hp$month %in% c(12,1,2),'winter',
                             ifelse(hp$month %in% c(3,4,5), 'spring',
                                    ifelse(hp$month %in% c(6,7,8,9), 'summer', 'fall'))))

# 서울 남자 
hp %>% filter(back_hospital.area == '서울', back_hospital.sex == 1) %>%
  ggplot(aes(x= back_hospital.frequency)) +
  geom_histogram(bins = 30)
summary(hp)
############################
# 요일에 따른 frequency
#############################
# 전체 시도
hp %>% ggplot(aes(x = weekday, y = back_hospital.frequency))+
  geom_violin()
hp %>% ggplot(aes(x = weekday, y = back_hospital.frequency))+ # 월요일 vs 나머지 요일
  geom_boxplot()
# 시도 별
hp %>% ggplot(aes(color = weekday, y = back_hospital.frequency, x = back_hospital.area))+
  geom_boxplot()

##########
#월요일
###########
# 월요일 dummy 변수 만들어줌
hp = hp %>% mutate(monday = as.factor(ifelse(weekday == 2, 1,0)))
hp %>% ggplot(aes(color = monday, y = back_hospital.frequency, x = back_hospital.area))+ # 월요일 vs 나머지 요일
  geom_boxplot()

## CHI_square test: 시도 별 MONDAY~ freq
chisq_monday = vector(mode= 'list', length(unique(hp$back_hospital.area)))
fisher_monday = vector(mode= 'list', length(unique(hp$back_hospital.area)))
for(i in 1:length(unique(hp$back_hospital.area))){
  hp_area = hp %>% filter(back_hospital.area == unique(hp$back_hospital.area)[i])
  chisq_monday[[i]]  = chisq.test(x = hp_area$monday, y=  hp_area$back_hospital.frequency)
  fisher_monday[[i]] = fisher.test(x = hp_area$monday, y=  hp_area$back_hospital.frequency, simulate.p.value =  TRUE)
}
names(chisq_monday) = unique(hp$back_hospital.area)
names(fisher_monday) = unique(hp$back_hospital.area)
chisq_monday    # 월요일, 다른요일 차이 있음
fisher_monday   # 월요일, 다른요일 차이 있음

##############################
# 월에 따른 frequency
################################
hp %>% ggplot(aes(col = month, y = back_hospital.frequency, x = back_hospital.area))+
  geom_boxplot()

chi_month = chisq.test(x = hp$month, y = hp$back_hospital.frequency)
chi_month$observed
chi_month

## CHI_square test: 시도 별 MONTH ~ freq
chisq_month = vector(mode= 'list', length(unique(hp$back_hospital.area)))
fisher_month = vector(mode= 'list', length(unique(hp$back_hospital.area)))
for(i in 1:length(unique(hp$back_hospital.area))){
  hp_area = hp %>% filter(back_hospital.area == unique(hp$back_hospital.area)[i])
  chisq_month[[i]]  = chisq.test(x = hp_area$month, y=  hp_area$back_hospital.frequency)
  fisher_month[[i]] = fisher.test(x = hp_area$month, y=  hp_area$back_hospital.frequency, simulate.p.value =  TRUE)
}
names(chisq_month) = unique(hp$back_hospital.area)
names(fisher_month) = unique(hp$back_hospital.area)

chisq_month # 월별 차이 없음 (단독으로는)
fisher_month # 월별 차이 없음 (단독으로는)



###############################
# 계절
###############################
# 시도별 계절
hp %>% 
  ggplot(aes(x = back_hospital.area , y = back_hospital.frequency, color = season)) +
  geom_boxplot()

## CHI_square test: 시도 별 SEASON ~ freq
chisq_season = vector(mode= 'list', length(unique(hp$back_hospital.area)))
fisher_season = vector(mode= 'list', length(unique(hp$back_hospital.area)))
for(i in 1:length(unique(hp$back_hospital.area))){
  hp_area = hp %>% filter(back_hospital.area == unique(hp$back_hospital.area)[i])
  chisq_season[[i]]  = chisq.test(x = hp_area$season, y=  hp_area$back_hospital.frequency)
  fisher_season[[i]] = fisher.test(x = hp_area$season, y=  hp_area$back_hospital.frequency, simulate.p.value =  TRUE)
}
names(chisq_season) = unique(hp$back_hospital.area)
names(fisher_season) = unique(hp$back_hospital.area)
chisq_season # 계절별 차이 없음 (단독으로는)
fisher_season # 계절별 차이 없음 (단독으로는)


###############################################################
# frequency 계절성, 추기성, 자기상관성 explanatory analysis
#################################################################
source('C:/Users/user/OneDrive/바탕 화면/JHLee/학교/석사/시계열/TS-library.R')

par(mfrow = c(1,1))
hp %>%
  ggplot(aes(x = date, y = back_hospital.frequency, col = back_hospital.area)) +
  geom_path() +
  facet_wrap(vars(back_hospital.area), nrow = 5)

diff()
### ACF for male
par(mfrow = c(5,4))
for(i in 1:length(unique(hp$back_hospital.area))){
  city = hp %>%
    filter(back_hospital.area == unique(hp$back_hospital.area)[i], back_hospital.sex == 2) %>%
    arrange(date)
  acf2(city$back_hospital.frequency, lag= 50) ;title(str_glue('acf_male',unique(hp$back_hospital.area)[i]))
}
### PACF for male
par(mfrow = c(2,2))
for(i in 1:length(unique(hp$back_hospital.area))){
  city = hp %>%
    filter(back_hospital.area == unique(hp$back_hospital.area)[i], back_hospital.sex == 2) %>%
    arrange(date)
  pacf(city$back_hospital.frequency, lag= 30, main = str_glue('pacf_male',unique(hp$back_hospital.area)[i]))
}
s1 = hp %>% filter(back_hospital.area == '서울', back_hospital.sex == 1)
par(mfrow = c(1,1))
acf(s1$numeric_freq)
pacf(s1$numeric_freq)

######################################
# ACF for categorical variable
###########################################
data <- hp %>%
  filter(back_hospital.area == '서울', back_hospital.sex == 1) %>%
  arrange(date) %>%
  mutate(factor_Freq = as.factor(back_hospital.frequency)) %>%
  select(factor_Freq) 
data = data$factor_Freq
Tlen <- length(data)
states <- unique(data)
nostates <- length(states)
datanum <- match(data, states)

#Binarization
bincodes <- diag(1,nostates)
databin <- bincodes[datanum,]

#relative frequencies
hatpi <- colMeans(databin)

maxlag <- 100
hatbivprob <- array(0,c(nostates,nostates,maxlag))

for(k in c(1:maxlag)){#for each lag
  for(i in c(1:nostates)){#for each lagged vector representing a category
    for(j in c(1:nostates)){#for each vector representing a category
      hatbivprob[i,j,k] <- mean(databin[(k+1):Tlen,i]*databin[1:(Tlen-k),j])
    }
  }
}
#compare
indprob <- hatpi %*% t(hatpi)


#Cramer's v
cramer <- rep(0,maxlag)
for(k in c(1:maxlag)){
  cramer[k] <- sqrt(sum((hatbivprob[,,k]-indprob)^2/indprob)/(nostates-1))
}
par(mfrow = c(1,1))
plot(cramer, type="h", xlab = "k", ylab = "Cramer's   v(k)", lwd=4, ylim=c(-1,1))


###############################################
# 공휴일
###############################################
rest = read.csv('C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/12-16년_공휴일.csv', fileEncoding="euc-kr")
rest$날짜 = as.Date(rest$날짜)
rest$요일 = as.factor(wday(rest$날짜))
rest = rest %>% filter(!(휴일명  %in% c('토요일', '일요일')))
hp = hp %>%
  left_join(rest, by = c('date'= '날짜')) %>%
  mutate(공휴일 = as.factor(ifelse(is.na(휴일명), 0, 1))) %>%
  select(!c(요일, 휴일명))
par(mfrow= c(3,1))
hp %>% ggplot(aes(color = 공휴일, y = back_hospital.frequency, x = back_hospital.area))+
  geom_boxplot()

hp %>% ggplot(aes(color = sunday, y = back_hospital.frequency, x = back_hospital.area))+
  geom_boxplot()

hp %>% ggplot(aes(color = saturday, y = back_hospital.frequency, x = back_hospital.area))+
  geom_boxplot()




################################################33
# AQI(PM10, SO2, CO, NO2, O3) WHO 기준치 초과일
#############################################3
aqi = read.csv("C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/pm10.csv")
class(aqi$tm)
# 세종시 = mean(대전 유성구, 충북 청주시)
sejong = aqi %>% 
  filter(area %in% c('대전 유성구', '충북 청주시')) %>%
  select(area, tm, so2, co, o3, no2, pm10) %>%
  group_by(tm) %>%
  summarize_all(~mean(na.rm = TRUE, .))
sejong_fin = sejong %>% filter(tm >= 2012000000) %>%
  mutate(area = '세종',
         area_17 = '세종',
         stn = '세종',
         stn_code = 212121,
         yyyymmdd = substr(tm,1,8),
         add = '세종시 어딘가')
aqi = rbind(aqi,sejong_fin)

## 24시간 단위 (SO2 PM10)
air_24avg = aqi %>%
  group_by(area_17, yyyymmdd) %>% 
  summarize(
    daily_avg_SO2 = mean(so2, na.rm = T), # 각 요소 별 일별 평균
    daily_avg_PM10 = mean(pm10, na.rm =T))

## 8시간 단위 (CO O3)#############################################################
air_8avg = aqi %>%
  filter(tm %% 100 <= 19 & tm %% 100 >= 12) %>%
  group_by(area_17, yyyymmdd) %>% 
  summarize(
    daily_avg_CO = mean(co, na.rm = T), # 각 요소 별 일별 12~20시 평균
    daily_avg_O3 = mean(o3, na.rm =T))

## 1시간 단위 (NO2)############################################################
air_1avg = aqi %>%
  filter(tm %% 100 == 15) %>%
  group_by(area_17, yyyymmdd) %>% 
  summarize(
    daily_avg_NO2 = mean(no2, na.rm = T)) #  일별 15시 평균

#############################################
# 측정단위 & who 기준 단위 맞춰줌 microgram per m^3 -> ppm
###########################################################
SO2_conv_micro_ppm = function(x){
  x = x/2.62/1000
  return(x)
}
NO2_conv_micro_ppm = function(x){
  x = x/1.88/1000
  return(x)
}
O3_conv_micro_ppm = function(x){
  x = x/1.96/1000
  return(x)
}

#############################################################
## SO2, PM10, NO2 (WHO 24시간 기준)
############################################################
# SO2, PM10 측정소별, 연도별로 기준치 넘는 날 정리 -> rowwise 쭉 이어짐
########################################################################################################

################################
SO2_PM10 =air_24avg %>%
  group_by(area_17,yyyymmdd) %>%
  summarize(over_SO2_WHO_crit = ifelse(daily_avg_SO2 >= SO2_conv_micro_ppm(20), 1,0),
            over_PM10_WHO_crit = ifelse(daily_avg_PM10 >= 50,1,0))
##################################
## CO, O3  (WHO 8시간 단위)
##################################

CO_O3 = air_8avg %>%
  group_by(area_17, yyyymmdd) %>%
  summarize(over_O3_WHO_crit = ifelse(daily_avg_O3 >= O3_conv_micro_ppm(100),1,0),
            over_CO_KOR_crit = ifelse(daily_avg_CO >= 9,1,0))
sum(CO_O3$over_CO_KOR_crit == 1)
##################################
##  NO2 15시 1시간 단위
##################################

NO2 = air_1avg %>%
  group_by(area_17, yyyymmdd) %>%
  summarize(over_NO2_WHO_crit = ifelse(daily_avg_NO2 >= NO2_conv_micro_ppm(200),1,0))
which(NO2$over_NO2_WHO_crit ==1)

###################################
# 취합
###################################

AQI_over_crit = cbind(SO2_PM10, CO_O3[,-c(1,2)], NO2[,-c(1,2)])

# NA 전날 값으로 채워줌
for(j in 3:7){
  for(i in which(is.na(AQI_over_crit[,j]))){
    AQI_over_crit[i,j] = AQI_over_crit[i-1,j]
  }
}

AQI_over_crit_train = AQI_over_crit %>% filter(yyyymmdd >=20120000, yyyymmdd < 20160000)
AQI_over_crit_test = AQI_over_crit %>% filter(yyyymmdd >= 20160000)
AQI_over_crit_full = AQI_over_crit %>% filter(yyyymmdd >= 20120000)


#########################################################3
# test set 2016년 파생, 추가 변수 붙여주기
#########################################################
test = read.csv("C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/test_3.csv")
test$date = as.Date(as.character(test$yyyymmdd),'%Y%m%d')
test$weekday = as.factor(wday(test$date, label = T))
test$month = as.factor(month(test$date))
test$year = as.factor(year(test$date))
test$season = as.factor(ifelse(test$month %in% c(12,1,2),'winter',
                               ifelse(test$month %in% c(3,4,5), 'spring',
                                      ifelse(test$month %in% c(6,7,8,9), 'summer', 'fall'))))
## 더미변수 만들어줌!
#계절
test = dummy_cols(test, 'season')
# 요일
test = dummy_cols(test, 'weekday')
# 월
test = dummy_cols(test, 'month')
# factor 변수 제거
test = test %>% select(!c(month, weekday, season, X))


###########
# 공휴일
###########
test_rest = rest %>% filter(날짜 >= as.Date('2016-01-01'))
test = test %>%
  left_join(test_rest, by = c('date' = '날짜'))
test = test %>%
  mutate(holiday = as.factor(ifelse(is.na(휴일명), 0, 1))) %>%
  select(!c(휴일명, 요일))
#############
# AQI 붙이기
#################
AQI_over_crit_test = read.csv(file = 'C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/파생변수, 추가변수/AQI_WHO기준초과_더미_test0723.csv', fileEncoding =  'cp949')
AQI_test = AQI_over_crit_test %>%
  mutate(date = as.Date(as.character(yyyymmdd),'%Y%m%d'))
AQI_test$over_SO2_WHO_crit = as.factor(AQI_test$over_SO2_WHO_crit)
AQI_test$over_PM10_WHO_crit = as.factor(AQI_test$over_PM10_WHO_crit)
AQI_test$over_CO_KOR_crit = as.factor(AQI_test$over_CO_KOR_crit)
AQI_test$over_NO2_WHO_crit = as.factor(AQI_test$over_NO2_WHO_crit)
AQI_test$over_O3_WHO_crit = as.factor(AQI_test$over_O3_WHO_crit)
test = test %>%
  left_join(AQI_test, by = c('date', 'add' = 'area_17'))
test = test %>% select(!c(yyyymmdd.y,  date, X)) %>%
  rename(yyyymmdd = yyyymmdd.x)
##################
# 폭염, 한파
##################
test = test %>% mutate(한파 = as.factor(ifelse(min_mrng_ta <= -12,1,0)),
                       폭염 = as.factor(ifelse(max_dytm_ta >= 33,1,0)))


##############################################################33
# train(2012~2015년) 에 붙이기
##################################################################
train = read.csv("C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/일단있는애들먼저6_인코딩변경.csv",fileEncoding="euc-kr")

train$date = as.Date(as.character(train$yyyymmdd),'%Y%m%d')
train$weekday = as.factor(wday(train$date, label= TRUE))
train$month = as.factor(month(train$date))
train$year = as.factor(year(train$date))
train$season = as.factor(ifelse(train$month %in% c(12,1,2),'winter',
                                ifelse(train$month %in% c(3,4,5), 'spring',
                                       ifelse(train$month %in% c(6,7,8,9), 'summer', 'fall'))))
## 더미변수 만들어줌!
#계절
train = dummy_cols(train, 'season')
# 요일
train = dummy_cols(train, 'weekday')
# 월
train = dummy_cols(train, 'month')
# factor 변수 제거
train = train %>% select(!c(month, weekday, season, X))
########
# 공휴일
###########
train_rest = rest %>% filter(날짜 < as.Date('2016-01-01'))
train = train %>%
  left_join(train_rest, by = c('date' = '날짜')) %>%
  mutate(holiday = as.factor(ifelse(is.na(휴일명), 0, 1))) %>%
  select(!c(휴일명, 요일))
#################
# AQI_train 붙여
#####################
AQI_train = AQI_over_crit_train %>%
  mutate(date = as.Date(as.character(yyyymmdd),'%Y%m%d'))
AQI_train$over_SO2_WHO_crit = as.factor(AQI_train$over_SO2_WHO_crit)
AQI_train$over_PM10_WHO_crit = as.factor(AQI_train$over_PM10_WHO_crit)
AQI_train$over_CO_KOR_crit = as.factor(AQI_train$over_CO_KOR_crit)
AQI_train$over_NO2_WHO_crit = as.factor(AQI_train$over_NO2_WHO_crit)
AQI_train$over_O3_WHO_crit = as.factor(AQI_train$over_O3_WHO_crit)

train = train %>%
  left_join(AQI_train, by = c('date', 'add' = 'area_17'))
colnames(train)

train = train %>% select(!c(yyyymmdd.y,  date, X)) %>%
  rename(yyyymmdd = yyyymmdd.x)
################
# 폭염, 한파
####################
train= train %>% mutate(한파 = as.factor(ifelse(min_mrng_ta <= -12,1,0)),
                        폭염 = as.factor(ifelse(max_dytm_ta >= 33,1,0)))


#####################################
# mmdd, dd
############################

train1 = read.csv("C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/TRAIN_tree_with_파생변수0802.csv")
train2 = read.csv("C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/TRAIN_nottree_with_파생변수0802.csv")
test = read.csv("C:/Users/user/OneDrive/바탕 화면/JHLee/공모전/3. 기상청 날씨에 따른 혈관질환 발병률 예측/데이터/TEST_with_파생변수0802.csv")

colnames(test)
train1 = train1 %>%
  select(!c(X, Unnamed..0)) %>%
  mutate(mmdd = yyyymmdd %% 10000,
         mm = floor(mmdd/100),
         dd= mmdd %% 100)

train2 = train2 %>%
  select(!c(X, Unnamed..0)) %>%
  mutate(mmdd = yyyymmdd %% 10000,
         mm = floor(mmdd/100),
         dd= mmdd %% 100)
test = test %>%
  select(!X) %>%
  mutate(mmdd = yyyymmdd %% 10000,
         mm = floor(mmdd/100),
         dd= mmdd %% 100)



############################################
# 날씨변수 lag1~7
############################################
#### lag_days= k 만큼 lagged vector 만드는 함수, 밀린만큼 단순평균으로 채워줌
lag_fill = function(vect, lag_days){
  lag_vector = dplyr::lag(vect, n= 34*lag_days)
  for(i in 1:34){
    lag_vector[seq(i,34*lag_days, by = 34)] = mean(vect[seq(i,34*lag_days, by = 34)], na.rm = T)
  }
  return(lag_vector)
}

##### var_lag_k 변수 df 만들기
lagdf = function(var, max_lag, colname = 'var'){
  lag_df = data.frame(matrix(nrow = length(var), ncol= max_lag))
  lag_names = vector('character', length = max_lag)
  for(i in 1:max_lag){
    lag_df[,i] = lag_fill(var, lag_days = i)
    lag_names[i] = str_glue(colname,'_lag_',i)
  }
  colnames(lag_df) = lag_names
  return(lag_df)
}


# ta
min_mrng_ta_lags = lagdf(full$min_mrng_ta, 7, 'min_mrng_ta')

# ps
min_ps_lags = lagdf(full$min_ps, 7, 'min_ps')

# hm
min_hm_lags = lagdf(full$min_td_hrmt, 7, 'min_td_hrmt_lags')

