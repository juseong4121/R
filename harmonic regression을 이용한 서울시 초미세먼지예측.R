#데이터 전처리,시각화####
data <- read.csv('C:/Users/PJS/Desktop/R 데이터/서울특별시_시간별 (초)미세먼지_20221231/서울시 대기질 자료 제공_2008-2011.csv',header = T,fileEncoding = 'CP949')
#library(dplyr)
#data %>% View()
data$구분 <- as.factor(data$구분)
data %>% View()
#library(ggplot2)
#ggplot은 필터링된 데이터를 직접 참조가 안됨.
new_data <-data[(data$구분 %in% c('관악구','강동구','강남구','강서구','강북구')),]
new_data <- data %>% filter(구분 %in% c('관악구','강동구','강남구','강서구','강북구'))

levels(new_data$구분)
unique(new_data$구분)
colSums(is.na(new_data))
na.omit(new_data)

rownames(new_data) <- 1:nrow(new_data)


#ggplot(data = gwanak_data, aes(x = gwanak_data$일시, y= gwanak_data$초미세먼지.PM25.))+
# geom_point(col='black')


new_data$일시 <- strptime(x = new_data$일시,format= '%Y-%m-%d%H:%M')

new_data$날짜 <- as.Date(new_data$일시)


daily_summary_mean_pm25 <- new_data %>% group_by(날짜,구분) %>% summarise(평균_PM25 = mean(초미세먼지.PM25.,na.rm = T)) %>% ungroup()
daily_summary_mean_pm25
daily_summary_mean_pm25<-na.omit(daily_summary_mean_pm25)

str(daily_summary_mean_pm25)
levels(daily_summary_mean_pm25$구분)
colnames(daily_summary_mean_pm25)


ggplot(data=daily_summary_mean_pm25, aes(x = 날짜, y= 평균_PM25,color=구분))+
  geom_line()+
  theme_minimal()+
  labs(title = "구별 일별 초미세먼지(PM2.5)",
       x = "날짜", y = "평균 PM2.5 (㎍/㎥)")+
  ylim(0,200)


ggplot(daily_summary_mean_pm25, aes(x = 날짜, y = 평균_PM25)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ 구분, ncol = 2) +
  labs(title = "구별 일별 초미세먼지(PM2.5)",
       x = "날짜", y = "평균 PM2.5 (㎍/㎥)") +
  theme_minimal()+
  ylim(0,200)







#시계열 데이터이므로 sample()과 같은 랜덤형 방식은 X 주기성을 꼭 고려해줘야 하므로 순서대로..
daily_summary_mean_pm25
gwanakgu <- daily_summary_mean_pm25 %>% filter(구분 == '관악구')
N<- nrow(gwanakgu)
t <- 1:N
logy <- log(gwanakgu$평균_PM25)
gwanakgu$평균_PM25 <- logy

train_idx <- 1:round(0.7*N)
test_idx <- (round(0.7*N)+1) : N

train_df <- gwanakgu[train_idx,]
test_df <- gwanakgu[test_idx,]

t_train <- 1:nrow(train_df)
t_test <- (nrow(train_df)+1):(nrow(train_df)+nrow(test_df))

create_fourier_feat <- function(time,K,period){
  cos_terms <- sapply(1:K, FUN = function(k) cos(2*pi*k*time/period))
  sin_terms <- sapply(1:K, FUN = function(k) sin(2*pi*k*time/period))
  
  fourier_feat <- cbind(cos_terms,sin_terms)
  colnames(fourier_feat) <- c(paste0('cos',1:K),paste0('sin',1:K))
  return(fourier_feat)
}

#훈련데이터 fitting
fourier_train <- create_fourier_feat(t_train,K=3,period=7)
fourier_train_df <- cbind(y=train_df$평균_PM25,fourier_train)


fourier_test <- create_fourier_feat(t_test,K=3,period=7)


model_fourier <- lm(y~.,data=data.frame(fourier_train_df))
pred_log <- predict(model_fourier, newdata= as.data.frame(fourier_test))
pred <- exp(pred_log)
actual_y  <- exp(test_df$평균_PM25)
mse_fourier <- mean((actual_y - pred)^2,na.rm = T)
mse_fourier


length(actual_y)
df<-data.frame(pred=pred,y=exp(test_df$평균_PM25))

plot(gwanakgu$날짜,exp(gwanakgu$평균_PM25),type='l',ylim=c(0,50))
lines(test_df$날짜,pred,col='blue',lwd=1,lty=2)
legend("topleft", legend = c("실제값", "Fourier 회귀 예측"),
       col = c("grey", "blue"), lty = c(1,2), lwd = 2)

#회귀모델 파라미터 갱신(R2 확인)####
gwanakgu$log_PM25 <- log(gwanakgu$평균_PM25)

train_df <- gwanakgu[train_idx, ]
test_df  <- gwanakgu[test_idx, ]

t_train <- 1:nrow(train_df)
t_test  <- (nrow(train_df)+1):(nrow(train_df)+nrow(test_df))

#period와 K 조정
K <- 1200
period <- 7

fourier_train <- create_fourier_feat(t_train, K = K, period = period)
fourier_test  <- create_fourier_feat(t_test,  K = K, period = period)

model_fourier <- lm(log_PM25 ~ ., data = data.frame(log_PM25 = train_df$log_PM25, fourier_train))

pred_log <- predict(model_fourier, newdata = as.data.frame(fourier_test))
pred <- exp(pred_log)
actual_y <- test_df$평균_PM25

mean((actual_y - pred)^2,na.rm=T)


# 시각화
plot(gwanakgu$날짜, exp(gwanakgu$평균_PM25), type='l')
lines(test_df$날짜, exp(actual_y), col='blue', lwd=1, lty=2)
legend("topleft", legend = c("실제값", "Fourier 회귀 예측"),
       col = c("grey", "blue"), lty = c(1,2), lwd = 2)

# SSR: 예측값과 평균값 사이의 제곱합
ssr <- sum((pred - mean(actual_y))^2)

# SST: 실제값과 평균값 사이의 제곱합
sst <- sum((actual_y - mean(actual_y))^2)

# 테스트셋 기준 R²
r2_test <- ssr / sst
cat("테스트셋 기준 R²:", round(r2_test, 4), "\n")



K_vals <- seq(1200, 1900, by = 100)

# 같은 방식으로 R² 테스트셋 계산
# 그리고 기존 결과와 이어서 플롯 추가

results <- data.frame(K = K_vals, R2_test = NA)

for (i in seq_along(K_vals)) {
  K <- K_vals[i]
  ft_train <- create_fourier_feat(t_train, K, period = 7)
  ft_test  <- create_fourier_feat(t_test,  K, period = 7)
  
  model <- lm(log_PM25 ~ ., data = data.frame(log_PM25 = train_df$log_PM25, ft_train))
  pred_log <- predict(model, newdata = as.data.frame(ft_test))
  pred <- exp(pred_log)
  
  ssr <- sum((pred - mean(actual_y))^2)
  sst <- sum((actual_y - mean(actual_y))^2)
  results$R2_test[i] <- ssr / sst
}

plot(results$K, results$R2_test, type = 'b',
     main = "K에 따른 테스트셋 R² 변화",
     xlab = "K", ylab = "R² (Test)")

#

#회귀모델 MSE,R2를 활용한 파라미터 갱신####
gwanakgu$log_PM25 <- log(gwanakgu$평균_PM25)

train_df <- gwanakgu[train_idx, ]
test_df  <- gwanakgu[test_idx, ]

t_train <- 1:nrow(train_df)
t_test  <- (nrow(train_df)+1):(nrow(train_df)+nrow(test_df))

#period와 K 조정
K <- 1200
period <- 7

fourier_train <- create_fourier_feat(t_train, K = K, period = period)
fourier_test  <- create_fourier_feat(t_test,  K = K, period = period)

model_fourier <- lm(log_PM25 ~ ., data = data.frame(log_PM25 = train_df$log_PM25, fourier_train))

pred_log <- predict(model_fourier, newdata = as.data.frame(fourier_test))
pred <- exp(pred_log)
actual_y <- test_df$평균_PM25

mean((actual_y - pred)^2,na.rm=T)


# 시각화
plot(gwanakgu$날짜, exp(gwanakgu$평균_PM25), type='l')
lines(test_df$날짜, exp(actual_y), col='blue', lwd=1, lty=2)
legend("topleft", legend = c("실제값", "Fourier 회귀 예측"),
       col = c("grey", "blue"), lty = c(1,2), lwd = 2)

# SSR: 예측값과 평균값 사이의 제곱합
ssr <- sum((pred - mean(actual_y))^2)

# SST: 실제값과 평균값 사이의 제곱합
sst <- sum((actual_y - mean(actual_y))^2)

# 테스트셋 기준 R²
r2_test <- ssr / sst
cat("테스트셋 기준 R²:", round(r2_test, 4), "\n")



K_vals <- seq(1200, 1900, by = 100)

# 같은 방식으로 R² 테스트셋 계산
# 그리고 기존 결과와 이어서 플롯 추가

results <- data.frame(K = K_vals, R2_test = NA)

for (i in seq_along(K_vals)) {
  K <- K_vals[i]
  ft_train <- create_fourier_feat(t_train, K, period = 7)
  ft_test  <- create_fourier_feat(t_test,  K, period = 7)
  
  model <- lm(log_PM25 ~ ., data = data.frame(log_PM25 = train_df$log_PM25, ft_train))
  pred_log <- predict(model, newdata = as.data.frame(ft_test))
  pred <- exp(pred_log)
  
  ssr <- sum((pred - mean(actual_y))^2)
  sst <- sum((actual_y - mean(actual_y))^2)
  results$R2_test[i] <- ssr / sst
}

plot(results$K, results$R2_test, type = 'b',
     main = "K에 따른 테스트셋 R² 변화",
     xlab = "K", ylab = "R² (Test)")

