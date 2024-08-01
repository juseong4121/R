#for문을 이용한 함수그래핑
#ex1) x^n n=1,2,3,4,5,6의 모든 그래프를 그려라.
func <- NULL
for(i in 0: 5){
  func <- c(func,local({
    val <- i
    function(x) x^(val+1)
  }))
}

# x 값 설정
x <- seq(0, 1, length.out = 100)

# 각 함수의 결과를 데이터 프레임으로 변환
df_list <- lapply(1:6, function(i) {
  data.frame(x = x, y = func[[i]](x), func = paste0("x^", i))
})

# 데이터 프레임 병합
df <- do.call(rbind, df_list)

library(ggplot2)
# ggplot으로 시각화
df %<>% as.data.frame()

ggplot(df, aes(x = x, y = y, color = func)) +
  geom_line(stat = 'identity') +
  labs(title = "Graph of x^n",x = "x",y = "y")+
  theme_minimal()



#ex2)  ((x-n)*x^2)/n n=1,2,3,..,11의 모든 그래프를 그려라. x의 범위[0,1]
func <- NULL
for(i in 0: 10){
  func <- c(func,local({
    val <- i
    function(x) ((x-val)*x^2)/val
  }))
}

# x 값 설정
x <- seq(0, 1, length.out = 100)

# 각 함수의 결과를 데이터 프레임으로 변환
df_list <- lapply(1:11, function(i) {
  data.frame(x = x, y = func[[i]](x), func = paste0("x^", i))
})

# 데이터 프레임 병합
df <- do.call(rbind, df_list)

library(ggplot2)
# ggplot으로 시각화
df %<>% as.data.frame()

ggplot(df, aes(x = x, y = y, color = func)) +
  geom_line(stat = 'identity') +
  labs(title = "Graph of ((x-n)*x^2)/n",x = "x",y = "y")+
  theme_minimal()

