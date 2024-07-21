library(ggplot2)
data <- round(runif(n = 255, min = 0, max = 255), digits = 0)

# 데이터 생성
data <- round(runif(n = 64, min = 0, max = 255), digits = 0)

# 표준화
standardized_data <- scale(data)

# Min-Max 스케일링
min_max_scaled_data <- (data - min(data)) / (max(data) - min(data))

# 데이터 프레임으로 결합
df <- data.frame(
  Original = data,
  Standardized = as.numeric(standardized_data),
  MinMaxScaled = min_max_scaled_data
)
df %>% head(3)

# 데이터 프레임을 long form으로 변환 (tidyr 사용.)
#long form : 기본 df의 colname이 하나의 컬 원소에 속해지며 기본 df의 col의 원소가 value라는 col
#에 속해진다. 즉, long form은 기본 df의 colname들의 col 1개와 value라는 col 1개 총 2개의 col로 이루어짐
df_long <- tidyr::pivot_longer(df, cols = everything(), names_to = "Type", values_to = "Value")
#cols=everything()이라 함은 모두 values_to에 투입시키겠다.
#와 동일한 말이고 , 이에 대한 names_to는
#모든 컬럼이 되겠다.
df_long
# 히스토그램 그리기
#alpha = 색의 강렬함, bins = x축을 몇개로 쪼갤것인지 많으면 얇은 선으로 보임.
ggplot(df_long, aes(x = Value, fill = Type)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~Type, scales = "free_x") + #패싯 : Type으로 분류하고 스케일을 
  #각 Type에 맡게 다시 스케일시키는 방식 : 'free_x'
  theme_minimal() +
  labs(title = "Comparison of Original, Standardized, and Min-Max Scaled Data", x = "Value", y = "Frequency")