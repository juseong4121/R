df <- read.csv('C:/Users/jsmat/OneDrive/바탕 화면/포스팅/df_merged_mean.csv',fileEncoding = 'CP949')
df
#결측치 확인
#1. 컬럼별 확인
is.na(df) %>% colSums()
#2. 결측치 행 제거  
na.omit(df)
df[complete.cases(df),]
#3. 결측치 어디서 존재하는지? -> 데이터의 행을 뽑아내고 싶다.
# apply()를 이용해야겠다. 행마다 함수를 적용하자.
df[apply(df,1,FUN = function(x) any(is.na(x))),]

#수치형과 문자형,범주형의 컬럼들로 분류하는 방법.
num_col <- df %>% select_if(function(x) is.numeric(x))
char_col <- df %>% select_if(function(x) is.character(x)|is.factor(x))

#결측치 대체하는 방법 (평균,중앙값 등.)
#ifelse는 Test가 TRUE이면 YES값으로 대체 , FALSE이면 NO값으로 대체
ifelse(test = is.na(num_col$수강금액),yes = mean(num_col$수강금액,na.rm = T),no = num_col$수강금액)
library(stringr)

#str_contains와 동치인 코드 = str_detect = grep()
#강사진 이름 중 패스쥬리? 페스로 시작하는 강사진은? 
df[str_detect(df$강사진,pattern = '페스'),]
df[grep('페스',x = df$강사진),]
str_split(string = df$선수과목,pattern = '/') %>% unlist()
