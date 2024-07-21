#다른 예제####
df2 <- read.csv('C:/Users/jsmat/OneDrive/바탕 화면/기획2번/문제/일가정양립회사추천.csv',fileEncoding = 'CP949')
#1. 결측치 확인
is.na(df2) %>% colSums()
#2. 존재한다면 어디서 존재하는지 확인
#is.na(df2)가 모든 요소마다 체크하므로, 
#이를 행으로 하나(컬에서)라도 존재하면 T를 출력하자. 
df2[apply(df2,1,FUN = function(x) any(is.na(x))),] %>% View()

#3.주소와 경력을 팩터로 한번에 변경하라.
#다음은 안되는 예시이다. 두 컬럼을 한번에 변수변환할때 생기는 문제
df2[,c('경력','주소')] %<>% as.factor()
#아래는 되는 예시이다.
df2 %<>% mutate(across(c(경력,주소),as.factor))
df2 %>% str()

#4.데이터 구조를 파악하고 이를 수치형과 문자,팩터형으로 분류하라.
df2 %>% str()
num_col <- df2 %>% select_if(function(x) is.numeric(x))
char_col <- df2 %>% select_if(function(x) is.character(x)|is.factor(x))

#5. 결측치 부분을 '없음'으로 채워라.
df2[apply(df2,1,function(x) any(is.na(x))),] %>% View()
df2$주소<-ifelse(test = is.na(df2$주소),yes = '없음',no = df2$주소)
df2 %>% filter(주소=='없음')

#6. 회사이름에 '게임'이란 단어가 들어간 회사를 추출하려면?
#두가지 방법을 실시하라. stringr 패키지의 str_detect과 grep()함수
df2[str_detect(string = df2$회사,pattern = '게임'),] %>% View()
#실제로 위치를 찾을 때 둘 다 무엇을 써도 상관이 없음.
which(str_detect(string = df2$회사,pattern = '게임')==TRUE)
grep(pattern = '게임',x = df2$회사)
df2[grep(pattern = '게임',x = df2$회사),]

#7. 어떤 직무들이 있는지 확인하라.
#명확하게 보기 어렵다면 이를 간략하게 만들어 보아라.
df2$직무
#간략하게 만들려면 각 직무 별로 뭐가 얼만큼 있는지 체크하면 될 것 같다.
work <- str_split(df2$직무,pattern = ',') %>% unlist()
#3194개의 직무가 존재한다.

#8. 각 회사에서 채용하는 직무 빈도를 시각화 그림으로 나타내라.
# work 데이터를 사용하되, bar와 histogram의 차이점을 서술하고
#무엇을 써야 할 지 이유를 들면서 사용하라.
work %>% table() %>% which.max()
#우선 전화상담이 제일 많은 것으로 확인.
#bar 그래프는 x축을 문자형같은 데이터, y축을 연속형변수를 사용한다.
#histogram 그래프는 x축만을 지정하고 x축이 연속형변수다.
#히스토그램에서 bins()을 이용해서 구간별 막대를 그릴 수 있다.
work %>% colnames()
work %<>% as.data.frame()
colnames(work) <- '직무'
#여기서 work는 직무들로 구성되어 있다.
#각 빈도수를 체크할것이니 이를 table로 바꿔서 freq df로 바꾸자
work_100 <- work[100:150] %>% table() %>% as.data.frame()
work_100 %>% colnames()
#x: 직무, y= 빈도로 하는 bar 그래프 그리면 되겠다.
ggplot(data = work_100,aes(x=.,y=Freq))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()

#10. 제약회사와 바이오회사를 지정해서 직무채용을 시각화해보자.
#grep의 패턴은 c('제약','바이오')같이 개별 패턴으로 인식 못함
# 하나로 묶어서 'x|y'꼴로 해줘야함.
bio <- df2[grep(pattern =  '제약|바이오',x = df2$회사),] 
bio_work <- str_split(bio$직무,pattern = ',') %>% unlist()
bio_work %>% unique()
#너무 많아서 시각화가 힘들다 각 회사로 쪼개서 해보고 직무 갯수를 
#제한하자.
pharmaceutical <- df2[grep(pattern = '제약',x=df2$회사),]
bio <- df2[grep(pattern = '바이오',x=df2$회사),]
pharmaceutical <- str_split(pharmaceutical$직무,pattern = ',') %>% unlist()
pharmaceutical_df <- pharmaceutical[10:30] %>% table() %>% as.data.frame()
bio <- str_split(bio$직무,pattern = ',') %>% unlist()
bio_df <- bio[10:30] %>% table() %>% as.data.frame()
pharmaceutical_df %>% colnames()
#x: 직무 ,y : 빈도로 하는 bar 그래프
ggplot(data = pharmaceutical_df,aes(x=.,y=Freq))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()
ggplot(data = bio_df,aes(x=.,y=Freq))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()

#11. 쪼개진 데이터 제약,바이오 데이터프레임을 합쳐서
#fill을 제약,바이오를 팩터로 하여 시각화하라.
library(tidyr)
pharmaceutical_df %>% colnames()

#pivot_longer는 컬럼을 기준으로 데이터를 길게 늘리는 방법임.
#컬럼이 제약과 빈도가 있음
#names_to : type은 cols로 지정됨.
#내가 제약으로 해두었으므로 컬럼네임인 제약이 계속 반복된다. 
#->type컬럼에
#나머지 values_to는? 그 제약의 요소들이 들어간다.
#요약하자면 cols:제약이면 제약요소들이 values_to 컬럼에 들어가고
#제약이란 이름표(names_to)가 붙어다닌다.

pharmaceutical_df %<>% rename('제약'='.')
pharmaceutical_longer<- pivot_longer(pharmaceutical_df,cols = 제약,names_to = 'Type',values_to = 'Value')

bio_df %<>% rename('바이오'='.') 
bio_df %>% head()
bio_longer <- pivot_longer(bio_df,cols=바이오,names_to = 'Type',values_to = 'Value')

new_df <- rbind.data.frame(pharmaceutical_longer,bio_longer)

ggplot(data=new_df, aes(x=Value,y=Freq,fill=Type))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()
#생명공학이 왜 나눠져서 나올까? 
#공백존재함. 위로 거슬러 올라가서 
#Freq 나타나기전 직무 공백을 수정하면 됨. 
#즉 table 전 데이터..
pharmaceutical <- df2[grep(pattern = '제약',x=df2$회사),]
bio <- df2[grep(pattern = '바이오',x=df2$회사),]
pharmaceutical <- str_split(pharmaceutical$직무,pattern = ',') %>% unlist()
pharmaceutical %<>% trimws()
pharmaceutical_df <- pharmaceutical[10:30] %>% table() %>% as.data.frame()
bio <- str_split(bio$직무,pattern = ',') %>% unlist()
bio %<>% trimws()
bio_df <- bio[10:30] %>% table() %>% as.data.frame()

pharmaceutical_df %<>% rename('제약'='.')
pharmaceutical_longer<- pivot_longer(pharmaceutical_df,cols = 제약,names_to = 'Type',values_to = 'Value')

bio_df %<>% rename('바이오'='.') 
bio_df %>% head()
bio_longer <- pivot_longer(bio_df,cols=바이오,names_to = 'Type',values_to = 'Value')

new_df <- rbind.data.frame(pharmaceutical_longer,bio_longer)

ggplot(data=new_df, aes(x=Value,y=Freq,fill=Type))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()

