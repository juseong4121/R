df1 <- read.csv('C:/Users/jsmat/OneDrive/바탕 화면/기획2번/기획2번/가족친화인증기업현황_20221231.csv',fileEncoding = 'CP949')
df2 <- read.csv('C:/Users/jsmat/OneDrive/바탕 화면/기획2번/기획2번/가족친화인증기업현황_20181231.csv',fileEncoding = 'CP949')

df1 <- df1 %>% mutate(기간='2022년말')
df2 <- df2 %>% mutate(기간 = '2018년말')

rbind.data.frame(df1,df2)
df1 %>% colnames()
df2 %>% colnames()
colnames(df1) <- c('구분','기언.관.명','분류','지역','기간')
colnames(df2)<- c('구분','기언.관.명','분류','지역','기간')
df <- rbind.data.frame(df1,df2)
#fill:분류 
df$기간 %<>% as.factor()
df %>% head(4)
df$분류 %<>% as.factor()

ggplot(data=df,aes(x=기간,fill=분류))+
  geom_bar(position = position_dodge())+
  labs(title= '가족친화인증기업',x='기간별',y='기관 수')+
  theme_minimal()

df3 <- read.csv('C:/Users/jsmat/OneDrive/바탕 화면/기획2번/문제/고용노동부_일가정양립실태조사 데이터_20231216/DATA_2022년 기준 일가정양립 실태조사.csv',fileEncoding = 'CP949')
df3 <- df3[,c("D_INFO2","C1b")]
colnames(df3) <- c('기업규모별','육아휴직제도_사용가능여부')
df3$기업규모별 %<>% as.factor()
df3$육아휴직제도_사용가능여부 %<>% as.factor()

df3 %>% str()

ggplot(df3,aes(x=기업규모별,fill=육아휴직제도_사용가능여부))+
  geom_bar(position = position_dodge())
levels(df3$육아휴직제도_사용가능여부) <- c('필요한 사람들 모두 사용가능','필요한 사람들 중 일부 사용가능','필요한 사람들 전혀 사용 불가능','비해당') 
levels(df3$기업규모별) <- c('5~9인','10~29인','30~99인','100인이상')
ggplot(df3,aes(x=기업규모별,fill=육아휴직제도_사용가능여부))+
  geom_bar(position = position_dodge())+
  labs(title = '기업규모별 육아휴직 자유롭게 못쓴다 응답',x='기업규모',y='응답수')+
  theme_minimal()
