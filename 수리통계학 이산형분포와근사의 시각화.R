#이항분포의 근사.
barplot(dbinom(x = x,size = 100,prob = 0.005),from=0,to=20,main='binomial')
barplot(dpois(lambda = 100*0.005,x = x),from=0,to=20,main='pois')

#0.포아송분포의 lambda에 따른 그래프 변화.
library(ggplot2)
library(magrittr)
poi <- NULL
for(i in 1:10){
  poi <- c(poi,local({
    lambda <- i 
    function(x) (exp(-lambda)*(lambda)^x)/factorial(x)
  }))
}

x <- seq(0,20,length.out=100)
x
df_list<-lapply(X = 1:10,FUN =function(col) {
  data.frame(x=x,y=poi[[col]](x),lambda_val = col)})

df <- do.call(rbind,df_list)
View(df)
ggplot(data = df,aes(x=x,y=y,color=factor(lambda_val)))+
  geom_line()

#1.binomial에서 n을 facet으로 나뉘어 각 p별로 확인하라.
#함수 미지수가 두가지 존재해야함 n, p
#그래프가 난잡할 수 있으므로, p = [0,0.3], n=[70,100]
f <- function(x,n,p){
  choose(n,x)*(p^x)*(1-p)^(n-x)
}

n_val <- c(10,20,30,70)
p_val <- seq(0,.3,length.out=10)
df <- NULL

for(n in n_val){
  for(p in p_val){
    for(x in 0:max(n_val)){
      binom_prob <- f(x,n,p)
      df <- rbind.data.frame(df,data.frame(n=n,p=p,x=x,binom_prob=binom_prob))
    }
  }
}

p<-ggplot(data=df,aes(x=x,y=binom_prob,color=factor(p)))+
  geom_line()+
  facet_wrap(~n,scale='free_y')

p + geom_line(data=df[(df$n==70)&(df$p==unique(sort(df$p))[2]),],aes(x=x,y=binom_prob),color='red',lwd=1.2)

#한 그래프 안에 실제로 근사됨을 확인시키자.
#2. np=lambda로 지정하고 포아송분포에 넣어 실제로 근사함을 보여라.####

df$lambda <- (df$n)*df$p

poi <- function(x,lambda){
  ((exp(-lambda)*(lambda)^x)/factorial(x))
}
df %>% colnames()

df$poi_prob<-apply(X = df,MARGIN = 1,FUN = function(row){
  poi(row[3],row[5])
})

#확률 컬럼을 서로 겹쳐야함.
library(tidyr)
df %>% colnames
df_longer <- pivot_longer(data = df,cols = c('binom_prob','poi_prob'),names_to ='total_prob' ,values_to = 'value')

p<-ggplot(data=df_longer,aes(x=x,y=value,color=factor(total_prob)))+
  geom_line()+
  facet_wrap(~n,scale='free_y')


p<-ggplot(data=df_longer[df_longer$n==30,],aes(x=x,y=value,color=factor(total_prob)))+
  geom_line()
p + geom_line(data=df_longer[(df_longer$n==30)&(df_longer$total_prob=='poi_prob'),],aes(x=x,y=value),color='blue')+
  ylim(0,0.2)
