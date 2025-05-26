set.seed(4121)
###ex1####
n <- 1000
X <- rnorm(n = n, mean = 2, sd = .7)

g <- function(x) 1/x
library(Deriv)
g2 <- Deriv(Deriv(f = g,x = 'x'),'x') 

mean_gX <- mean(g(X)) # 실제 평균
appro_1st <- g(mean(X)) #1차근사
appro_2nd <- g(mean(X)) + 0.5 * g2(mean(X))*var(X) #2차근사

cat('정확한 실제 평균 E[1/x] = ', mean_gX, "\n")
cat('1차 근사 평균 1/mu = ', appro_1st, "\n")
cat('2차 근사 평균 1/mu + (1/2)*g^(2) (mu)*Var(X) =', appro_2nd,'\n')
cat('몬테카를로 시뮬레이션 E[g(hat_p)] = ', sim_mean,'\n')
Nsim  <- 10000
hat_p  <- replicate(Nsim, mean(rnorm(n = n,mean = 2,sd = 0.0007)))
g_hat  <- g(hat_p)
sim_mean <- mean(g_hat)

####ex2###

n <- 1000
p <- 0.3
X <- rbinom(n = n, size = 1,prob = p )
X

g <- function(x) x/(1-x)
g2 <- Deriv(Deriv(f = g,x = 'x'),'x') 
var_hat_p <- p*(1-p)/n

delta1 <- g(p)
delta2 <- g(p) + 0.5 * g2(p) * var_hat_p
cat("델타 1차 근사 g(p) =", round(delta1, 5), "\n")
cat("델타 2차 근사 g(p)+(1/2)*g^(2) (p)var(x) =", round(delta2, 5), "\n")
####시뮬레이션#####
Nsim  <- 10000  # 시뮬레이션 반복 횟수 
hat_p  <- replicate(Nsim, mean(rbinom(n, size = 1, prob = p)))
# mean(rbinom(n, size = 1, prob = p)) = hat{p}
head(hat_p) 
#ex. 동전 던지기를 1000번 시행한 후 그 앞면 수를 계산.
#이를 10번 반복하여 앞면(1) 개수를 기록
results <- replicate(10, sum(rbinom(1e3, size = 1, prob = 0.5)))
results

g_hat  <- g(hat_p)
sim_mean <- mean(g_hat)
cat('몬테카를로 시뮬레이션 E[g(hat_p)] = ', sim_mean,'\n')
cat("델타 1차 근사 g(p) =", round(delta1,5), "\n")
cat("델타 2차 근사 g(p)+(1/2)*g^(2) (p)var(x) =", round(delta2,    5), "\n")


####실제값####
p <- 0.3
n <- 1000

y <- 0:(n-1)  # k=n 제외
pmf <- dbinom(y, size = n, prob = p)
odds_vals <- y / (n - y)

E_odds <- sum(odds_vals * pmf)
cat("E[hat{p}/(1-hat{p})] =", E_odds, "\n")
