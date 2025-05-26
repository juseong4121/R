set.seed(4121)

# 파라미터
p    <- 0.3
n    <- 50
Nsim <- 10000

# 결과를 저장할 빈 벡터 미리 생성 : 1만개 0의 값 생성.
hat_p <- numeric(Nsim)

# for 문으로 몬테카로 반복
for (i in seq_len(Nsim)) {
    # 1) n개의 Bernoulli(p) 표본 생성
    x <- rbinom(n, size = 1, prob = p)
    # 2) 표본평균 계산
    hat_p[i] <- mean(x)
}

# 결과 확인
head(hat_p)
length(hat_p)  # Nsim 만큼 저장됐는지 확인
tail(hat_p)
