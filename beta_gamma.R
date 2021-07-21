library(deSolve)
library(dplyr)
library(tidyr)
library(EpiEstim)
library(shellpipes)

loadEnvironments()

rr2 <- runsir(gammafun=gammafun_base2)

betafun_r2 <- beta_reconstruct(rr2)

tvec <- seq(0, 120, by=0.02)

s2 <- data.frame(
  time=tvec,
  beta=betafun_r2(tvec)
)

s5 <- data.frame(
  time=tvec,
  gamma=gammafun_base2(tvec)
)

speed <- data.frame(
  time=tail(rr2$time, -1),
  incidence=diff(rr2$incidence)/tail(rr2$incidence, -1)
) %>%
  gather(key, value, -time)

saveEnvironment()
