library(deSolve)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(EpiEstim)
library(egg)
library(tikzDevice)
source("sir-semi.R")

rr1 <- runsir(gammafun=gammafun_base1)
rr2 <- runsir(gammafun=gammafun_base2)
rr3 <- runsir(gammafun=gammafun_base3)

betafun_r1 <- beta_reconstruct(rr1)
betafun_r2 <- beta_reconstruct(rr2)
betafun_r3 <- beta_reconstruct(rr3)

rr1a <- runsir(gammafun=gammafun_base1, tmax=150)
rr2a <- runsir(gammafun=gammafun_base2, tmax=150)
rr3a <- runsir(gammafun=gammafun_base3, tmax=150)

rr1r <- runsir(betafun=betafun_r1, gammafun=gammafun_null, tmax=150)
rr2r <- runsir(betafun=betafun_r2, gammafun=gammafun_null, tmax=150)
rr3r <- runsir(betafun=betafun_r3, gammafun=gammafun_null, tmax=150)

plot(rr2a$incidence, type="l", log="y")
lines(rr2r$incidence, col=2)

plot(rr3a$I, type="l", log="y")
lines(rr3r$I, col=2)

