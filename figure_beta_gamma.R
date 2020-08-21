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

tvec <- seq(0, 120, by=0.02)

s1 <- data.frame(
  time=tvec,
  beta=betafun_r1(tvec)
)

s2 <- data.frame(
  time=tvec,
  beta=betafun_r2(tvec)
)

s3 <- data.frame(
  time=tvec,
  beta=betafun_r3(tvec)
)

s4 <- data.frame(
  time=tvec,
  gamma=gammafun_base1(tvec)
)

s5 <- data.frame(
  time=tvec,
  gamma=gammafun_base2(tvec)
)

s6 <- data.frame(
  time=tvec,
  gamma=gammafun_base3(tvec)
)

g1 <- ggplot(s1) +
  geom_line(aes(time, beta), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 130)) +
  scale_y_continuous("Transmission rate (1/day)", expand=c(0, 0), limits=c(0, 0.35)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank()
  )

g2 <- g1 %+% s2 +
  theme(
    axis.title.y = element_blank()
  )

g3 <- g1 %+% s3 +
  theme(
    axis.title.y = element_blank()
  )

g4 <- ggplot(s4) +
  geom_line(aes(time, gamma), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 130)) +
  scale_y_continuous("Removal rate (1/day)", expand=c(0, 0), limits=c(0, 0.55)) +
  theme(
    panel.grid = element_blank()
  )

g5 <- g4 %+% s5 +
  theme(
    axis.title.y = element_blank()
  )

g6 <- g4 %+% s6 +
  theme(
    axis.title.y = element_blank()
  )

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, nrow=2,
                  labels = c("A", "B", "C", "D", "E", "F"))

tikz(file="figure_beta_gamma.tex", width=8, height=4, standAlone = T)
gtot
dev.off()
tools::texi2dvi("figure_beta_gamma.tex", pdf=T, clean=T)


