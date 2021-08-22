library(deSolve)
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
library(EpiEstim)
library(egg)
library(tikzDevice)
source("sir-semi.R")

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

g1 <- ggplot(s2) +
  geom_line(aes(time, beta), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("$\\beta(t)$ (1/day)", expand=c(0, 0), limits=c(0, 0.35)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank()
  )

g2 <- ggplot(s5) +
  geom_line(aes(time, gamma), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("$\\gamma(t)$ (1/day)", expand=c(0, 0), limits=c(0, 0.55)) +
  theme(
    panel.grid = element_blank()
  )

g3 <- ggplot(rr2) +
  geom_line(aes(time, incidence), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("Incidence (1/day)", expand=c(0, 0), limits=c(0, 0.004)) +
  scale_colour_viridis_d(begin=0, end=0.8, option="B") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(0.73, 0.84),
    legend.title = element_blank()
  )

g4 <- ggplot(speed) +
  geom_line(aes(time, value), size=1) +
  geom_hline(yintercept=0, lty=2, col="gray", size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("Growth rate (1/days)", expand=c(0, 0), limits=c(-0.012, 0.006)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.73, 0.84),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x = element_blank()
  )

gtot <- ggarrange(g1, g2, g3, g4, nrow=4,
                  labels = c("A", "B", "C", "D"))

tikz(file="figure_beta_gamma.tex", width=8, height=8, standAlone = T)
gtot
dev.off()
tools::texi2dvi("figure_beta_gamma.tex", pdf=T, clean=T)
