library(ggplot2); theme_set(theme_bw(base_size=18))
library(EpiEstim)
library(egg)
library(tikzDevice)
library(shellpipes)

loadEnvironments()

bet <- ggplot(s2) +
  geom_line(aes(time, beta), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("$\\beta(t)$ (1/day)", expand=c(0, 0), limits=c(0, 0.35)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank()
  )

gam <- ggplot(s5) +
  geom_line(aes(time, gamma), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("$\\gamma(t)$ (1/day)", expand=c(0, 0), limits=c(0, 0.55)) +
  theme(
    panel.grid = element_blank()
  )

ts <- ggplot(rr2) +
  geom_line(aes(time, incidence), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("Incidence (1/day)", expand=c(0, 0), limits=c(0, 0.0105)) +
  scale_colour_viridis_d(begin=0, end=0.8, option="B") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(0.73, 0.84),
    legend.title = element_blank()
  )


tikz(file="scenarios.tex", width=6, height=8, standAlone = T)
print(ggarrange(gam, bet, ts, nrow=3))
