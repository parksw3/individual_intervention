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

rr1r <- runsir(betafun=betafun_r1, gammafun=gammafun_null)
rr2r <- runsir(betafun=betafun_r2, gammafun=gammafun_null)
rr3r <- runsir(betafun=betafun_r3, gammafun=gammafun_null)

dd1 <- data.frame(
  time=rr1r$time,
  incidence=rr1r$incidence
) %>%
  tail(-1) %>%
  mutate(
    time=ceiling(time)
  ) %>%
  group_by(time) %>%
  summarize(
    incidence=sum(incidence)
  )

dd2 <- data.frame(
  time=rr2r$time,
  incidence=rr2r$incidence
) %>%
  tail(-1) %>%
  mutate(
    time=ceiling(time)
  ) %>%
  group_by(time) %>%
  summarize(
    incidence=sum(incidence)
  )

dd3 <- data.frame(
  time=rr3r$time,
  incidence=rr3r$incidence
) %>%
  tail(-1) %>%
  mutate(
    time=ceiling(time)
  ) %>%
  group_by(time) %>%
  summarize(
    incidence=sum(incidence)
  )

cori1 <- estimate_R(round(dd1$incidence*1e6),
                    method="parametric_si",
                    config = make_config(t_start=2:119, t_end=3:120, mean_si = 5, std_si = 5))

cori2 <- estimate_R(round(dd2$incidence*1e6),
                    method="parametric_si",
                    config = make_config(t_start=2:119, t_end=3:120, mean_si = 5, std_si = 5))

cori3 <- estimate_R(round(dd3$incidence*1e6),
                    method="parametric_si",
                    config = make_config(t_start=2:119, t_end=3:120, mean_si = 5, std_si = 5))

R1 <- list(
  data.frame(
    time=rr1r$time,
    est=rr1r$Rt,
    type="$\\mathcal{R}(t)$"
  ),
  data.frame(
    time=rr1r$time,
    est=rr1r$Rtest,
    type="$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$"
  ),
  data.frame(
    time=cori1$R$t_end,
    est=cori1$R$`Median(R)`,
    type="EpiEstim"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, levels=c("$\\mathcal{R}(t)$", "$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$", "EpiEstim"))
  )

R2 <- list(
  data.frame(
    time=rr2r$time,
    est=rr2r$Rt,
    type="$\\mathcal{R}(t)$"
  ),
  data.frame(
    time=rr2r$time,
    est=rr2r$Rtest,
    type="$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$"
  ),
  data.frame(
    time=cori2$R$t_end,
    est=cori2$R$`Median(R)`,
    type="EpiEstim"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, levels=c("$\\mathcal{R}(t)$", "$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$", "EpiEstim"))
  )

R3 <- list(
  data.frame(
    time=rr3r$time,
    est=rr3r$Rt,
    type="$\\mathcal{R}(t)$"
  ),
  data.frame(
    time=rr3r$time,
    est=rr3r$Rtest,
    type="$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$"
  ),
  data.frame(
    time=cori3$R$t_end,
    est=cori3$R$`Median(R)`,
    type="EpiEstim"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, levels=c("$\\mathcal{R}(t)$", "$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$", "EpiEstim"))
  )

speed1 <- data.frame(
  time=tail(rr1$time, -1),
  incidence=diff(rr1$incidence)/tail(rr1$incidence, -1)
) %>%
  gather(key, value, -time)

speed2 <- data.frame(
  time=tail(rr2$time, -1),
  incidence=diff(rr2$incidence)/tail(rr2$incidence, -1)
) %>%
  gather(key, value, -time)

speed3 <- data.frame(
  time=tail(rr3$time, -1),
  incidence=diff(rr3$incidence)/tail(rr3$incidence, -1)
) %>%
  gather(key, value, -time)

gen1 <- rr1r %>%
  select(time, meang, meanf, meanb) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("meang", "meanf", "meanb"), 
               labels=c("Instantaneous", "Forward", "Backward"))
  )

gen2 <- rr2r %>%
  select(time, meang, meanf, meanb) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("meang", "meanf", "meanb"), 
               labels=c("Instantaneous", "Forward", "Backward"))
  )

gen3 <- rr3r %>%
  select(time, meang, meanf, meanb) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("meang", "meanf", "meanb"), 
               labels=c("Instantaneous", "Forward", "Backward"))
  )

g1 <- ggplot(rr1) +
  geom_line(aes(time, incidence), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 130)) +
  scale_y_continuous("Intantaneous icidence", expand=c(0, 0), limits=c(0, 0.0105)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank()
  )

g2 <- g1 %+% rr2 +
  theme(
    axis.title.y = element_blank()
  )

g3 <- g1 %+% rr3 +
  theme(
    axis.title.y = element_blank()
  )

g4 <- ggplot(R1) +
  geom_line(aes(time, est, col=type, lty=type), size=1) +
  geom_hline(yintercept=1, size=1, col="gray", lty=2) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 130)) +
  scale_y_continuous("Reproduction number") +
  scale_colour_viridis_d(begin=0, end=0.8) +
  scale_size_manual(values=c(1, 0.7, 0.7)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(1.1, 0.84),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x = element_blank()
  )

g5 <- g4 %+% R2 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

g6 <- g4 %+% R3 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )


g7 <- ggplot(speed1) +
  geom_line(aes(time, value), size=1) +
  geom_hline(yintercept=0, lty=2, col="gray", size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 130)) +
  scale_y_continuous("Growth rate (1/days)", expand=c(0, 0), limits=c(-0.0045, 0.0025)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.73, 0.84),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x = element_blank()
  )

g8 <- g7 %+% speed2 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

g9 <- g7 %+% speed3 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

g10 <- ggplot(gen1) +
  geom_line(aes(time, value, col=key, lty=key), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 130)) +
  scale_y_continuous("Mean interval (days)", expand=c(0, 0), limits=c(0, 13.5)) +
  scale_colour_viridis_d(begin=0, end=0.8, option="A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.73, 0.84),
    legend.title = element_blank(),
    legend.background = element_blank()
  )

g11 <- g10 %+% gen2 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

g12 <- g10 %+% gen3 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, nrow=4,
                  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"))

tikz(file="figure_sir_beta.tex", width=8, height=10, standAlone = T)
gtot
dev.off()
tools::texi2dvi("figure_sir_beta.tex", pdf=T, clean=T)
