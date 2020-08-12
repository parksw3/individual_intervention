library(deSolve)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(EpiEstim)
library(egg)
library(tikzDevice)
source("sir-semi.R")

rr1 <- runsir(gammafun=gammafun_base1)
rr2 <- runsir(gammafun=gammafun_base2)
rr3 <- runsir(gammafun=gammafun_base3)

dd1 <- data.frame(
  time=rr1$time,
  incidence=rr1$incidence
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
  time=rr2$time,
  incidence=rr2$incidence
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
  time=rr3$time,
  incidence=rr3$incidence
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
    time=rr1$time,
    est=rr1$Rt,
    type="$\\mathcal{R}(t)$"
  ),
  data.frame(
    time=rr1$time,
    est=rr1$Rtest,
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
    time=rr2$time,
    est=rr2$Rt,
    type="$\\mathcal{R}(t)$"
  ),
  data.frame(
    time=rr2$time,
    est=rr2$Rtest,
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
    time=rr3$time,
    est=rr3$Rt,
    type="$\\mathcal{R}(t)$"
  ),
  data.frame(
    time=rr3$time,
    est=rr3$Rtest,
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

gen1 <- rr1 %>%
  select(time, meang, meanf, meanb) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("meang", "meanf", "meanb"), 
               labels=c("Instantaneous", "Forward", "Backward"))
  )

gen2 <- rr2 %>%
  select(time, meang, meanf, meanb) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("meang", "meanf", "meanb"), 
               labels=c("Instantaneous", "Forward", "Backward"))
  )

gen3 <- rr3 %>%
  select(time, meang, meanf, meanb) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("meang", "meanf", "meanb"), 
               labels=c("Instantaneous", "Forward", "Backward"))
  )

g1 <- ggplot(rr1) +
  geom_line(aes(time, incidence), size=1) +
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

g7 <- ggplot(gen1) +
  geom_line(aes(time, value, col=key, lty=key), size=1) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 130)) +
  scale_y_continuous("Mean interval (days)", expand=c(0, 0), limits=c(0, 13.5)) +
  scale_colour_viridis_d(begin=0, end=0.8, option="A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.73, 0.84),
    legend.title = element_blank(),
    legend.background = element_blank()
  )

g8 <- g7 %+% gen2 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

g9 <- g7 %+% gen3 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, nrow=3,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"))

tikz(file="figure_sir_semi.tex", width=8, height=8, standAlone = T)
gtot
dev.off()
tools::texi2dvi("figure_sir_semi.tex", pdf=T, clean=T)
