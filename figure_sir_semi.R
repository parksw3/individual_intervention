library(deSolve)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(EpiEstim)
library(egg)
library(tikzDevice)
source("sir-semi.R")

rr <- runsir(gammafun=gammafun_base2)

dd <- data.frame(
  time=rr$time,
  incidence=rr$incidence
) %>%
  tail(-1) %>%
  mutate(
    time=ceiling(time)
  ) %>%
  group_by(time) %>%
  summarize(
    incidence=sum(incidence)
  )

cori <- estimate_R(round(dd$incidence*1e6),
                   method="parametric_si",
                   config = make_config(t_start=2:119, t_end=3:120, mean_si = 5, std_si = 5))

Ri <- list(
  data.frame(
    time=rr$time,
    est=rr$Rt,
    type="$\\mathcal{R}_{\\textrm{\\tiny i}}(t)$"
  ),
  data.frame(
    time=rr$time,
    est=rr$Rtest,
    type="$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$"
  ),
  data.frame(
    time=cori$R$t_end,
    est=cori$R$`Median(R)`,
    type="EpiEstim"
  ),
  data.frame(
    time=rr$time,
    est=rr$Rforward,
    type="$\\mathcal{R}_{\\textrm{\\tiny forward}}(t)$"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, levels=c("$\\mathcal{R}_{\\textrm{\\tiny i}}(t)$", "$\\mathcal{R}_{\\textrm{\\tiny prop}}(t)$", "EpiEstim",
                               "$\\mathcal{R}_{\\textrm{\\tiny forward}}(t)$"))
  )

Rc <- list(
  data.frame(
    time=rr$time,
    est=rr$Rc,
    type="$\\mathcal{R}_{\\textrm{\\tiny c}}(t)$"
  ),
  data.frame(
    time=rr$time,
    est=rr$wallinga,
    type="Wallinga-Teunis"
  ),
  data.frame(
    time=rr$time,
    est=rr$Rforward,
    type="$\\mathcal{R}_{\\textrm{\\tiny forward}}(t)$"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, levels=c("$\\mathcal{R}_{\\textrm{\\tiny c}}(t)$", "Wallinga-Teunis", "$\\mathcal{R}_{\\textrm{\\tiny forward}}(t)$"))
  )

gen <- rr %>%
  select(time, meang, meanf, meanb) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("meang", "meanf", "meanb"), 
               labels=c("Instantaneous", "Forward", "Backward"))
  )

g1 <- ggplot(Ri) +
  geom_line(aes(time, est, col=type, lty=type), size=1) +
  geom_hline(yintercept=1, size=1, col="gray", lty=2) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("Reproduction number", limits=c(NA, 2)) +
  scale_colour_viridis_d(begin=0, end=0.8) +
  scale_size_manual(values=c(1, 0.7, 0.7)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.9, 0.8),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x = element_blank()
  )

g2 <- ggplot(Rc) +
  geom_line(aes(time, est, col=type, lty=type), size=1) +
  geom_hline(yintercept=1, size=1, col="gray", lty=2) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("Reproduction number", limits=c(0, 2)) +
  scale_colour_viridis_d(begin=0, end=0.8, option="E") +
  scale_size_manual(values=c(1, 0.7, 0.7)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.9, 0.84),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x = element_blank()
  )

g3 <- ggplot(gen) +
  geom_line(aes(time, value, col=key, lty=key), size=1) +
  geom_vline(xintercept=c(25, 40), size=1, col="gray", lty=2) +
  scale_x_continuous("Day", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_continuous("Mean interval (days)", expand=c(0, 0), limits=c(0, 13.5)) +
  scale_colour_viridis_d(begin=0, end=0.8, option="A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.9, 0.84),
    legend.title = element_blank(),
    legend.background = element_blank()
  )

gtot <- ggarrange(g1, g2, g3, nrow=3,
                  labels = c("A", "B", "C"))

tikz(file="figure_sir_semi.tex", width=8, height=8, standAlone = T)
gtot
dev.off()
tools::texi2dvi("figure_sir_semi.tex", pdf=T, clean=T)
