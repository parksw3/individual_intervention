library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(tikzDevice)
library(gridExtra)
library(egg)

t <- seq(0, 90, by=0.01)
tau <- seq(0, 15, by=0.01)
R0 <- 2

tstart <- 30
tend <- 60

gdist <- function(tau) dweibull(tau, shape=2.826, scale=5.665)

Pfun <- function(t1, tstart, tend) {
  if (t1 >= tstart && t1 <= tend) {
    0.5
  } else {
    1
  }
}

Ifun <- function(t1, t2, tstart, tend) {
  if (t1 <= tstart) {
    1
  } else if (tend < t2) {
    1
  } else {
    exp(-1/5 * (min(t1, tend) - max(tstart, t2)))
  }
}

k_intrinsic1 <- data_frame(
  tau=tau,
  den=R0*gdist(tau),
  t2=tstart-5,
  t1=t2+tau
)

k_intrinsic2 <- data_frame(
  tau=tau,
  den=R0*gdist(tau),
  t2=tstart,
  t1=t2+tau
)

k_intrinsic3 <- data_frame(
  tau=tau,
  den=R0*gdist(tau),
  t2=tend-5,
  t1=t2+tau
)

k_pop1 <- k_intrinsic1 %>%
  group_by(t1, t2) %>%
  mutate(
    pop=Pfun(t1, tstart, tend)
  ) %>%
  mutate(
    keff=den*pop
  )

k_pop2 <- k_intrinsic2 %>%
  group_by(t1, t2) %>%
  mutate(
    pop=Pfun(t1, tstart, tend)
  ) %>%
  mutate(
    keff=den*pop
  )

k_pop3 <- k_intrinsic3 %>%
  group_by(t1, t2) %>%
  mutate(
    pop=Pfun(t1, tstart, tend)
  ) %>%
  mutate(
    keff=den*pop
  )

k_ind1 <- k_intrinsic1 %>%
  group_by(t1, t2) %>%
  mutate(
    ind=Ifun(t1, t2, tstart, tend)
  ) %>%
  mutate(
    keff=den*ind
  )

k_ind2 <- k_intrinsic2 %>%
  group_by(t1, t2) %>%
  mutate(
    ind=Ifun(t1, t2, tstart, tend)
  ) %>%
  mutate(
    keff=den*ind
  )

k_ind3 <- k_intrinsic3 %>%
  group_by(t1, t2) %>%
  mutate(
    ind=Ifun(t1, t2, tstart, tend)
  ) %>%
  mutate(
    keff=den*ind
  )

gbase <- ggplot(k_intrinsic1) +
  geom_polygon(aes(tau, den), fill="gray70") +
  scale_x_continuous("Time since infection (days), $\\tau$", expand=c(0, 0)) +
  scale_y_continuous("Effective kernel, $K_{\\textrm{\\tiny{eff}}}$", expand=c(0, 0), limits=c(0, 0.399)) +
  theme(
    panel.grid = element_blank()
  )

g1 <- gbase +
  geom_polygon(data=k_pop1, aes(tau, keff), fill="darkred")

g2 <- gbase +
  geom_polygon(data=k_pop2, aes(tau, keff), fill="darkred")

g3 <- gbase +
  geom_polygon(data=k_pop3, aes(tau, keff), fill="darkred")

g4 <- gbase +
  geom_polygon(data=k_ind1, aes(tau, keff), fill="darkblue")

g5 <- gbase +
  geom_polygon(data=k_ind2, aes(tau, keff), fill="darkblue")

g6 <- gbase +
  geom_polygon(data=k_ind3, aes(tau, keff), fill="darkblue")

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, nrow=2,
          labels=c("A", "B", "C",
                   "D", "E", "F"))

tikz(file="pop_ind_compare.tex", width=8, height=6, standAlone = T)
gtot
dev.off()
tools::texi2dvi("pop_ind_compare.tex", pdf=T, clean=T)
