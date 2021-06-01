library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(tikzDevice)
library(gridExtra)
library(egg)

t <- seq(0, 90, by=0.01)
tau <- seq(0, 30, by=0.01)
R0 <- 2

tstart <- 30
tend <- 60

gdist <- function(tau) dgamma(tau, shape=5, rate=1)

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
  scale_x_continuous("Time since infection, $\\tau$ (days)", expand=c(0, 0), limits=c(0, 15)) +
  scale_y_continuous("Forward kernel, $F_{t}(\\tau)$",
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     expand=c(0, 0), limits=c(0, 1.1),
                     sec.axis = sec_axis(~., name="Relative changes in infection kernel", breaks=c(0, 0.5, 1))) +
  theme(
    panel.grid = element_blank()
  )

g1 <- gbase +
  geom_polygon(data=k_pop1, aes(tau, keff), fill="darkred") +
  geom_line(data=k_pop1, aes(tau, keff/den), lwd=1.5) +
  geom_vline(xintercept=sum(k_pop1$keff*k_pop1$tau)/sum(k_pop1$keff), col="black", lty=2) +
  geom_vline(xintercept=sum(k_intrinsic1$den*k_pop1$tau)/sum(k_intrinsic1$den), col="black", lty=1) +
  annotate("text", x=Inf, y=Inf, label="$t=t_{\\textrm{\\tiny{start}}}-5$",
           hjust=1.1, vjust=1.1) +
  theme(
    axis.title.y.right = element_blank(),
    axis.title.x = element_blank()
  )

g2 <- gbase +
  geom_polygon(data=k_pop2, aes(tau, keff), fill="darkred") +
  geom_line(data=k_pop2, aes(tau, keff/den), lwd=1.5) +
  geom_vline(xintercept=sum(k_pop2$keff*k_pop1$tau)/sum(k_pop2$keff), col="black", lty=2) +
  geom_vline(xintercept=sum(k_intrinsic1$den*k_pop1$tau)/sum(k_intrinsic1$den), col="black", lty=1) +
  annotate("text", x=Inf, y=Inf, label="$t=t_{\\textrm{\\tiny{start}}}$",
           hjust=1.1, vjust=1.1)  +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

g3 <- gbase +
  geom_polygon(data=k_pop3, aes(tau, keff), fill="darkred") +
  geom_line(data=k_pop3, aes(tau, keff/den), lwd=1.5) +
  geom_vline(xintercept=sum(k_pop3$keff*k_pop1$tau)/sum(k_pop3$keff), col="black", lty=2) +
  geom_vline(xintercept=sum(k_intrinsic1$den*k_pop1$tau)/sum(k_intrinsic1$den), col="black", lty=1) +
  annotate("text", x=Inf, y=Inf, label="$t=t_{\\textrm{\\tiny{end}}}-5$",
           hjust=1.1, vjust=1.1)  +
  theme(
    axis.title.y.left = element_blank(),
    axis.title.x = element_blank()
  )

g4 <- gbase +
  geom_polygon(data=k_ind1, aes(tau, keff), fill="darkblue") +
  geom_line(data=k_ind1, aes(tau, keff/den), lwd=1.5) +
  geom_vline(xintercept=sum(k_ind1$keff*k_pop1$tau)/sum(k_ind1$keff), col="black", lty=2) +
  geom_vline(xintercept=sum(k_intrinsic1$den*k_pop1$tau)/sum(k_intrinsic1$den), col="black", lty=1) +
  annotate("text", x=Inf, y=Inf, label="$t=t_{\\textrm{\\tiny{start}}}-5$",
           hjust=1.1, vjust=1.1)+
  theme(
    axis.title.y.right = element_blank()
  )

g5 <- gbase +
  geom_polygon(data=k_ind2, aes(tau, keff), fill="darkblue") +
  geom_line(data=k_ind2, aes(tau, keff/den), lwd=1.5) +
  geom_vline(xintercept=sum(k_ind2$keff*k_pop1$tau)/sum(k_ind2$keff), col="black", lty=2) +
  geom_vline(xintercept=sum(k_intrinsic1$den*k_pop1$tau)/sum(k_intrinsic1$den), col="black", lty=1) +
  annotate("text", x=Inf, y=Inf, label="$t=t_{\\textrm{\\tiny{start}}}$",
           hjust=1.1, vjust=1.1)  +
  theme(
    axis.title.y = element_blank()
  )

g6 <- gbase +
  geom_polygon(data=k_ind3, aes(tau, keff), fill="darkblue") +
  geom_line(data=k_ind3, aes(tau, keff/den), lwd=1.5) +
  geom_vline(xintercept=sum(k_ind3$keff*k_pop1$tau)/sum(k_ind3$keff), col="black", lty=2) +
  geom_vline(xintercept=sum(k_intrinsic1$den*k_pop1$tau)/sum(k_intrinsic1$den), col="black", lty=1) +
  annotate("text", x=Inf, y=Inf, label="$t=t_{\\textrm{\\tiny{end}}}-5$",
           hjust=1.1, vjust=1.1)  +
  theme(
    axis.title.y.left = element_blank()
  )

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, nrow=2,
          labels=c("A", "B", "C",
                   "D", "E", "F"))

tikz(file="pop_ind_compare.tex", width=8, height=6, standAlone = T)
gtot
dev.off()
tools::texi2dvi("pop_ind_compare.tex", pdf=T, clean=T)
