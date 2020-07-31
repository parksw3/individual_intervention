sirfun <- function(t, y, param, betafun, gammafun) {
  with(as.list(c(y, param)), {
    
    inc <- betafun(t) * S * I
    
    dS <- - inc
    dI <- inc - gammafun(t) * I
    dR <- gammafun(t) * I
    
    list(c(dS, dI, dR), incidence=inc)
  })
}

gammafun_base1 <- function(t) {
  ifelse(t < 40, ifelse(t < 25, 1/5, 1/2), 1/3)
}

gammafun_base2 <- function(t) {
  ifelse(t < 40, ifelse(t < 25, 1/5, 1/2), 1/4)
}

gammafun_base3 <- function(t) {
  ifelse(t < 40, ifelse(t < 25, 1/5, 1/2), 1/5)
}

betafun_base <- function(t) {
  3/10
}

runsir <- function(yini=c(S=1-1e-3, I=1e-3, R=0),
                   tmax=100,
                   tby=0.01,
                   betafun=betafun_base,
                   gammafun=gammafun_base) {
  par <- NULL
  
  times <- seq(0, tmax, by=tby)
  
  out <- as.data.frame(ode(yini, times, sirfun, par, betafun=betafun, gammafun=gammafun))
  
  kint <- sapply(times, function(t) {
    tau <- seq(0.01, 100, by=0.01)
    
    sum(exp(-cumsum(gammafun(t-tau)*0.01))*0.01)
  })
  
  Rt <- kint * betafun(tt) * out$S
  
  const <- sum(exp(-gammafun(0)*200000:1*tby))
  
  inc <- out$incidence
  
  inc0 <- inc[1] * exp((betafun(0)-gammafun(0)) * seq(-20, -tby, by=tby))
  
  inc2 <- c(inc0, inc)
  
  Rtest <- tail(inc2, -1)/sapply(1:(length(inc2)-1), function(x) sum(inc2[1:x]* exp(-1/5*(x:1)*tby))) * const
  
  plot(Rt, type="l")
  lines(tail(Rtest, -length(inc0)), col=2)
  
  out$Rt <- Rt
  out$Rtest <- tail(Rtest, -length(inc0)+1)
  
  out
}


