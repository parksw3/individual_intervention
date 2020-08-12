sirfun <- function(t, y, param, betafun, gammafun) {
  with(as.list(c(y, param)), {
    
    inc <- betafun(t) * S * I
    
    dS <- - inc
    dI <- inc - gammafun(t) * I
    dR <- gammafun(t) * I
    
    list(c(dS, dI, dR), incidence=inc)
  })
}

gammafun_null <- function(t) {
  rep(1/5, length(t))
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

beta_reconstruct <- function(out) {
  time <- out$time
  betarecon <- out$Rtest*1/5/out$S
  
  function(t) {
    approx(time, betarecon, t, rule=2)$y
  }
}

runsir <- function(yini=c(S=1-1e-3, I=1e-3, R=0),
                   tmax=100,
                   tby=0.02,
                   betafun=betafun_base,
                   gammafun=gammafun_base1) {
  par <- NULL
  
  times <- seq(0, tmax, by=tby)
  
  out <- as.data.frame(ode(yini, times, sirfun, par, betafun=betafun, gammafun=gammafun))
  
  tau <- seq(tby, tmax, by=tby)
  
  K_t_tau <- sapply(times, function(t) {
    exp(-cumsum(gammafun(t-tau)*tby))*tby * betafun(t) * out$S[match(t, times)]
  })
  
  Rt <- apply(K_t_tau, 2, sum)
  
  meang <- apply(K_t_tau, 2, function(x) sum(tau * x))/Rt
  
  F_t_tau <- sapply(times, function(t) {
    mm <- matrix(c(1:length(tau), match(t, times)-1+1:length(tau)), ncol=2)
    
    mm <- mm[mm[,1] < length(times) & mm[,2] < length(times),]
    
    K_t_tau[mm]
  })
  
  Rc <- sapply(F_t_tau, sum)
  
  meanf <- sapply(F_t_tau, function(x) sum(tau[1:length(x)] * x))/Rc
  
  b_t_tau <- sapply(tail(times, -1), function(t) {
    mm <- match(t, times)-1
    
    rev(out$incidence[1:mm]) * K_t_tau[1:mm,mm]
  })
  
  meanb <- c(0, sapply(b_t_tau, function(x) sum(tau[1:length(x)] * x))/sapply(b_t_tau, sum))
  
  const <- sum(exp(-gammafun(0)*200000:1*tby))
  
  inc <- out$incidence
  
  inc0 <- inc[1] * exp((betafun(0)-gammafun(0)) * seq(-20, -tby, by=tby))
  
  inc2 <- c(inc0, inc)
  
  Rtest <- tail(inc2, -1)/sapply(1:(length(inc2)-1), function(x) sum(inc2[1:x]* exp(-1/5*(x:1)*tby))) * const
  
  out$Rt <- Rt
  out$Rtest <- tail(Rtest, -length(inc0)+1)
  out$Rc <- Rc
  
  out$meang <- meang
  out$meanf <- meanf
  out$meanb <- meanb
  
  out
}
