renewal_det <- function(R0=2.5,
                        N=40000,
                        S0=40000-10,
                        dt=0.025,
                        incfun=function(x) dlnorm(x, meanlog=1.62, sdlog=0.42),
                        deathfun=function(x) dlnorm(x, meanlog=2.84, sdlog=0.43),
                        genfun=function(x) dlnorm(x, meanlog=1.54, sdlog=0.37),
                        I0=10,
                        tmax=800,
                        genmax=10000) {
  inc <- incfun(0:genmax*dt+dt)
  inc <- inc/sum(inc)
  dd <- deathfun(0:genmax*dt+dt)
  dd <- dd/sum(dd)
  gen <- genfun(0:genmax*dt+dt)
  gen <- gen/sum(gen)
  tvec <- seq(0, tmax, by=dt)
  Ivec <- rep(0, tmax/dt)
  
  r <- optim(0.19, function(x) (1/sum(exp(-x*(0:genmax*dt+dt)) * gen)-R0)^2,
        lower=0.1,
        upper=0.3,
        method="Brent")[[1]]
  
  ## r=0.1904075 for R0=2.5
  Ivec[1:(2*genmax)] <- exp(r*1:(2*genmax)*dt)
  Ivec[1:(2*genmax)] <- I0*Ivec[1:(2*genmax)]/sum(Ivec[1:(2*genmax)])
  Svec <- rep(0, tmax/dt)
  Svec[1:(2*genmax)] <- N - cumsum(Ivec)[1:(2*genmax)]
  
  for (i in (genmax+1):length(tvec)) {
    Ivec[i] <- Svec[i-1] * R0 * sum(Ivec[max(1, i-genmax):(i-1)] * gen[min(i, genmax+1):2])/N
    Svec[i] <- Svec[i-1] - Ivec[i]
  }
  
  symptomatic <- sapply((genmax+1):(length(Ivec)), function(x) sum(Ivec[x:(x-genmax)]*inc))
  death <- sapply((genmax+1):(length(symptomatic)), function(x) sum(symptomatic[x:(x-genmax)]*dd))
  
  plot(head(tvec, -2*genmax), tail(Ivec, -2*genmax), type="l", log="y", xlim=c(0, 150), ylim=c(1e-6, 1e2),
       xlab="Time (days)", ylab=c("Incidence per time step"))
  lines(head(tvec, -2*genmax), tail(symptomatic, -genmax), col=2)
  lines(head(tvec, -2*genmax), death, col=3)
  legend(
    "bottomleft",
    legend=c("infection", "symptom onset", "death"),
    col=c(1, 2, 3),
    lty=1
  )
  
  list(
    tvec=head(tvec, -2*genmax),
    cI=tail(head(cumsum(Ivec), -genmax), -genmax),
    Ivec=tail(head(Ivec, -genmax), -genmax),
    Svec=tail(head(Svec, -genmax), -genmax),
    gen=gen,
    R0=R0,
    r=r
  )
}

rr <- renewal_det()

plot(rr$Ivec, type="l")


