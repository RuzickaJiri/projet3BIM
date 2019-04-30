L_t <- function(x,parms)
{
  y <- 1 - exp(-x/parms[1])
}


temps <- seq(1, 500, by=1)
cond1 = c(420)

y1 <- L_t(temps, parms = cond1)
plot (temps, y1)


L_nu <- function(x,parms)
{
  y <- parms[1] + (parms[2]-parms[1])*exp(parms[3]*(x-parms[4]))
}
# parms = L_basal, L(max), alpha_6, nu_max
# x = nu

parameters = c(1, 7, 0.01, 500)
nu = seq(1, 500, by = 1)

y2 <- L_nu(x=nu, parms= parameters)
plot(nu,y2)

L_fin = y1*y2
plot(temps, L_fin)