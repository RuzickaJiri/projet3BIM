Eq3 <- function(x,parms)
{
  y <- parms[1] + (parms[2]-parms[1])*exp(parms[3]*(x-parms[4]))
}
# parms = L_basal, L(max), alpha_6, nu_max
# x = nu

parameters = c(1, 7, 0.01, 500)
nu = seq(1, 500, by = 1)

y <- Eq3(x=nu, parms= parameters)
plot(nu,y)
