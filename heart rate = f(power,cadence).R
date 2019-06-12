Eq1 <- function(x,parms)
{
  x[1] <- (parms[1]/(exp(-4)*x[2]**(1.6)*exp(-0.02*parms[2])))**(1/0.85)+parms[3]
  x
}
# parms = P, C, Hmini
# x = H

temps <- seq(0, 100000, by=60)
cond1 = c(300, 90, 55)
cond2 = c(300, 120, 55)
cond3 = c(300, 60, 55)

y <- Eq1(temps, parms=cond1)
y2 <- Eq1(temps, parms = cond2)
y3 <- Eq1(temps, parms = cond3)

s1 = rep(90,1667)
plot (s1, y)
points(120, y2)
points(60, y3)
y
y2
y3
cadence <- seq(40, 160, by = 1)
y <- Eq1(temps, x = cadence)