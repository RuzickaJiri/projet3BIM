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


#####################################
#Puissance equation
Puissance <- function(x,parms)
{
  y <- parms[1] * x^parms[2]*exp(-parms[3]*x[1])*160^0.86
  #*x[2]^parms[4] 
  
}

param = c(exp(-4),1.6,0.02,0.85)
cad = seq(30.3, 180, by = 0.3)
heart_rate = seq(50, 200, by = 1)
#x = data.frame(cad,heart_rate)

puis = Puissance(x=cad, parms= param)
plot(cad, puis)
puis_const = rep(300,500)

# avec la cadence (intermediaire puissance)
y3 <- L_nu(x=puis_const, parms= parameters)

L_fin_puis = y1*y3
plot(cad, L_fin_puis,ylim = c(0,10))
# on remarque que la cadence qui optimise la fatigue
# en prenant la plus grande puissance possible
# est cad = 68,7

# pistes pour la prochaine fois:
# couples puissance : cadence (moyenne de puissance)
# puissance constante (voir au_dessus)





