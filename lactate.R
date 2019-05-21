L_t <- function(x,parms)
{
  y <- 1 - exp(-x/parms[1])
}


temps <- seq(1, 500, by=1)
cond1 = c(420)

y1 <- L_t(temps, parms = cond1)
plot (temps, y1, xlab="temps", ylab="Lactate", 
      main="Concentration de lactate temps")


L_nu <- function(x,parms)
{
  y <- parms[1] + (parms[2]-parms[1])*exp(parms[3]*(x-parms[4]))
}
# parms = L_basal, L(max), alpha_6, nu_max
# x = nu

parameters = c(1, 7, 0.01, 500)
nu = seq(1, 500, by = 1)

y2 <- L_nu(x=nu, parms= parameters)
plot(nu,y2, xlab="Intensité", ylab="Lactate", 
     main="Concentration de lactate lambda")

L_fin = y1*y2
plot(temps, L_fin, xlab="temps", ylab="Lactate", 
     main="Concentration de lactate (lambda+temps)")


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
plot(cad, puis, xlab="cadence", ylab="puissance", 
    main="Puissance en fonction de la cadence")

# avec la cadence (intermediaire puissance)
y3 <- L_nu(x=puis, parms= parameters)

L_fin_puis = y1*y3
plot(cad, L_fin_puis,ylim = c(0,10), xlab="Cadence", ylab="Lactate", 
  main="Concentration de lactate (lambda+temps) en fonction de la cadence")
# on remarque que la cadence qui optimise la fatigue
# en prenant la plus grande puissance possible
# est cad = 68,7

# pistes pour la prochaine fois:
# couples puissance : cadence (moyenne de puissance)
# puissance constante (voir au_dessus)

#14/05
puis_const = rep(600,500)
y4 <- L_nu(x=puis_const, parms= parameters)

L_fin_puis_const = y1*y4
plot(cad, L_fin_puis_const, xlab="Cadence", ylab="Lactate", 
  main="Concentration de lactate (lambda_constante+temps) 
     en fonction de la cadence")

#Demarche : fixer la cadence (pour toutes les cadences) onfait varier la heart_rate 
#et on calcule la moyenne

Puissance_heart <- function(x,parms)
{
  y <- parms[1] * parms[5]^parms[2]*exp(-parms[3]*parms[5])*x^parms[4]
  
}


cad = seq(50, 180, by = 1)
heart_rate = seq(50, 200, by = 1)
means_puis = 0
puis_actu = 0

#paramètres dépendent de l'individu - à mesurer
for (i in cad) {
  parms_ph = c(exp(-4),1.6,0.02,0.95, i)  #paramètres à modifier
  puis_actu = Puissance_heart(x=heart_rate, parms= parms_ph)
  means_puis[i-49] = mean(puis_actu)
}
 
plot(cad, means_puis, xlab="Cadence", ylab="Puissance", main="
     Puissance en fonction de cadence et fréq. cardiaque")
#abline(v=80)



t <- seq(1, 655, by=5)

y6 <- L_t(t, parms = cond1)
y5 <- L_nu(x=means_puis, parms= parameters)
L_fin_puis_nouv = y6*y5
plot(cad, L_fin_puis_nouv, xlab="Cadence", ylab="Lactate", 
  main="Concentration de lactate (lambda_constante+temps) 
  en fonction de la cadence")


#fin modélisation cadence, maintenant on voudrait modéliser muscle stress
#pendant un effort en fonction de la cadence(pour éliminer les petites cadences)
