library(readxl)
library(car)
ciga <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/0 Curso CSIC/Primera parte/ciga.xls")
View(ciga)

summary(ciga)
ciga$Light=factor(ciga$Light)
levels(ciga$Light)=c("no", "si")
summary(ciga)

# Ajuste de una regresión simple

# Relación con el alquitrán
fit1=lm(CO~Alq, data=ciga)
summary(fit1)
plot(CO~Alq, data=ciga)
abline(a=2.74328, b=0.80098)
layout(matrix(c(1,2,3,4),2,2))
plot(fit1)

# Relación con la nicotina
fit2=lm(CO~Nic, data=ciga)
summary(fit2)
plot(CO~Nic, data=ciga)
abline(a=1.6647, b=12.3945)
layout(matrix(c(1,2,3,4),2,2))
plot(fit2)

# Relación con el peso
fit3=lm(CO~Pes, data=ciga)
summary(fit3)
plot(CO~Pes, data=ciga)
abline(a=-11.795, b=25.068)
layout(matrix(c(1,2,3,4),2,2))
plot(fi3)

# Regresión múltiple
fit4=lm(CO~Alq+Nic+Pes, data=ciga)
summary(fit4)
layout(matrix(c(1,2,3,4),2,2))
plot(fit4)


# Relación con "light"
fit5=lm(CO~Alq+Light, data=ciga)
summary(fit5)
layout(matrix(c(1,2,3,4),2,2))
plot(fit5)

plot(CO~Alq, data=ciga, col=as.numeric(ciga$Light)+1)


fit6=lm(CO~Light, data=ciga)
summary(fit6)
plot(CO~Light, data=ciga)

fit7=aov(CO~Light, data=ciga)
summary(fit7)


anova(fit4, fit1)



### Regresion Logistica
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/0 Curso CSIC")
require(foreign)
Votos=read.spss("Votos.sav", to.data.frame =TRUE)
head(Votos)
Votos$Cifras=log10(10*Votos$Contribution+1)

## Un primer análisis de los datos
plot(Contribution~Voto, data=Votos)
plot(Cifras~Voto, data=Votos)
plot(Contribution~Partido, data=Votos)
plot(Cifras~Partido, data=Votos)

gfit1=glm(Voto~Cifras, data=Votos, family=binomial)
summary(gfit1)
anova(gfit1, test = "Chisq")
coefficients(gfit1)
exp(coefficients(gfit1)[2])

x=seq(0, 8, 0.1)
z=- 6.496 + 1.468*x
p=exp(z)/(1+exp(z))
cut=-1*- 6.496/1.468
plot(x,p, type = "l")
abline(v=cut, col="red", lwd=3)

gfit2=glm(Voto~Partido, data=Votos, family=binomial)
summary(gfit2)
anova(gfit2, test = "Chisq")
coefficients(gfit2)
exp(coefficients(gfit2)[2])


gfit3=glm(Voto~Cifras + Partido, data=Votos, family=binomial)
summary(gfit3)
anova(gfit3, test = "Chisq")
coefficients(gfit3)
exp(coefficients(gfit3)[2:3])

x=seq(0, 8, 0.1)
zD=-4.51 + 0.8990*x
zR=-4.51+1.9146 + 0.8990*x
pD=exp(zD)/(1+exp(zD))
pR=exp(zR)/(1+exp(zR))
cutD=4.51/0.899
cutR=(4.51- 1.9146)/0.899
plot(x,pD, type = "l", col="blue")
points(x,pR, type = "l", col="red")
abline(v=cutD, col="blue", lwd=3)
abline(v=cutR, col="red", lwd=3)

gfit4=glm(Voto~Cifras + Partido + Cifras:Partido, data=Votos, family=binomial)
summary(gfit4)
anova(gfit4, test = "Chisq")
coefficients(gfit4)
exp(coefficients(gfit4)[2:4])


x=seq(0, 8, 0.1)
zD=-3.2027 + 0.6184*x
zR=(-3.2027-8.046) + (0.6184+2.0193)*x
pD=exp(zD)/(1+exp(zD))
pR=exp(zR)/(1+exp(zR))
cutD=3.2027/0.6184
cutR=(3.2027+8.046)/(0.6184+2.0193)
plot(x,pD, type = "l", col="blue")
points(x,pR, type = "l", col="red")
abline(v=cutD, col="blue", lwd=3)
abline(v=cutR, col="red", lwd=3)