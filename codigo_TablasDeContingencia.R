## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

install.packages("data.table")
install.packages("gmodels")

library(data.table)
library(gmodels)


## ------------------------------------------------------------------------
obs = data.table(Trat = c(
  rep("Trat1", times = 150),
  rep("Trat2", times = 120),
  rep("Trat3", times = 130),
  rep("Trat4", times = 160)
))

obs[, resultado := c(c(
  rep("Peor", times = 7),
  rep("Igual", times = 28),
  rep("Mejor", times = 115)
),
c(
  rep("Peor", times = 15),
  rep("Igual", times = 20),
  rep("Mejor", times = 85)
),
c(
  rep("Peor", times = 10),
  rep("Igual", times = 30),
  rep("Mejor", times = 90)
),
c(
  rep("Peor", times = 5),
  rep("Igual", times = 40),
  rep("Mejor", times = 115)
))]

obs[, ':=' (Trat = factor(Trat, levels = c("Trat1", "Trat2", "Trat3", "Trat4")),
            resultado = factor(resultado, levels = c("Peor", "Igual", "Mejor")))]

## ------------------------------------------------------------------------
table(obs$Trat, obs$resultado)

## ------------------------------------------------------------------------
CrossTable(obs$Trat, obs$resultado)

## ------------------------------------------------------------------------
chisq.test(obs$Trat, obs$resultado)

## ------------------------------------------------------------------------

opi = data.table(Region = factor(
  c(
    rep("País Vasco", times = 100),
    rep("Cataluña", times = 100),
    rep("Galicia", times = 100),
    rep("Resto", times = 100)
  ),
  levels = c("País Vasco", "Cataluña", "Galicia", "Resto")
))

opi[, Opinión := factor(
  c(
    rep("Sí", times = 60),
    rep("No", times = 19),
    rep("Ns/Nc", times = 21),
    rep("Sí", times = 81),
    rep("No", times = 10),
    rep("Ns/Nc", times = 9),
    rep("Sí", times = 79),
    rep("No", times = 8),
    rep("Ns/Nc", times = 13),
    rep("Sí", times = 83),
    rep("No", times = 5),
    rep("Ns/Nc", times = 12)
  ),
  levels = c("Sí", "No", "Ns/Nc")
)]


## ------------------------------------------------------------------------
with(opi,
     CrossTable(
       Opinión,
       Region,
       prop.r = F,
       prop.c = F,
       prop.t = F,
       prop.chisq = F,
       chisq = T
     ))


## ------------------------------------------------------------------------
with(opi, chisq.test(Opinión, Region))


## ------------------------------------------------------------------------
opi_s_vasco = opi[Region != "País Vasco"]

with(
  opi_s_vasco,
  CrossTable(
    Opinión,
    Region,
    prop.r = F,
    prop.c = F,
    prop.t = F,
    prop.chisq = F,
    chisq = T
  )
)


## ------------------------------------------------------------------------
opi[, catgalres := ifelse(Region == "País Vasco", "País Vasco", "Resto de España")]

with(
  opi,
  CrossTable(
    Opinión,
    catgalres,
    prop.r = F,
    prop.c = F,
    prop.t = F,
    prop.chisq = T,
    chisq = T
  )
)
