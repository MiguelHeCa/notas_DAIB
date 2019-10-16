
"eurostat"

install.packages("eurostat")

library(tidyverse)
library(eurostat)
library(data.table)


# Inicio ------------------------------------------------------------------


library(tidyverse)
library(eurostat)
library(data.table)

titulos_ = search_eurostat("death")
disease = search_eurostat("disease")
illness = search_eurostat("illness")
weight = search_eurostat("weight")
health = search_eurostat("health")

obesity = get_eurostat("hlth_ehis_bm1e", time_format = "num")

bmi = get_eurostat("sdg_02_10", time_format = "num")

euro_pop = get_eurostat("demo_pjan", time_format = "num")
euro_pop = as.data.table(label_eurostat(euro_pop))

pop_edad = euro_pop[, age := if_else(age == "Less than 1 year", 0, as.numeric(str_extract(age, "\\d{1,2}")))]
pop_edad = pop_edad[sex == "Total" & !is.na(age)]
pop_edad[, gpo_etario := case_when(
  age == 0 ~ "Less than 1 year",
  age %in% 1:4 ~ "From 1 to 4 years",
  age %in% 5:9 ~ "From 5 to 9 years",
  age %in% 10:14 ~ "From 10 to 14 years",
  age %in% 15:19 ~ "From 15 to 19 years",
  age %in% 20:24 ~ "From 20 to 24 years",
  age %in% 25:29 ~ "From 25 to 29 years",
  age %in% 30:34 ~ "From 30 to 34 years",
  age %in% 35:39 ~ "From 35 to 39 years",
  age %in% 40:44 ~ "From 40 to 44 years",
  age %in% 45:49 ~ "From 45 to 49 years",
  age %in% 50:54 ~ "From 50 to 54 years",
  age %in% 55:59 ~ "From 55 to 59 years",
  age %in% 60:64 ~ "From 60 to 64 years",
  age %in% 65:69 ~ "From 65 to 69 years",
  age %in% 70:74 ~ "From 70 to 74 years",
  age %in% 75:79 ~ "From 75 to 79 years",
  age %in% 80:84 ~ "From 80 to 84 years",
  age %in% 85:89 ~ "From 85 to 89 years",
  age %in% 90:94 ~ "From 90 to 94 years",
  age > 94 ~ "95 years or over",
)]
pop_edad = pop_edad[time %in% 2011:2017, .(pop = sum(values)), by = .(time, geo, gpo_etario)]
pop_edad[, geo := as.character(geo)]
pop_edad = pop_edad[!geo %like% "Euro" & !geo %like% "FRG" & geo != "France (metropolitan)"]
pop_edad[geo %like% "Germany", geo := "Germany"]
pop_edad[geo %like% "Kosovo", geo := "Kosovo"]
pop_edad

hlth_cd_aro = get_eurostat("hlth_cd_aro", time_format = "num")
causas_11_17 = as.data.table(label_eurostat(hlth_cd_aro))
causas_11_17 = causas_11_17[sex == "Total" & resid == "All deaths reported in the country"]
causas_11_17 = causas_11_17[age != "Total"][, (c("age", "geo")) := lapply(.SD, as.character), .SDcols = c("age", "geo")]
causas_11_17 = causas_11_17[!geo %in% c("European Union - 28 countries", "France (metropolitan)")]
causas_11_17[geo %like% "Germany", geo := "Germany"]
causas_11_17 = causas_11_17[, .(gpo_etario = age, icd10, geo, time, muertes = values)]
causas_11_17

causas_tot = pop_edad[causas_11_17, on = c("gpo_etario", "geo", "time")]
causas_tot = causas_tot[!is.na(pop) & !is.na(muertes)]
causas_tot[, tasas := muertes/(pop/1e5)]

espana = causas_tot[geo == "Spain"]

espana[, causa := "Otras enfermedades"]
espana[icd10 %like% "Neoplasm" | icd10 %like% "neoplasm", causa := "Cáncer"]
espana[icd10 %like% "Diabetes", causa := "Diabetes"]
espana[icd10 %like% "circulatory" | icd10 %like% "heart" | icd10 %like% "vascular", causa := "Cardiovascular"]
espana[icd10 %like% "Ulcer", causa := "Úlceras gástricas"]
espana[icd10 %like% "kidney", causa :="Renales"]
espana[icd10 %like% "liver", causa := "Hepáticas"]
espana[icd10 %like% "digestive", causa := "Otras enfermades del aparato digestivo"]

espana = espana[, .(tasas = sum(tasas, na.rm = T)), by = .(time, gpo_etario, causa)]
espana[, gpo_etario := factor(gpo_etario,
                              levels = c(
                                "Less than 1 year",
                                "From 1 to 4 years",
                                "From 5 to 9 years",
                                "From 10 to 14 years",
                                "From 15 to 19 years",
                                "From 20 to 24 years",
                                "From 25 to 29 years",
                                "From 30 to 34 years",
                                "From 35 to 39 years",
                                "From 40 to 44 years",
                                "From 45 to 49 years",
                                "From 50 to 54 years",
                                "From 55 to 59 years",
                                "From 60 to 64 years",
                                "From 65 to 69 years",
                                "From 70 to 74 years",
                                "From 75 to 79 years",
                                "From 80 to 84 years",
                                "From 85 to 89 years",
                                "From 90 to 94 years",
                                "95 years or over"
                              ),
                              labels = c(
                                "Menos de 1 años",
                                "De 1 a 4 años",
                                "De 5 a 9 años",
                                "De 10 a 14 años",
                                "De 15 a 19 años",
                                "De 20 a 24 años",
                                "De 25 a 29 años",
                                "De 30 a 34 años",
                                "De 35 a 39 años",
                                "De 40 a 44 años",
                                "De 45 a 49 años",
                                "De 50 a 54 años",
                                "De 55 a 59 años",
                                "De 60 a 64 años",
                                "De 65 a 69 años",
                                "De 70 a 74 años",
                                "De 75 a 79 años",
                                "De 80 a 84 años",
                                "De 85 a 89 años",
                                "De 90 a 94 años",
                                "95 años o más"
                              ))]
espana = espana[order(gpo_etario)]

ggplot(espana, aes(x = factor(time), y = tasas)) +
  geom_line() +
  facet_wrap(vars(gpo_etario)) +
  theme_light()


espana_diabetes = espana[causa == "Diabetes"]

espana_diabetes_95 = espana[gpo_etario == "95 years or over" & causa == "Diabetes"]

ggplot(espana_diabetes_95, aes(x = factor(time), y = tasas, group = 1)) +
  geom_line()

ggplot(espana_diabetes, aes(x = factor(time), y = tasas, group = 1)) +
  geom_line() +
  facet_wrap(vars(gpo_etario), scales = "free_y")

espana_pc = espana[causa != "Otras enfermedades"]
ggplot(espana_pc, aes(x = factor(time), y = tasas, color = causa, group = causa)) +
  geom_line() +
  guides(color = guide_legend(title = "Causa de muerte")) +
  facet_wrap(vars(gpo_etario), scales = "free_y") + 
  labs(
    x = "Año",
    y = "Tasas de muertes por cada 100.000 habitantes",
    title = "Causa de muerte de enfermades no infecciosas en España",
    caption = "Fuente: elaboración propia con datos de la Oficina Europea de Estadística."
  ) + 
  theme_light() +
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggplot(espana, aes(x = factor(time), y = tasas, color = causa, group = causa)) +
  geom_line() +
  facet_wrap(vars(gpo_etario), scales = "free_y")


causas_tot[, causa := "Otras enfermedades"]
causas_tot[icd10 %like% "Neoplasm" | icd10 %like% "neoplasm", causa := "Cáncer"]
causas_tot[icd10 %like% "Diabetes", causa := "Diabetes"]
causas_tot[icd10 %like% "circulatory" | icd10 %like% "heart" | icd10 %like% "vascular", causa := "Cardiovascular"]
causas_tot[icd10 %like% "Ulcer", causa := "Úlceras gástricas"]
causas_tot[icd10 %like% "kidney", causa :="Renales"]
causas_tot[icd10 %like% "liver", causa := "Hepáticas"]
causas_tot[icd10 %like% "digestive", causa := "Otras enfermades del aparato digestivo"]

causas_to

ggplot(espana_pc, aes(x = factor(time), y = tasas, color = causa, group = causa)) +
  geom_line() +
  guides(color = guide_legend(title = "Causa de muerte")) +
  facet_wrap(vars(gpo_etario), scales = "free_y") + 
  labs(
    x = "Año",
    y = "Tasas de muertes por cada 100.000 habitantes",
    title = "Causa de muerte de enfermades no infecciosas en España",
    caption = "Fuente: elaboración propia con datos de la Oficina Europea de Estadística."
  ) + 
  theme_light() +
  theme(legend.position = "bottom", legend.direction = "horizontal")




espana[, causa := case_when(
  icd10 %like% "Neoplasm" | icd10 %like% "neoplasm" ~ "Cáncer",
  icd10 icd10 %like% "Diabetes" ~ "Diabetes",
  icd10 %like% "circulatory" | icd10 %like% "heart" | icd10 %like% "vascular" ~ "Cardiovascular",
  icd10 %like% "Ulcer" ~ "Úlceras gástricas",
  icd10 %like% "kidney" ~ "Renales",
  icd10 %like% "liver" ~ "Hepáticas",
  icd10 %like% "digestive" ~ "Otras enfermades del aparato digestivo",
  TRUE ~ "Otras enfermedades")]

causas_tot[!is.na(pop) & !is.na(muertes), unique(gpo_etario)]
causas_tot[!is.na(pop) & !is.na(muertes), unique(geo)]
causas_tot[, unique(gpo_etario)]

causas_11_17[geo == "France"]

causas_11_17[pop_edad, on = ]

datos2 = datos[age != "Total" & sex == "Total"]

datos3 = datos2[icd10 %like% "Neoplasm" | icd10 %like% "neoplasm" |
                  icd10 %like% "Diabetes" |
                  icd10 %like% "metabolic" |
                  icd10 %like% "circulatory" |
                  icd10 %like% "heart" |
                  icd10 %like% "vascular" |
                  icd10 %like% "digestive" |
                  icd10 %like% "Ulcer" |
                  icd10 %like% "liver" | 
                  icd10 %like% "kidney"]
datos4 = datos3[resid == "All deaths reported in the country"]

datos4[, geo := as.character(geo)]
datos4[, unique(time)]

datos4[, unique(age)]

datos4[, unique(geo)]

datos3[geo == "United Kingdom", unique(resid)]

datos3[time == 2017 & geo == "Spain", unique(resid)]

pop_edad2

euro_pop_tot = euro_pop[age == "Total" & sex == "Total" & time %in% 2011:2017, .(geo, time, pop = values)]



euro_pop_tot

euro_pop[, unique(age)]

causas_id = get_eurostat("hlth_cd_asdr", time_format = "num")

get_eurostat("hlth_cd_aro", time_format = "num")


causas_lab = label_eurostat(causas_id)

causas = as.data.table(causas_lab)

causas = causas[icd10 %like% "neoplasm" |
         icd10 %like% "diabetes" |
         icd10 %like% "metabolic" |
         icd10 %like% "circulatory" |
         icd10 %like% "heart" |
         icd10 %like% "vascular" |
         icd10 %like% "digestive" |
         icd10 %like% "Ulcer" |
         icd10 %like% "liver" | 
         icd10 %like% "kidney"]
causas2 = causas[, unique(age)]

causas[, unique(icd10)]

causas

str(causas_id)
causas[, unique(time)]

causas[icd10 %like% "neoplasm" |
         icd10 %like% "diabetes" |
         icd10 %like% "metabolic" |
         icd10 %like% "circulatory" |
         icd10 %like% "heart" |
         icd10 %like% "vascular" |
         icd10 %like% "digestive" |
         icd10 %like% "Ulcer" |
         icd10 %like% "liver" | 
         icd10 %like% "kidney"]

causas[, unique(time)]
causas[]

euro_pop_tot[causas, on = c("geo", "time")]

hlth_cd_aro = get_eurostat("hlth_cd_aro", time_format = "num")
datos = as.data.table(label_eurostat(hlth_cd_aro))

datos2 = datos[age != "Total" & sex == "Total"]

datos3 = datos2[icd10 %like% "neoplasm" |
                  icd10 %like% "diabetes" |
                  icd10 %like% "metabolic" |
                  icd10 %like% "circulatory" |
                  icd10 %like% "heart" |
                  icd10 %like% "vascular" |
                  icd10 %like% "digestive" |
                  icd10 %like% "Ulcer" |
                  icd10 %like% "liver" | 
                  icd10 %like% "kidney"]
datos4 = datos3[resid == "All deaths reported in the country"]

datos4[, geo := as.character(geo)]
datos4[, unique(time)]

datos4[, unique(age)]

datos4[, unique(geo)]

datos3[geo == "United Kingdom", unique(resid)]

datos3[time == 2017 & geo == "Spain", unique(resid)]

search("hlth_cd_asdr2")

hlth_cd_asdr2 = get_eurostat("hlth_cd_asdr2", time_format = "num")
tasas_muerte = as.data.table(label_eurostat(hlth_cd_asdr2))
tasas_muerte[, unique(age)]



# Obesidad ----------------------------------------------------------------


obesity = get_eurostat("hlth_ehis_bm1e", time_format = "num")

bmi = get_eurostat("sdg_02_10", time_format = "num")

bmi1 = label_eurostat(get_eurostat("hlth_ehis_de1", "num"))
DTbmi1 = as.data.table(bmi1)
DTbmi1[, unique(bmi)]
DTbmi1 = DTbmi1[sex == "Total" & isced97 == "All ISCED 1997 levels"]
DTbmi1[geo %like% "Germany", geo := "Germany"]
DTbmi1 = DTbmi1[age != "Total"]
DTbmi1 = DTbmi1[, .(time, bmi, geo = as.character(geo), age = droplevels(age), values)]

bmi2 = label_eurostat(get_eurostat("hlth_ehis_bm1e", "num"))
DTbmi2 = as.data.table(bmi2)
DTbmi2 = DTbmi2[sex == "Total" & isced11 == "All ISCED 2011 levels"]
DTbmi2 = DTbmi2[geo != c("European Union - 28 countries")]
DTbmi2[geo %like% "Germany", geo := "Germany"]
DTbmi2 = DTbmi2[!age %in% c(
  "Total",
  "From 15 to 19 years",
  "From 15 to 24 years",
  "From 15 to 64 years",
  "From 18 to 64 years",
  "From 25 to 64 years",
  "From 45 to 64 years",
  "18 years or over",
  "65 years or over"
)]
DTbmi2 = DTbmi2[, .(time, bmi, geo = as.character(geo), age = droplevels(age), values)]


DTbmi = merge(DTbmi1, DTbmi2, all = TRUE)

DTbmi

BMI1 = get_eurostat("hlth_ehis_de1", "num")
BMI2 = get_eurostat("hlth_ehis_bm1e", "num")


BMI = dcast(DTbmi, bmi + age + geo ~ time, value.var = "values")
BMI[, change := (`2014` - `2008`)/`2008`]

BMI[!is.na(change)]
BMI[, unique(bmi)]


obese = BMI[bmi == "Obese" & !is.na(change)][, bmi := droplevels(bmi)]

obese[,unique(age)]

ow_ob = BMI[bmi %in% c("Obese", "Overweight") & !is.na(change)][, bmi := droplevels(bmi)]

ow_ob[order(geo, age, bmi)]
ow_ob =melt(ow_ob, id = 1:3, measure = 4:5, variable.name = "time", value.name = "percent", variable.factor = TRUE)

ow_ob[, .(percent = sum(percent, na.rm = T)), by = .(age, geo, time)]

overweight = BMI[bmi == "Overweight" & !is.na(change)][, bmi := droplevels(bmi)]
overweight_18_44 = overweight[age %in% c("From 18 to 24 years", "From 25 to 34 years", "From 35 to 44 years")][, age := droplevels(age)]
overweight_35_75 = overweight[age %in% c("From 45 to 54 years", "From 55 to 64 years", "From 65 to 74 years")][, age := droplevels(age)]

overweight_18_44

ggplot(overweight_18_44, aes(x = geo, y = `2008`)) +
  geom_point() +
  geom_point(aes(y = `2014`)) + 
  theme_light()

overweight_18_44 = melt(overweight_18_44, id = 1:3, measure = 4:5, variable.name = "time", value.name = "percent", variable.factor = TRUE)

ggplot(overweight_18_44, aes(x = time, y = percent, group = geo)) +
  geom_line() +
  facet_grid(rows = vars(geo), cols = vars(age))



ggplot(overweight) +
  geom_segment(aes(
    x = `2008`,
    xend = `2014`,
    y = fct_reorder2(geo, age, -`2008`),
    yend = fct_reorder2(geo, age, -`2008`)
  ),
  #size = 1,
  color = "#CCCCCC") +
  geom_point(aes(x = `2008`, y = geo, color = "2008"), size = 2) +
  geom_point(aes(x = `2014`, y = geo, color = "2014"), size = 2) +
  scale_color_manual(values = c("gold", "blue")) +
  guides(color = guide_legend(title = "Año")) +
  facet_grid(cols = vars(age)) +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    x = "Porcentaje de la población con sobrepeso",
    y = "Países de la Unión Europea",
    title = "Porcentaje de la población con sobrepeso de países de la Unión Europea de 2008 a 2014.",
    caption = "Datos de la Oficina Europea de Estadística."
  )


DTow1 = DTbmi1[bmi %in% c("Obese", "Overweight")]
DTow1 = DTow1[, .(values = sum(values, na.rm = T)), by = .(time, geo, age)]

DTow2 = DTbmi2[bmi == "Overweight", .(time, geo, age, values)]
DTow2

DTow = merge(DTow1, DTow2, all = TRUE)

BMI = dcast(DTow, age + geo ~ time, value.var = "values")
BMI[, change := (`2014` - `2008`)/`2008`]

BMI[!is.na(change)]

overweight = BMI[!is.na(change)][, age := droplevels(age)]
overweight[, age := factor(
  age,
  labels = c(
    "De 18 a 24 años",
    "De 25 a 34 años",
    "De 35 a 44 años",
    "De 45 a 54 años",
    "De 55 a 64 años",
    "De 65 a 74 años"
  )
)]
overweight[, sort(unique(geo))]
overweight[geo == "Belgium", geo := "Bélgica"][geo == "Cyprus", geo := "Chipre"]
overweight[geo == "Czechia", geo := "Rep. Checa"][geo == "France", geo := "Francia"]
overweight[geo == "Germany", geo := "Alemania"][geo == "Greece", geo := "Grecia"]
overweight[geo == "Hungary", geo := "Hungría"][geo == "Latvia", geo := "Letonia"]
overweight[geo == "Poland", geo := "Polonia"][geo == "Romania", geo := "Rumanía"]
overweight[geo == "Slovakia", geo := "Eslovaquia"][geo == "Slovenia", geo := "Eslovenia"]
overweight[geo == "Spain", geo := "España"][geo == "Turkey", geo := "Turquía"]
overweight = overweight[order(age, `2008`)]
overweight[, direction := if_else(change > 0, "Aumentó", "Disminuyó")]
overweight


ggplot(overweight) +
  geom_segment(aes(
    x = `2008`,
    xend = `2014`,
    y = age,
    yend = age,
    colour = direction
  ),
  size = 4
  ) +
  geom_point(aes(x = `2008`, y = age, color = "2008"), size = 2, alpha = 0.7) +
  geom_point(aes(x = `2014`, y = age, color = "2014"), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("#b2b2b2", "#323232", "#d8003d", "#00f2ad")) +
  guides(color = guide_legend(title = "Año")) +
  facet_wrap(vars(geo), ncol = 3) +
  theme_light(base_size = 18) +
  labs(
    x = "Porcentaje de la población con sobrepeso y obesidad",
    y = "Rangos de edad",
    title = "Porcentaje de la población con sobrepeso de países de la Unión Europea de 2008 a 2014.",
    caption = "Fuente: Datos de la Oficina Europea de Estadística."
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 22),
    plot.caption = element_text(size = 20),
    strip.background = element_rect(fill = "White"),
    strip.text = element_text(colour = "black", size = 20)
  )

overweight[, .(median(`2008`, na.rm = T), median(`2014`, na.rm = T)), by = age]


# Percepción de salud -----------------------------------------------------

perc_salud = get_eurostat("hlth_silc_18", time_format = "num")


set.seed(45)
DT <- data.table(
  i_1 = c(1:5, NA),
  i_2 = c(NA,6,7,8,9,10),
  f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
  c_1 = sample(c(letters[1:3], NA), 6, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
  d_2 = as.Date(6:1, origin="2012-01-01"))
# add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]

# id, measure as character/integer/numeric vectors
melt(DT, id=1:2, measure="f_1")
melt(DT, id=c("i_1", "i_2"), measure=3) # same as above
melt(DT, id=1:2, measure=3L, value.factor=TRUE) # same, but 'value' is factor
melt(DT, id=1:2, measure=3:4, value.factor=TRUE) # 'value' is *ordered* factor


# Poblacion ---------------------------------------------------------------

poblacion_original = fread("pob_edad_es.csv")

# Preparar población
poblacion = fread("pob_edad_es.csv")


poblacion[, length(unique(age))]

# Separar población mayor a 75 años
pob_may_75 = poblacion[age %in% unique(age)[(21-5):21]]
pob_may_75 = pob_may_75[, .(age = "75 años o mayor", pob = sum(pob)), by = comunidad]

# Menores a 75 años, considerando no adultos
pob_men_75 = poblacion[!age %in% unique(age)[(21-5):21]]

# Juntar ambas poblaciones
pob = merge(pob_men_75, pob_may_75, all = TRUE)

# Sacar proporciones para cuotas
pob[, f := pob/sum(pob, na.rm = TRUE), by = comunidad]

# Población por comunidades
pob_total = poblacion[, .(pob = sum(pob)), by = comunidad]
pob_total[, sum(pob)]


muestreo = function(N, error = E) {
  round(N / (error^2 * (N-1) + 1))
}
E = 0.04
# Encuestas
pob_total = pob_total[, .(comunidad, pob, muestra = muestreo(pob))]

# Muestra con cuotas
muestra_pob = pob[pob_total[,.(comunidad, muestra)], on = "comunidad"]
muestra_pob[, cuota := muestra * f]

muestra_pob


# Adultos -----------------------------------------------------------------

# Separar población mayor a 75 años
poblacion[age %in% unique(age)[(21-6):21], unique(age)]
pob_may_75 = poblacion[age %in% unique(age)[(21-5):21]]
pob_may_75 = pob_may_75[, .(age = "75 años o mayor", pob = sum(pob)), by = comunidad]

# Menores a 75 años, considerando no adultos
pob_men_75 = poblacion[!age %in% unique(age)[(21-5):21]]
pob_men_75 = pob_men_75[!age %in% unique(age)[1:3]]

# Juntar ambas poblaciones
pob = merge(pob_men_75, pob_may_75, all = TRUE)

# Sacar proporciones para cuotas
pob[, f := pob/sum(pob, na.rm = TRUE), by = comunidad]

# Población por comunidades
poblacion[!age %in% unique(age)[1:4]]
pob_total = poblacion[!age %in% unique(age)[1:3], .(pob = sum(pob)), by = comunidad]
pob_total[, sum(pob)]

muestreo = function(N, error = E) {
  round(N / (error^1.96 * (N-1) + 1))
}
E = 0.05
# Encuestas
pob_total = pob_total[, .(comunidad, pob, muestra = muestreo(pob))]

# Muestra con cuotas
muestra_pob = pob[pob_total[,.(comunidad, muestra)], on = "comunidad"]
muestra_pob[, cuota := round(muestra * f)]

muestra_pob

# Calcular el error en los estratos

error_estrat = function(N, n, z = 2, p = 0.5, q = 0.5) {
  z * sqrt( ((p*q)/n) * ((N-n)/(N-1)))
}

errormuestra = muestra_pob[, .(comunidad, edad = age, pob, muestra, cuota, error = error_estrat(pob, muestra))]
pob_total[, sum(muestra)]

pob_total

errormuestra
# 
# fwrite(pob_total, "data/muestreo_pob_total.csv")
# fwrite(errormuestra, "data/error_muestra.csv")

# Población de interés ----------------------------------------------------
library(data.table)
library(scales)
library(flextable)

poblacion = fread("pob_edad_es.csv")
poblacion[, comunidad := stringr::str_to_title(comunidad, locale = "es")]

# Seleccionar edades de interés
pob = poblacion[!age %in% c(unique(age)[1:4], unique(age)[(21-6):21])]

pob_total = poblacion[!age %in% unique(age)[1:3], .(pob = sum(pob)), by = comunidad]

# Valor crítico de la distribución Normal estándar
z = 1.96

muestreo = function(N, error = E) round(N / (error^z * (N-1) + 1))
# Error global
E = 0.04

# Encuestas
pob_total = pob_total[, .(comunidad, pob, muestra = muestreo(pob))]
pob_total

# Muestra con cuotas

# Sacar proporciones para cuotas
pob[, f := pob/sum(pob, na.rm = TRUE), by = comunidad]

muestra_pob = pob[pob_total[, .(comunidad, muestra)], on = "comunidad"]
muestra_pob[, cuota := round(muestra * f)]

muestra_pob

# Cuotas por afijación proporcional
error_estrat = function(N, n, z_dn = z, p = 0.5, q = 0.5) {
  z_dn * sqrt( ((p*q)/n) * ((N-n)/(N-1)))
  }

muestra_pob[, error := error_estrat(pob, cuota)]
muestra_pob

# pob_total[, sum(muestra)]

# Preparación de tablas para exportar
pob_total
pob_total_reporte = rbind(pob_total, data.table(comunidad = "Total", pob = pob_total[, sum(pob)], muestra = pob_total[, sum(muestra)]))
pob_total_reporte[, (c("pob", "muestra")) := lapply(.SD, comma, big.mark = ".", decimal.mark = ","), .SDcols = c("pob", "muestra")]
pob_total_reporte = pob_total_reporte[, .(Comunidad = comunidad, Población = pob, Muestra = muestra)]

muestra_pob_reporte = muestra_pob[, .(comunidad, age, pob, f, muestra, cuota, error)]
muestra_pob_reporte[, comunidad := paste0(comunidad, "\nn = ", muestra)]
muestra_pob_reporte[, (c("f", "error")) := lapply(.SD, percent, accuracy = 0.01), .SDcols = c("f", "error")]
muestra_pob_reporte[, (c("pob", "cuota")) := lapply(.SD, comma, big.mark = ".", decimal.mark = ","), .SDcols = c("pob", "cuota")]
muestra_pob_reporte = muestra_pob_reporte[, .(Comunidad = comunidad, Grupo_etario = age, Población = pob, Proporción = f, Cuota = cuota, Error = error)]

# fwrite(pob_total_reporte, "data/muestreo_pob_total.csv")
# fwrite(muestra_pob_reporte, "data/error_muestra.csv")

ptr_tab = flextable(pob_total_reporte)
ptr_tab = align(ptr_tab, j = ~ Comunidad, align = "left", part = "all")
ptr_tab = align(ptr_tab, j = ~ Población + Muestra, align = "right", part = "all")
print(ptr_tab, preview = "docx")

mtr_tab = flextable(muestra_pob_reporte)
mtr_tab = merge_v(mtr_tab, j = ~ Comunidad)
mtr_tab = valign(mtr_tab, j = ~ Comunidad, valign = "top")
mtr_tab = align(mtr_tab, j = ~ Comunidad + Grupo_etario, align = "left", part = "all")
mtr_tab = align(mtr_tab, j = ~ Población + Proporción + Cuota + Error, align = "right", part = "all")

print(mtr_tab, preview = "docx")
