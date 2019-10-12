install.packages("eurostat")

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

merge(DTbmi1, DTbmi2, all = TRUE)

# Percepción de salud -----------------------------------------------------

perc_salud = get_eurostat("hlth_silc_18", time_format = "num")
