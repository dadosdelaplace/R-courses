
library(tidyverse)
# Notas simuladas

# qnorm --> quantiles
# pnorm --> probabildiad
# dnorm --> función de distirbuición
# rnorm --> generar una muestra
# pois 
# binom

datos <- 
  tibble("id_alumno" = 1:30,
         "notas" = rnorm(n = 30, mean = 6, sd = 2)) |> 
  mutate("notas_recod" = 
           case_when(is.na(notas) ~ "desconocido",
                     notas < 5 ~ "suspenso",
                     notas < 7 ~ "aprobado",
                     notas < 9 ~ "notable",
                     TRUE ~ "sobresaliente")
           ) |> 
  mutate("notas_recod_fct" =
           factor(notas_recod,
                  levels = c("suspenso", "aprobado", "notable", "sobresaliente"),
                  labels = c("F", "C", "B", "A"),
                  ordered = TRUE))



















library(tidyverse)
library(haven)

# 1. Carga los datos .sav
datos_spss <- read_sav(file = "./data/SARCOIMAGE1&2_090724 (1).sav")

# 2. Determina número de individuos y número de variables
# 3. Necesitamos tener su código de paciente si o si. Determina 
# de la forma que sepas la cantidad de pacientes que tienen código ausente
# rbase
sum(is.na(datos_spss$Código))

# tidyverse
datos_spss |> 
  count(is.na(Código))

datos_spss |> 
  summarise("n_ausentes" = sum(is.na(Código)))
datos_spss |> 
  drop_na(Código)

datos_spss |> 
  filter(is.na(Código))

# [Todo el 4 hazlo en un solo flujo de trabajo guardando la tabla solo una vez]
# 4a. Selecciona solo las variables 
#   - Código, Edad, Peso, Talla, IMC, Velocidadmarcha1, Fumador, OBE_IMC
#   - Charlson, SMM_ESPEN, Four_groups, Sarc_F, Fat_Mass_Index
#   - variables relacionadas con sentarse/levantarse (SitStand)
#   - variables relacionadas con sarcopenia (SARCO)
# 4b. Instala el paquete janitor y aplica janitor::clean_names() a esa tabla
#    ¿Qué ha hecho?
# 4c. Renombra todas las variables seleccionadas del primer item - a inglés
#     para tenerlo todo estandarizado
# 4d. La variable Four_groups contiene los 4 grupos de sarco:
#     normal / obeso / sarcopénico / obeso-sarcopénico. Quédate solo con los pacientes
#     de los que conocemos dicha clasificación.
datos4 <- 
  datos_spss |> 
  select(Código, Edad, Peso, Talla, IMC, Velocidadmarcha1, Fumador, OBE_IMC,
         Charlson, SMM_ESPEN, Four_groups, Sarc_F, Fat_Mass_Index,
         contains("SitStand"), contains("SARCO")) |> 
  janitor::clean_names() |> 
  rename(code = codigo, age = edad, mass = peso, height = talla,
         bmi = imc, walk = velocidadmarcha1, smoker = fumador, obe_bmi = obe_imc) |> 
  drop_na(four_groups)
write_csv(datos4, file = "./datos_ejercicio4.csv")



# 5. ¿Qué % de pacientes desconocemos su edad? ¿Y de cuántos desconocemos
#    la puntuación SARC-F del cuestionario de cribado?
#    Tras comprobarlo elimina todos los pacientes que tengan ausente en alguna de las dos.
datos4 |> 
  count(is.na(age)) |> 
  mutate("porc" = 100*n/sum(n))

datos4 |> 
  summarise("porc_age" = 100*mean(is.na(age)),
            "porc_sarcf" = 100*mean(is.na(sarc_f)))
datos5 <- 
  datos4 |> 
  drop_na(age, sarc_f)



# 6. La variable Fat_Mass_Index representa algo parecido al BMI solo que con los 
#   kg de grasa (kg de grasa / estatura^2). Con ella crea una nueva variable en el
#   dataset que sean los kilos de grasa y otra que represente su % respecto a su peso.
datos6 <-
  datos5 |> # Fat_Mass_Index = (kg de grasa / estatura^2) 
  # kg de grasa = Fat_Mass_Index* estatura^2
  mutate("kg_fat" = fat_mass_index * (height^2),
         "porc_fat" = 100*kg_fat/mass) |> 
  mutate("obe_38fat" = porc_fat > 38,
         "obe_40_9fat" = porc_fat > 40.9)

# 7. En la tabla original existían las variables OBE_38FAT y OBE_40.9FAT que
#   deberían almacenar un 1/0 en función de si la persona tiene más de un 38%
#   de grasa o un 40.9%, respectivamente. Recategoriza tu la variable manualmente
#   creando obe_38fat y obe_40fat con el % de grasa corporal creado en 6.

# 8. Sabiendo que para recategorizar OBE_BMI se ha usado algún corte en BMI,
#   haz uso de count() para "adivinar" que umbral se ha elegido.
datos6 |> 
  count(obe_bmi, bmi > 30)

# 9. SitStand → valor numérico en segundos (tiempo en completar prueba, menos tiempo = mejor)
#    SitStand_DICO → dicotomizado con un umbral general
#    SitStand_ESPEN_DICO → dicotomizado con el umbral específico de ESPEN
# La diferencia entre SitStand_DICO y SitStand_ESPEN_DICO es simplemente que 
# usan umbrales distintos para clasificar, ¿cómo podríamos comprobar en qué valores 
# de SitStand empiezan a divergir los dos? Detecta los umbrales usados en ambos
datos6 |> 
  filter(sit_stand_dico == 1 & sit_stand_espen_dico == 0) |> 
  slice_min(sit_stand)
  


# 10. Analicemos la relación entre Fat_Mass_Index y BMI:
#   10a. Si tuviéses que cuantificar su relación/asociación con un solo número,
#       ¿qué usarías? Incluye eso en lo que estés pesando en una tabla
#       resumen junto con la media yb la mediana de cada una de las variables
#   10b. Repite el proceso pero desagregado por categoría de Four groups
#   10c. Volviendo al caso sin desagregar, ¿cómo saber si esa correlación es suficientemente
#       distinta de 0 para decir que existe una asociación (estadísticamente 
#       significativa) entre ambas? Incluye lo que te permite concluirlo en la tabla
#       resumen del 9a.
#   10d. Exporta el resumen obtenido en 9c. a un .csv

# 11. Analicemos la relación entre fumador y la clasificación en 4 grupos
#   11a. Recodifica la variable fumador (y tras ello retira los NA en la variable)
#       para tener algo binario
#     - 0 si nunca fumó (0) o si tiene NA pero fumó menos de 10 cigarros de media al día
#     - 1 si fuma (1) o si tiene NA pero fumó igual o más de 10 cigarros de media al día
#     - si es ex-fumador (2):
#       - 1 si lo hizo más de 20 años o más de 15 cigarros diarios.
#       - 0 en caso contrario.
#     - NA si tiene NA en Fumador y NA en cigarros de media al día
#   11b. ¿Qué usarías para cuantificar la relación entre fumar/no fumar
#       y la clasificación en 4 grupos?

# 12. La variable Sarc_F sale de un cribado hecho mediante cuestionario
# (paciente reporta sus dificultades de manera subjetiva).
# Puede dar falsos negativos si el paciente no percibe su limitación o la minimiza.
# Sin embargo SARCO_PROB/CONFI/SEVERE → diagnóstico objetivo basado en mediciones reales
# (masa muscular, fuerza, función física, etc)
#   12a. Recategoriza Sarc_F tal que >= 4 --> 1 y 0 en otro caso
#   12b. ¿Existe relación entre el cribado y SARCO_PROB?
#   12c. Si definimos como SARCO_PROB = 1 verdaderos positivos, crea
#        una tabla resumen que incluya:
#         - pvalor del 11b
#         - accuracy: del total de todas, qué % está bien clasificado por Sarc_F?)
#         - sensibilidad: del total de verdaderos positivos, qué % está bien clasificado?
#         - especificidad: del total de verdaderos negativos, ¿qué % está bien clasificado?

