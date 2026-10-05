########################################################################
######                                                            ######
#                      UNIVERSIDAD DEL QUINDÍO                         #
#                       PROGRAMA DE ECONOMÍA                           #
#                           ECONOMETRÍA II                             #
#             ACTIVIDAD 2 - ESTIMACIÓN SOBRE DATOS PANEL               #
######                                                            ######
########################################################################

# By: Juan José Garcés Pineda
# juanj.garcesp@uqvirtual.edu.co

options('scipen' = 100, 'digits' = 4)
rm(list = ls())

# Librerías
# install.packages(c("haven","tidyverse","lmtest","sandwich","plm","car","modelsummary","skimr"))
library('haven')
library('tidyverse')
library('lmtest')
library('sandwich')
library('plm')
library('car')
library('modelsummary')

## Base de datos
DATA_PANEL = haven::read_dta("Segundo corte/nls_panel.dta") |>
  data.frame()
DATA_PANEL[] = lapply(DATA_PANEL, as.numeric)


########################################################################
# 1. PLANTEAMIENTO DEL MODELO Y NUEVAS HIPÓTESIS
########################################################################
# MODELO (salario explicado por jornada, capital humano en el
# trabajo, afiliación sindical, estado civil, localización, educación y raza):
#
# lwage_it = b1 hours_it + b2 exper_it + b3 exper2_it + b4 tenure_it
#          + b5 tenure2_it + b6 union_it + b7 msp_it + b8 south_it
#          + b9 not_smsa_it + b10 educ_i + b11 black_i + a_i + u_it
#
# H1: b1 <= 0  Más horas semanales no elevan (o reducen) el salario por hora.
# H2: b2 > 0, b3 < 0  Experiencia con rendimientos decrecientes.
# H3: b4 > 0, b5 < 0  Antigüedad con rendimientos decrecientes.
# H4: b6 > 0   Prima sindical (MCO la sobrestima: b6_FE < b6_pooled).
# H5: b7 = 0   Estar casada con cónyuge presente no afecta el salario horario.
# H6: b8 < 0, b9 < 0  Vivir en el sur / fuera de un área metropolitana reduce el salario.
# H7: b10 > 0, b11 < 0  Más educación aumenta el salario; brecha para
#             trabajadoras negras (solo pooled y RE).
# H8: Existen efectos individuales no observados (a_i) correlacionados con
#     los regresores, que sesgan al estimador pooled.


########################################################################
# 2. DESCRIPTIVOS
########################################################################
head(DATA_PANEL)
str(DATA_PANEL)

## Estructura del panel: i = 1,...,716 (N); t = 1,...,5 (T)
table(DATA_PANEL$year, useNA = "always")
DATA_PANEL |> count(id) |> count(n)      # ¿balanceado?

vars = c("lwage", "hours", "exper", "tenure", "union", "msp", "south", "not_smsa")

## Variación ENTRE unidades (sd de las medias individuales)
ENTRE = DATA_PANEL |> group_by(id) |>
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)))
sapply(ENTRE[vars], sd, na.rm = TRUE)

## Variación DENTRO de cada unidad (sd de las desviaciones a la media individual)
DENTRO = DATA_PANEL |> group_by(id) |>
  mutate(across(all_of(vars), ~ .x - mean(.x, na.rm = TRUE)))
sapply(DENTRO[vars], sd, na.rm = TRUE)

## Estadísticos por periodo
DATA_PANEL |> group_by(year) |>
  summarise(across(all_of(vars), mean)) |> as.data.frame()


########################################################################
# 3. REGRESIONES CLÁSICAS (corte transversal por año)
########################################################################
# Años disponibles: 82, 83, 85, 87, 88 (no existen 84 ni 86)
f_cs = lwage ~ hours + exper + exper2 + tenure + tenure2 + union + msp +
  south + not_smsa + educ + black

for (a in c(82, 83, 85, 87, 88)) {
  cat("\n=== Año", a, "===\n")
  print(summary(lm(f_cs, data = DATA_PANEL, subset = (year == a))))
}


########################################################################
# 4. ESTIMACIÓN POOLED
########################################################################
pooled = lm(f_cs, data = DATA_PANEL)
summary(pooled)


########################################################################
# 5. EFECTOS FIJOS POR DIFERENCIAS  t1: 82 , t2: 88
########################################################################
DATOS_82_88 = DATA_PANEL |>
  filter(year %in% c(82, 88)) |>
  select(id, year, lwage, hours, exper2, tenure, tenure2, union, msp,
         south, not_smsa) |>
  pivot_wider(id_cols = id, names_from = year,
              values_from = c(lwage, hours, exper2, tenure, tenure2, union,
                              msp, south, not_smsa))

DATA_DIFF = DATOS_82_88 |>
  transmute(
    c_lwage    = lwage_88    - lwage_82,
    c_hours    = hours_88    - hours_82,
    c_exper2   = exper2_88   - exper2_82,
    c_tenure   = tenure_88   - tenure_82,
    c_tenure2  = tenure2_88  - tenure2_82,
    c_union    = union_88    - union_82,
    c_msp      = msp_88      - msp_82,
    c_south    = south_88    - south_82,
    c_not_smsa = not_smsa_88 - not_smsa_82
  )

# exper no entra: entre 1982 y 1988 aumenta ~6 años para casi todas,
# es decir, es casi una constante y la absorbe el intercepto.
red_diff = lm(c_lwage ~ c_hours + c_exper2 + c_tenure + c_tenure2 + c_union +
                c_msp + c_south + c_not_smsa, data = DATA_DIFF)
summary(red_diff)
coeftest(red_diff, vcov = vcovHC(red_diff, type = "HC1"))   # EE robustos


########################################################################
# 6. MODELO DE EFECTOS FIJOS AUTOMÁTICO (plm)
########################################################################
panel_de_datos = pdata.frame(DATA_PANEL, index = c("id", "year"))

f_fe = lwage ~ hours + exper + exper2 + tenure + tenure2 + union + msp +
  south + not_smsa

# Solo variables que cambian en el tiempo: educ y black las absorbe a_i
FE = plm(f_fe, data = panel_de_datos, model = "within")
summary(FE)

# ¿Cuánta variación "dentro" tienen las variables?
pvar(panel_de_datos$educ)
pvar(panel_de_datos$black)
pvar(panel_de_datos$south)
pvar(panel_de_datos$not_smsa)

## Pocas personas cambian de zona => poca variación "dentro"
DATOS_TRANS = DATA_PANEL |>
