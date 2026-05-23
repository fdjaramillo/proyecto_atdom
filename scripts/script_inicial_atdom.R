library(tidyverse)
library(compareGroups)
library(labelled)

# Carga de datos raw
load("data/DF_work.RData")
load("data/DF_work2.RData")

# Cargar funciones de ayuda
source("scripts/utils_functions.R")

# descriptiva enfermedades ------------------------------------------------
patologias <- get_disease_summary(DF_work_2, `Abuso de sustancias`, VIH)

# Cargar el diccionario de metadatos desde csv
metadata_dict <- read_csv2("metadata_dict.csv") |>
  # eliminar filas con todo NA
  filter(if_any(everything(), ~ !is.na(.)))

# preparar datos para descirptiva -----------------------------------------

df <- DF_work |>
  as_tibble() |>
  left_join(DF_work_2 |> select(ID, C_GMA_N_CRONIQUES, VC_VIU_SOL_VALOR, VC_ADEQ_LLAR_VALOR,
                                C_GMA_COMPLEXITAT,PR_MACA_DATA,PR_PCC_DATA),
    by = "ID"
  )

# Flujo
validate_input_data(df, metadata_dict) # valida y lanza warnings/errors
df <- apply_all_transformations(df, metadata_dict) # transformar
df <- set_names_to_df(df, metadata_dict) # poner etiquetas

# compare groups ----------------------------------------------------------

method <- c(
  DOMICILI_INF_TOT = 2,
  TOTAL_VISITS_INF = 2,
  coc_nurse = 2,
  DOMICILI_MF_TOT = 2,
  TOTAL_VISITS_MF = 2,
  coc_physician = 2,
  DOMICILI_CONJ = 2,
  TOTAL_VISITS_CONJF = 2,
  coc_conj = 2,
  SEM_num = 2,
  emergency_visits= 2,
  INGRES_num= 2
  )

# Descritiva todos los pa. Atdom

descriptiva <- descrTable(
  ~ . - ID,
  data = df,
  method = method,
  max.xlev = 25,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva, format = "html")

# Descritiva en función PHC center

descriptiva_strat_1 <- descrTable(
  USUA_UAB_UP ~ . - ID - organit_atdom_1 - organit_atdom_2,
  data = df,
  max.ylev = 7,
  max.xlev = 25,
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_1, format = "html")

# Descritiva en función Equip_Atdom, Equip_Inf	y  UAB_consulta

descriptiva_strat_2 <- descrTable(
  organit_atdom_1 ~ . - ID - USUA_UAB_UP - organit_atdom_2,
  data = df,
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_2, format = "html")

# Descritiva Equip_Atdom, Equip_Inf,  UAB_consulta y UAB_consulta_reforç

descriptiva_strat_3 <- descrTable(
  organit_atdom_2 ~ . - ID - USUA_UAB_UP - organit_atdom_1,
  data = df,
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3, format = "html")


# new ---------------------------------------------------------------------

descriptiva_strat_2_2 <- descrTable(
  organit_atdom_1 ~ SEM_num + emergency_visits + INGRES_num,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_2_2, format = "html")

descriptiva_strat_3_3 <- descrTable(
  organit_atdom_2 ~ SEM_num + emergency_visits + INGRES_num,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3_3, format = "html")


# categorizacion ----------------------------------------------------------

vars <- c("SEM_num", "emergency_visits", "INGRES_num")

df <- df |> 
  mutate(
    across(all_of(vars), cat3, .names = "{.col}_cat3"),
    across(all_of(vars), cat2, .names = "{.col}_cat2")
  )

descriptiva_strat_2_2_cat <- descrTable(
  organit_atdom_1 ~ SEM_num_cat2 + emergency_visits_cat2 + INGRES_num_cat2 +
                    SEM_num_cat3 + emergency_visits_cat3 + INGRES_num_cat3,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_2_2_cat, format = "html")

descriptiva_strat_3_3_cat <- descrTable(
  organit_atdom_2 ~ SEM_num_cat2 + emergency_visits_cat2 + INGRES_num_cat2 +
    SEM_num_cat3 + emergency_visits_cat3 + INGRES_num_cat3,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3_3_cat, format = "html")

###Balancing
library("WeightIt")
library("cobalt")



df<-df%>%
  mutate(Age=factor(ntile(df$Edat,10)),
         GMA_CODE_quantile=factor(ntile(df$GMA_CODE,10)
                                  )
                            )

df<-df|>
  left_join(DF_work_2[,c(1,11)],
            by="ID")

###Check balance
bal.tab(organit_atdom_2~ GMA_CODE_quantile+Age+sex_female+barthel_cat, 
        data = df,
        thresholds = c(m = .05))

W.out <- weightit(organit_atdom_2~ GMA_CODE_quantile+Age+sex_female+barthel_cat,
                  data = df,
                  estimand = "ATO",
                  method = "glm")

bal.tab(W.out, 
        stats = "mean.diffs",
        thresholds = c(m = .05))

summary(W.out) #print the output
names(df_balanced)

love.plot(
  W.out,
  abs = TRUE,
  threshold = 0.05
)

df_balanced <- df %>%
  mutate(w_ato = W.out$weights)

continous_outcomes <- c("INGRES_num", "SEM_num", "emergency_visits")
cat_outcomes <- c(
  "emergency_visits_cat2",
  "SEM_num_cat2",
  "INGRES_num_cat2",
  "remain_active"
)

names(df_balanced)

#### Crudo
results_all <- run_models_automatic(
  data = df_balanced,
  continuous_outcomes = continous_outcomes,
  categorical_outcomes = cat_outcomes,
  exposure = "organit_atdom_2",
  weights_var = "w_ato"
)

print(n = 21,results_all)

#### Adjusted
results_adjusted <- run_models_automatic(
  data = df_balanced,
  continuous_outcomes = continous_outcomes,
  categorical_outcomes = cat_outcomes,
  exposure = "organit_atdom_2",
  weights_var = "w_ato",
  adjust_vars = c("Edat", "sex_female","GMA_CODE")
)

print(n = 21,results_adjusted)

library(survival)
cox_ato<- coxph(
  Surv(time_to_event, remain_active) ~ organit_atdom_2,
  data = df_balanced,
  weights = w_ato,
  robust = TRUE
)



