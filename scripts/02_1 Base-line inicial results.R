# ============================================================
# 02_1 Base-line inicial results.R
# ============================================================

source(here("scripts", "00_setup.R"))

############ Prep data_frame ###############

DF_Inicial <- readRDS(here("data","processed","DF_pacientes_inicial.RDS"))
TB_Descrip <- apply_all_transformations_inicial(DF_Inicial, metadata_dict_inicial) # transformar

TB_Descrip<- set_names_to_df(TB_Descrip, metadata_dict_inicial) # poner etiquetas

saveRDS(TB_Descrip, here("data","Final","TB_Descrip.RDS"))

TB_Descrip<-readRDS(here("data","Final","TB_Descrip.RDS"))
TB_Pacients<-readRDS(here("data","Tables_DB","TB_pacientes.RDS"))

### Descriptives

### Total población ----------------------------------------------------------

### Descriptiva funcional y clínica 

baseline_inicial <- descrTable(
  ~ .,
  data = TB_Descrip[,c(2:40)],
  method = 2,
  max.xlev = 25,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)

export2md(baseline_inicial, format = "html")

export2html(
  baseline,
  file = here("Output", "Tables", "baseline_inicial.html")
)

export2xls(
  baseline,
  file = here("Output", "Tables", "baseline_inicial.xlsx")
)

# Descriptiva Equip_Atdom, Equip_Inf, UAB_consulta y UAB_consulta_reforç
Table_2_by_org_inicial <- descrTable(
  Home_based_PHC_org ~ . - USUA_CIP,
  data = TB_Descrip[,c(1:19)],
  show.all = T,
  chisq.test.perm = T,
  method = 4,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)

export2md(Table_2_by_org_inicial, format = "html")

export2html(
  Table_2_by_org_inicial,
  file = here("Output", "Tables", "Table_2_by_org_inicial.html")
)

export2xls(
  Table_2_by_org_inicial,
  file = here("Output", "Tables", "Table_2_by_org_inicial.xlsx")
)

# categorizacion ----------------------------------------------------------
#AQUI

vars <- c("SEM_num", "ED_visits", "PC_Emergency_unit", "Hospital_Admissions","Exitus")

df <- df |> 
  mutate(
    across(all_of(vars), ~ cat3(as.numeric(as.character(.x))), .names = "{.col}_cat3"),
    across(all_of(vars), ~ cat2(as.numeric(as.character(.x))), .names = "{.col}_cat2")
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
  organit_atdom_2 ~ SEM_num+SEM_num_cat2 + emergency_visits+emergency_visits_cat2 + INGRES_num+INGRES_num_cat2
  +Exitus,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3_3_cat, format = "html")
