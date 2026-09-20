# ============================================================
# 02_Base-line results.R
# ============================================================

source(here("scripts", "00_setup.R"))

### Prep data_frame

TB_Descrip <- readRDS(here("data","Tables_DB","TB_pacientes.RDS"))
TB_Descrip <- apply_all_transformations(TB_pacientes, metadata_dict) # transformar
TB_Descrip<- set_names_to_df(TB_Descrip, metadata_dict) # poner etiquetas

saveRDS(TB_Descrip, here("data","Final","TB_Descrip.RDS"))

TB_Descrip<-readRDS(here("data","Final","TB_Descrip.RDS"))
TB_Pacients<-readRDS(here("data","Tables_DB","TB_pacientes.RDS"))

### Descriptives

method <- c(
  Nurse_Home_Visits = 2,
  Nurse_Total_Visits = 2,
  COC_Nurse = 2,
  GP_Home_Visits = 2,
  GP_Total_Visits = 2,
  COC_GP = 2,
  Tota_GP_Nurse_visits = 2,
  GP_Nurse_home_Visits = 2,
  COC_Total = 2,
  Home_Ambulances = 2,
  PC_Emergency_unit = 2,
  ED_visits= 2,
  Hospital_Admissions= 2
)

### Total población ----------------------------------------------------------

### Descriptiva funcional y clínica 

baseline <- descrTable(
  ~ . - ID,
  data = TB_Descrip[,c(1:18)],
  method = method,
  max.xlev = 25,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)

export2md(baseline, format = "html")

export2html(
  baseline,
  file = here("Output", "Tables", "baseline.html")
)

export2xls(
  baseline,
  file = here("Output", "Tables", "baseline.xlsx")
)

### Patologias

Diseases<-readRDS(here("data","Tables_DB","TB_pacientes.RDS"))%>%
  select(`Abuso de sustancias`:VIH)

Diseases <- get_disease_summary(Diseases, `Abuso de sustancias`, VIH)

write_xlsx(
  Diseases,
  here("Output", "Tables", "table1_baseline_patologia.xlsx")
)

# Descriptiva Equip_Atdom, Equip_Inf, UAB_consulta y UAB_consulta_reforç
Table_2_by_org <- descrTable(
  Home_based_PHC_org ~ . - ID,
  data = TB_Descrip[,c(1:19,21,22,24,33)],
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)

export2md(Table_2_by_org, format = "html")

export2html(
  Table_2_by_org,
  file = here("Output", "Tables", "Table_2_by_org.html")
)

export2xls(
  Table_2_by_org,
  file = here("Output", "Tables", "Table_2_by_org.xlsx")
)

# Outcomes ---------------------------------------------------------------------

Table_3_outcomes <- descrTable(
  Home_based_PHC_org ~ Home_Ambulances + ED_visits + PC_Emergency_unit+Hospital_Admissions,
  data = TB_Descrip,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(Table_3_outcomes, format = "html")

export2html(
  Table_3_outcomes,
  file = here("Output", "Tables", "Outcomes_by_PHC_org.html")
)

export2xls(
  Table_3_outcomes,
  file = here("Output", "Tables", "Outcomes_by_PHC_org.xlsx")
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
