# ============================================================
# 02_1 Base-line inicial results.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

############ Prep data_frame ###############

DF_Inicial <- readRDS(here("data","processed","DF_pacientes_inicial.RDS"))
metadata_dict_inicial <- read_csv2(here("data", "metadata_dict_inicial.csv"))

TB_Descrip <- apply_all_transformations_inicial(DF_Inicial, metadata_dict_inicial) # transformar

TB_Descrip<- set_names_to_df(TB_Descrip, metadata_dict_inicial) # poner etiquetas

saveRDS(TB_Descrip, here("data","Final","TB_Descrip.RDS"))

### Descriptives

### Total población ----------------------------------------------------------

### Descriptiva funcional y clínica 
names(TB_Descrip)
baseline_inicial <- descrTable(
  ~ .,
  data = TB_Descrip[,c(2:39)],
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
  Home_based_PHC_org ~ . - ID,
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
