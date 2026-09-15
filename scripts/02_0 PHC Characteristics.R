# ============================================================
# 02_0 PHC Characteristics.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

############ Prep data_frame ###############

# Data

Central_res<- read.xlsx(here("data","external","DADES_CENTRAL_RESULTATS_2024.xlsx"),
                        sheetName="Sheet1")

Central_res<- Central_res %>%
  mutate(UP_ABS = sprintf("%05d", UP_ABS))


### Descriptives

### Total población 

### Descriptiva funcional y clínica 

baseline_inicial <- descrTable(
  ~ .,
  data = TB_Descrip,
  method = 2,
  max.xlev = 25,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)

export2md(baseline_inicial, format = "html")

export2html(
  baseline_inicial,
  file = here("Output", "Tables", "baseline_inicial.html")
)

export2xls(
  baseline_inicial,
  file = here("Output", "Tables", "baseline_inicial.xlsx")
)

# Descriptiva Equip_Atdom, Equip_Inf, UAB_consulta y UAB_consulta_reforç
Table_2_by_org_inicial <- descrTable(
  Home_based_PHC_org ~ . - ID,
  data = TB_Descrip,
  show.all = F,
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
