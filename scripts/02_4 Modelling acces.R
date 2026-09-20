# ============================================================
# 02_4 Modelling access.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

#Data load

Data_population_UC <- readRDS(
  here("data", "SF", "unitats_censals_estudi_sf.rds"))%>%
  st_drop_geometry()

Data_centres_UC <- readRDS(
  here("data", "SF", "Centres_estudi_SF.rds"))%>%
  st_drop_geometry()

pob_denominadores <- readRDS(
  here("data", "processed", "tasas_cobertura_atdom_long.rds"))%>%
  st_drop_geometry()%>%
  rename(tasa_cruda_atdom_1000=tasa_atdom_1000)

atdom_numeradores <- readRDS(
  here("data", "processed", "atdom_numeradores.rds"))

Data_SES_UC <- readRDS(
  here("data", "processed", "Data_SES_UC.rds"))%>%
  st_drop_geometry()

pesos_edad_sexo<-readRDS(here("data","processed","pesos_edad_sexo.rds"))

#Merging
names(Data_SES_UC)
Data_model_UC <- Data_population_UC[,c(7:10)]%>%
  left_join(Data_SES_UC, by = "Seccio_Censal") %>%
  left_join(atdom_numeradores, by = "Seccio_Censal")%>%
  left_join(pob_denominadores[,c(1:4,6)], by = c("Seccio_Censal","edat_cat", "sexo"))%>%
  select(
    Seccio_Censal,
    CODABSa,
    NOMABS,
    Home_based_PHC_org,
    Index_SES,
    Renta_media_hogar,
    edat_cat,
    sexo,
    n_atdom,
    poblacion_total
  ) 

Data_model_UC <- Data_model_UC %>%
  mutate(
    Home_based_PHC_org = factor(Home_based_PHC_org),
    edat_cat = factor(edat_cat),
    sexo = factor(sexo),
    NOMABS = factor(NOMABS),
    Index_SES_z = as.numeric(scale(Index_SES)),
    Renta_10k = Renta_media_hogar / 10
  )%>%
  mutate(Home_based_PHC_org=relevel(Home_based_PHC_org,ref="UAB_consulta"))


#Modeling

#Poisson con errores robustos (las U-censales dependen del centro).

# M0: organización
m0 <- glm(
  n_atdom ~ Home_based_PHC_org +
    offset(log(poblacion_total)),
  family = poisson(link = "log"),
  data = Data_model_UC
)

# M1: organización + edad + sexo
m1 <- glm(
  n_atdom ~ Home_based_PHC_org +
    edat_cat +
    sexo +
    offset(log(poblacion_total)),
  family = poisson(link = "log"),
  data = Data_model_UC
)

# M2: organización + edad + sexo + SES
m2 <- glm(
  n_atdom ~ Home_based_PHC_org +
    edat_cat +
    sexo +
    Index_SES_z  +
    offset(log(poblacion_total)),
  family = poisson(link = "log"),
  data = Data_model_UC
)

names(Data_model_UC)

##Resultados

res_m0 <- get_robust_results(m0, Data_model_UC)
res_m1 <- get_robust_results(m1, Data_model_UC)
res_m2 <- get_robust_results(m2, Data_model_UC)

##Tasas ajustadas por 1000hab

V_m2 <- vcovCL(
  m2,
  cluster = Data_model_UC$Seccio_Censal,
  type = "HC1"
)

emm_rates_robust <- emmeans(
  m2,
  ~ Home_based_PHC_org,
  type = "response",
  offset = log(1000),
  vcov. = V_m2
)

emm_rates_robust

rates_adj_robust <- as.data.frame(emm_rates_robust) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2)))%>%
      select(-df,-SE)%>%
      rename(CI_Lower=asymp.LCL,
             CI_Upper=asymp.UCL,
             access_rate_1000hab=rate)


names(rates_adj_robust)

#análisis de sensibilidad por centro:

m_abs <- glm(
  n_atdom ~ NOMABS +
    edat_cat +
    sexo +
    Index_SES_z +
    offset(log(poblacion_total)),
  family = poisson(link = "log"),
  data = Data_model_UC
)

res_m_abs <- get_robust_results(m_abs, Data_model_UC)

##Tabla final
tab_models <- bind_rows(
  prep_model(res_m0, "M0: Crude"),
  prep_model(res_m1, "M1: Age + sex"),
  prep_model(res_m2, "M2: Age + sex + SES")
) %>%
  pivot_wider(
    names_from = Model,
    values_from = estimate
  )

tab_models <- bind_rows(
  tibble(
    Home_based_PHC_org = "UAB consulta",
    `M0: Crude` = "1.00 (Ref.)",
    `M1: Age + sex` = "1.00 (Ref.)",
    `M2: Age + sex + SES` = "1.00 (Ref.)"
  ),
  tab_models
)

tab_rates <- rates_adj_robust %>%
  transmute(
    Home_based_PHC_org = case_when(
      Home_based_PHC_org == "UAB_consulta" ~ "UAB consulta",
      Home_based_PHC_org == "Equip_Atdom" ~ "Equip ATDOM",
      Home_based_PHC_org == "Equip_Inf" ~ "Equip Inf",
      Home_based_PHC_org == "UAB_consulta_reforc" ~ "UAB consulta reforç"
    ),
    `Adjusted rate per 1,000` = sprintf(
      "%.1f (%.1f–%.1f)",
      access_rate_1000hab,
      CI_Lower,
      CI_Upper
    )
  )

Table_access <- tab_models %>%
  left_join(
    tab_rates,
    by = "Home_based_PHC_org"
  )

Table_access

write.xlsx(Table_access,here("Output","Tables","Access_model.xlsx"))
