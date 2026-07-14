# ============================================================
# 07_Costs.R
# ============================================================
source(here("scripts", "00_setup.R"))


Farm<-read_rds(here("data","Tables_DB", "TB_Farm.rds"))
Der<-read_rds(here("data","Tables_DB", "TB_Der.rds"))
Visits<-read_rds(here("data","Tables_DB", "TB_pacientes.RDS"))
Analitiques<-read_rds(here("data","Tables_DB","TB_analitiques.rds"))

Visits_outcomes<-Visits%>%
  select(ID,DOMICILI_INF_TOT,CENTRE_INF_TOT,DOMICILI_MF_TOT,CENTRE_MF_TOT,INGRES_num,CUAP_num,SEM_num,ALTA_UCIES_num)%>%
  mutate(Cost_v_INF_dom=DOMICILI_INF_TOT*60,
         Cost_v_INF_cent=CENTRE_INF_TOT*45,
         Cost_v_MG_dom=DOMICILI_MF_TOT*90,
         Cost_v_MG_cent=CENTRE_MF_TOT*65,
         Cost_CUAP=CUAP_num*150)%>%
    mutate(across(c(INGRES_num, CUAP_num, SEM_num, ALTA_UCIES_num),
                   ~ coalesce(.x, 0L)))
  

Total_costs<-Visits_outcomes[,c(1,10:14)]%>%
  left_join(Farm,by="ID")%>%
  left_join(analitiques[,c(1,3)],by="ID")%>%
  left_join(Der[,c(1,9:14)],by="ID")%>%
  mutate(TOTAL = rowSums(
    across(c(2:6, 9, 16)),
    na.rm = TRUE
  ))

