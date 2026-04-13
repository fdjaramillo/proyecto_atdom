# 1
n y % para las enfermedades 0/1


# 2
Rename USUA_UAB_UP: 00460->Borrell, 00462:Casanova; 01004:Lluch; 00474: Montnegre_1; 00475:Montnegre_2; 00478 :Sant_Elies, 00477: Marc_Aureli

Recode, Organit_Atdom_1. 
    Equip_Atdom: Borrell, Lluch;
    Equip Inf: Casanova.
    UAB_consulta: Montnegre_1, Montnegre_2, Sant_Elies , Marc_Aureli

Recode, Organit_Atdom_2. 
Equip_Atdom: Borrell, Lluch;
Equip Inf: Casanova.
UAB_consulta: Montnegre_1, Montnegre_2, 
UAB_consulta_reforç: Sant_Elies , Marc_Aureli

Generar la tablas de descritpivos USUA_UAB_UP~. ; Organit_Atdom_1.~. ; Organit_Atdom_2.