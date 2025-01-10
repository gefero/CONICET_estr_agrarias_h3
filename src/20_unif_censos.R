library(tidyverse)
library(sf)

## Normalizo datos desocupacion rural
desocup_2001 <- read_csv('./cap5/data/raw/desocup_rural_2001.csv', skip=17) %>%
        janitor::clean_names() %>%
        slice(-c(533:536))

desocup_2001 <- desocup_2001 %>%
        mutate(desocup_rural_2001 = solo_busca_trabajo + busca_trabajo_y_estudia +  busca_trabajo_y_percibe_jubilaci_n,
               ocup_rural_2001 = solo_trabaja + trabaja_y_estudia + trabaja_y_percibe_jubilaci_n,
               inact_rural_2001 = estudiante + jubilado_a_o_pensionado_a + otra_situaci_n) %>%
        mutate(total_rural_2001 = desocup_rural_2001 + ocup_rural_2001 + inact_rural_2001) %>%
        select(c_digo, nombre_de_departamentos, desocup_rural_2001:total_rural_2001) %>%
        rename(link = c_digo,
               depto_2001 = nombre_de_departamentos)

desocup_2001 <- desocup_2001 %>%
        filter(!(link %in% c(2001:2021)))

desocup_2010 <- read_csv('./cap5/data/raw/desocup_rural_2010.csv', skip=17) %>%
        janitor::clean_names() %>%
        rename(
                link = c_digo,
                depto_2010 = nombre_de_departamento_partido,
                ocup_rural_2010 = ocupado,
               desocup_rural_2010 = desocupado,
               inact_rural_2010 = inactivo) %>%
        mutate(total_rural_2010 = ocup_rural_2010 + desocup_rural_2010 + inact_rural_2010) %>%
        slice(-c(528:530))

desocup_2010 <- desocup_2010 %>%
        filter(!(link %in% c(2001:2015)))

desocup_rural <- desocup_2010 %>%
        left_join(desocup_2001 %>% select(-depto_2001))


desocup_rural <- desocup_rural %>%
        filter(total_rural_2001 != 0 & total_rural_2010 != 0) %>%
       mutate( 
               tasa_ocup_2001 = ocup_rural_2001 / total_rural_2001,
               tasa_ocup_2010 = ocup_rural_2010 / total_rural_2010,
               tasa_desocup_2001 = desocup_rural_2001 / (ocup_rural_2001 + desocup_rural_2001),
               tasa_desocup_2010 = desocup_rural_2010 / (ocup_rural_2010 + desocup_rural_2010),
               tasa_inact_2001 = inact_rural_2001 / total_rural_2001,
               tasa_inact_2010 = inact_rural_2010 / total_rural_2010,
               dif_desocup = tasa_desocup_2010 - tasa_desocup_2001)

## Unifico datos de asalariados agropecuarios
catocup_agro_2001 <- read_csv('./cap5/data/raw/2001_cat_ocup_agro.csv', 
                              skip = 17) %>%
        janitor::clean_names() %>%
        mutate(asal_total_agro_2001 = obrero_empleado_sector_p_blico + 
                       obrero_empleado_sector_privado, trabajador_familiar_con_sueldo) %>%
        rename(link=c_digo,
               depto_2001 = nombre_de_departamentos,
               patron_agro_2001 = patr_n,
               tcp_agro_2001 = trabajador_por_cuenta_propia,
               tf_agro_2001 = trabajador_familiar_sin_sueldo
               ) %>%
        select(-c(obrero_empleado_sector_p_blico,obrero_empleado_sector_privado, 
                  trabajador_familiar_con_sueldo)) %>%
        mutate(total_agro_2001 = patron_agro_2001+tcp_agro_2001+
                       tf_agro_2001+asal_total_agro_2001) %>%
        slice(-c(533:536))
        
catocup_direct_agro_2001 <- read_csv('./cap5/data/raw/2001_cat_ocup_direc_agro.csv',
                                     skip=17) %>%
        janitor::clean_names() %>%
        mutate(asalariado_direct_agro_2001 = obrero_empleado_sector_p_blico + 
                       obrero_empleado_sector_privado, trabajador_familiar_con_sueldo) %>%
        rename(link=c_digo,
               depto_2001 = nombre_de_departamentos) %>%
        select(link, depto_2001, asalariado_direct_agro_2001) %>%
        slice(-c(533:536))

catocup_agro_2001 <- catocup_agro_2001 %>%
        left_join(catocup_direct_agro_2001 %>% select(-depto_2001)) %>%
        mutate(asal_nodirect_2001_agro = asal_total_agro_2001 - asalariado_direct_agro_2001) %>%
        filter(!(link %in% c(2001:2021)))

catocup_agro_2010 <- read_csv('./cap5/data/raw/2010_cat_ocup_agro.csv', skip = 17) %>%
        janitor::clean_names() %>%
        rename(link=c_digo,
               depto_2010 = departamento_partido,
               patron_agro_2010 = patr_n,
               tcp_agro_2010 = trabajador_por_cuenta_propia,
               tf_agro_2010 = trabajador_familiar,
               asalariado_total_agro_2010 = obrero_o_empleado 
        ) %>%
        mutate(total_agro_2010 = patron_agro_2010+tcp_agro_2010+
                       tf_agro_2010+asalariado_total_agro_2010) %>%
        slice(-c(528:530))

catocup_direct_agro_2010 <- read_csv('./cap5/data/raw/2010_cat_ocup_direc_agro.csv',
                                     skip=17) %>%
        janitor::clean_names() %>%
        rename(link=c_digo,
               depto_2010 = departamento_partido,
               asalariado_direct_agro_2010 = obrero_o_empleado) %>%
        select(link, depto_2010, asalariado_direct_agro_2010) %>%
        slice(-c(528:530))

catocup_agro_2010 <- catocup_agro_2010 %>%
        left_join(catocup_direct_agro_2010 %>% select(-depto_2010)) %>%
        mutate(asalariado_nodirec_agro_2010 = asalariado_total_agro_2010 - asalariado_direct_agro_2010) %>%
        filter(!(link %in% c(2001:2015)))


catocup_agro_final <- catocup_agro_2010 %>%
        left_join(catocup_agro_2001 %>% select(-depto_2001))

catocup_agro_final <- catocup_agro_final %>%
        mutate(tasa_asal_tot_2001 = asal_total_agro_2001 / total_agro_2001,
               tasa_asal_nodirec_2001 = asal_nodirect_2001_agro / total_agro_2001,
               asal_total_patron_2001 = asal_total_agro_2001 / patron_agro_2001,
               asal_nodirec_patron_2001 = asal_nodirect_2001_agro / patron_agro_2001,
               tasa_asal_tot_2010 = asalariado_total_agro_2010 / total_agro_2010,
               tasa_asal_nodirec_2010 = asalariado_nodirec_agro_2010 / total_agro_2010,
               asal_total_patron_2010 = asalariado_total_agro_2010 / patron_agro_2010,
               asal_nodirec_patron_2010 = asalariado_nodirec_agro_2010 / patron_agro_2010               
               ) %>%
        mutate(diff_tasa_asal_tot = tasa_asal_tot_2010 - tasa_asal_tot_2010,
               diff_tasa_asalnodirec_tot = tasa_asal_nodirec_2010 - tasa_asal_nodirec_2001,
               diff_asal_total_patron = asal_total_patron_2010 - asal_total_patron_2001,
               diff_asal_nodirec_patron = asal_nodirec_patron_2010 - asal_nodirec_patron_2001
        )

ocup_agro_rural <- catocup_agro_final %>%
        left_join(desocup_rural %>% select(-depto_2010))

write_csv(ocup_agro_rural, './cap5/data/proc/CNPyV_ocup_agro_rural.csv')
