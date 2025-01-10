library(tidyverse)
library(sf)

pluriact <- read_csv('../CONICET_tablas_CNA/data/proc/pluriact_2018.csv') %>%
        filter(unidad=="EAP") %>%
        mutate(link = case_when(
                depto == "Chascomús" ~ "06217",
                TRUE ~ link)
        ) %>%
        rename_with(~str_c("plur_", ., "_2018"), c(total:sd))

est_soc <- read_sf('../CONICET_estr_agr/proc_data/tablas_finales/frontera_depto.geojson')

est_soc <- est_soc %>%
        filter(depto_cna18!="1 de mayo")

est_soc <- est_soc %>%
        mutate(toteap_18 = case_when(
                provincia_cna18=="santa cruz" ~ toteap_18/2,
                TRUE ~ toteap_18)) 


ocup <- read_csv('./cap5/data/proc/CNPyV_ocup_agro_rural.csv') %>%
        mutate(
                link = as.character(link)
        ) %>%
        mutate(link = case_when(
                nchar(link) < 5 ~ paste0("0", link),
                TRUE ~ link
        ))

est_soc <- est_soc %>%
        left_join(
                ocup 
                ) %>%
        left_join(pluriact)

est_soc <- est_soc %>%
        mutate(prop_pluriact_total = case_when(
                plur_total_2018/toteap_18 > 1 ~ 1,
                plur_total_2018/toteap_18 <= 1 ~ plur_total_2018/toteap_18),
               prop_pluriact_asal_agro_todo = plur_agro_asal_todo_2018 / plur_total_2018,
               prop_pluriact_asal_agro_parte = plur_agro_asal_parte_2018 / plur_total_2018,
               prop_pluriact_asal_noagro_todo = plur_no_agro_asal_todo_2018 / plur_total_2018,
               prop_pluriact_asal_noagro_parte = plur_no_agro_asal_parte_2018 / plur_total_2018,
               prop_pluriact_asal_todo = (plur_no_agro_asal_todo_2018 + plur_agro_asal_todo_2018) / plur_total_2018,
               prop_pluriact_asal = (plur_no_agro_asal_todo_2018 + plur_agro_asal_todo_2018 +
                                             plur_no_agro_asal_parte_2018 + plur_agro_asal_parte_2018) / plur_total_2018,
               prop_pluriact_noasal = (plur_agro_tcp_2018 + plur_agro_patron_2018 + plur_no_agro_tcp_2018 +plur_no_agro_patron_2018) / plur_total_2018
               )




est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(#desoc_2001 = sum(desocup_rural_2001, na.rm=TRUE) / (sum(desocup_rural_2001, na.rm=TRUE) + sum(ocup_rural_2001, na.rm=TRUE)),
                  #desoc_2010 = sum(desocup_rural_2010, na.rm=TRUE) / (sum(desocup_rural_2010, na.rm=TRUE) + sum(ocup_rural_2010, na.rm=TRUE)),
                  median_desoc2001 = round(100*spatstat.geom::weighted.median(tasa_desocup_2001, w=total_rural_2001, na.rm = TRUE), 2),
                  median_desoc2010 = round(100*spatstat.geom::weighted.median(tasa_desocup_2010, w=total_rural_2010, na.rm = TRUE), 2),
        ) %>%
        bind_rows(
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(#desoc_2001 = sum(desocup_rural_2001, na.rm=TRUE) / (sum(desocup_rural_2001, na.rm=TRUE) + sum(ocup_rural_2001, na.rm=TRUE)),
                                #desoc_2010 = sum(desocup_rural_2010, na.rm=TRUE) / (sum(desocup_rural_2010, na.rm=TRUE) + sum(ocup_rural_2010, na.rm=TRUE)),
                                median_desoc2001 = round(100*spatstat.geom::weighted.median(tasa_desocup_2001, w=total_rural_2001, na.rm = TRUE), 2),
                                median_desoc2010 = round(100*spatstat.geom::weighted.median(tasa_desocup_2010, w=total_rural_2010, na.rm = TRUE), 2),
                        ) %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())
        ) %>%
        drop_na() %>%
        openxlsx::write.xlsx('./cap5/plots/tabla52.xlsx')

est_soc %>%
        ggplot() + 
        geom_boxplot(aes(x=cluster2, y=dif_desocup))

est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(#inact_2001 = sum(inact_rural_2001, na.rm=TRUE) / sum(total_rural_2001, na.rm = TRUE),
                  #inact_2010 = sum(inact_rural_2010, na.rm=TRUE) / sum(total_rural_2010, na.rm = TRUE),
                  median_inact_2001 = round(100*spatstat.geom::weighted.median(tasa_inact_2001, w=total_rural_2001, na.rm = TRUE),2),
                  median_inact_2010 = round(100*spatstat.geom::weighted.median(tasa_inact_2010, w=total_rural_2010, na.rm = TRUE),2)
        ) %>%
        bind_rows(
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(#inact_2001 = sum(inact_rural_2001, na.rm=TRUE) / sum(total_rural_2001, na.rm = TRUE),
                                #inact_2010 = sum(inact_rural_2010, na.rm=TRUE) / sum(total_rural_2010, na.rm = TRUE),
                                median_inact_2001 = round(100*spatstat.geom::weighted.median(tasa_inact_2001, w=total_rural_2001, na.rm = TRUE),2),
                                median_inact_2010 = round(100*spatstat.geom::weighted.median(tasa_inact_2010, w=total_rural_2010, na.rm = TRUE),2)
                        )  %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())     
        ) %>%
        drop_na() %>%
        openxlsx::write.xlsx('./cap5/plots/tabla5z.xlsx')

est_soc %>%
        ggplot() + 
        geom_boxplot(aes(x=cluster2, y=dif_desocup))


est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(#asal_2001 = sum(asal_nodirect_2001_agro, na.rm=TRUE) / sum(total_agro_2001, na.rm=TRUE),
                  #asal_2010 = sum(asalariado_nodirec_agro_2010 , na.rm=TRUE) / sum(total_agro_2010, na.rm=TRUE),
                  median_asal_2001 = round(100*spatstat.geom::weighted.median(tasa_asal_nodirec_2001, w=total_agro_2001, na.rm = TRUE),2),
                  median_asal_2010 = round(100*spatstat.geom::weighted.median(tasa_asal_nodirec_2010, w=total_agro_2010, na.rm = TRUE),2)
        ) %>%
        drop_na() %>%
        bind_rows(
                
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(#inact_2001 = sum(inact_rural_2001, na.rm=TRUE) / sum(total_rural_2001, na.rm = TRUE),
                                #inact_2010 = sum(inact_rural_2010, na.rm=TRUE) / sum(total_rural_2010, na.rm = TRUE),
                                median_asal_2001 = round(100*spatstat.geom::weighted.median(tasa_asal_nodirec_2001, w=total_agro_2001, na.rm = TRUE),2),
                                median_asal_2010 = round(100*spatstat.geom::weighted.median(tasa_asal_nodirec_2010, w=total_agro_2010, na.rm = TRUE),2)
                        )  %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())        
                
        ) %>%
        openxlsx::write.xlsx('./cap5/plots/tabla51.xlsx')

est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(#asal_2001 = sum(asal_total_agro_2001, na.rm=TRUE) / sum(total_agro_2001, na.rm=TRUE),
                  #asal_2010 = sum(asalariado_total_agro_2010 , na.rm=TRUE) / sum(total_agro_2010, na.rm=TRUE),
                  median_asal_2001 = round(100*spatstat.geom::weighted.median(tasa_asal_tot_2001, w=total_agro_2001, na.rm = TRUE),2),
                  median_asal_2010 = round(100*spatstat.geom::weighted.median(tasa_asal_tot_2010, w=total_agro_2010, na.rm = TRUE),2),
        ) %>%
        drop_na() %>%
        bind_rows(
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(#asal_2001 = sum(asal_total_agro_2001, na.rm=TRUE) / sum(total_agro_2001, na.rm=TRUE),
                                #asal_2010 = sum(asalariado_total_agro_2010 , na.rm=TRUE) / sum(total_agro_2010, na.rm=TRUE),
                                median_asal_2001 = round(100*spatstat.geom::weighted.median(tasa_asal_tot_2001, w=total_agro_2001, na.rm = TRUE),2),
                                median_asal_2010 = round(100*spatstat.geom::weighted.median(tasa_asal_tot_2010, w=total_agro_2010, na.rm = TRUE),2)
                                ) %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())
        ) %>%
        openxlsx::write.xlsx('./cap5/plots/tabla5zz.xlsx')


est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(
                tasa_migr_2010_2001 = round(spatstat.geom::weighted.median(tasa_migr_2010_2001, w=total_agro_2001, na.rm = TRUE),2),
                tasa_migr_2018_2011 = round(spatstat.geom::weighted.median(tasa_migr_2018_2011, w=total_agro_2010, na.rm = TRUE),2),
                tasa_migr_2018_2001 = round(spatstat.geom::weighted.median(tasa_migr_2018_2001, w=total_agro_2001, na.rm = TRUE),2)
        ) %>%
        drop_na() %>%
        bind_rows(
                
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(
                                tasa_migr_2010_2001 = round(spatstat.geom::weighted.median(tasa_migr_2010_2001, w=total_agro_2001, na.rm = TRUE),2),
                                tasa_migr_2018_2011 = round(spatstat.geom::weighted.median(tasa_migr_2018_2011, w=total_agro_2010, na.rm = TRUE),2),
                                tasa_migr_2018_2001 = round(spatstat.geom::weighted.median(tasa_migr_2018_2001, w=total_agro_2001, na.rm = TRUE),2)
                        ) %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())
                
        ) %>%
        openxlsx::write.xlsx('./cap5/plots/tabla53.xlsx')


est_soc %>%
        ggplot() + 
                geom_boxplot(aes(x=cluster2, y=tasa_migr_2018_2001, weight=total_agro_2001))


est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(provincia_cna18) %>%
        summarise(
                #eaps_1988 = sum(toteap_88, na.rm=TRUE),
                eaps_2002 = sum(toteap_02,na.rm=TRUE),
                eaps_2018 = sum(toteap_18),
                #sup_1988 = sum(totsup_88, na.rm=TRUE),
                sup_2002 = sum(totsup_02,na.rm=TRUE),
                sup_2018 = sum(totsup_18)
        ) %>% print(n=100)



est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(
                #eaps_1988 = sum(toteap_88, na.rm=TRUE),
                eaps_2002 = sum(toteap_02,na.rm=TRUE),
                eaps_2018 = sum(toteap_18),
                #sup_1988 = sum(totsup_88, na.rm=TRUE),
                sup_2002 = sum(totsup_02,na.rm=TRUE),
                sup_2018 = sum(totsup_18)
                ) %>%
        drop_na() %>%
        bind_rows(
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(
                                #eaps_1988 = sum(toteap_88, na.rm=TRUE),
                                eaps_2002 = sum(toteap_02, na.rm=TRUE),
                                eaps_2018 = sum(toteap_18),
                                #sup_1988 = sum(totsup_88, na.rm=TRUE),
                                sup_2002 = sum(totsup_02, na.rm=TRUE),
                                sup_2018 = sum(totsup_18)
                        ) %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())
                
                
        ) %>%
        mutate(#sup_mean_1988 = sup_1988/eaps_1988,
               sup_mean_2002 = sup_2002/eaps_2002,
               sup_mean_2018 = sup_2018/eaps_2018) %>%
        mutate(diff_eaps = eaps_2018/eaps_2002 - 1,
               diff_supmedia = sup_mean_2018/sup_mean_2002 - 1
               ) %>%
        openxlsx::write.xlsx('./cap5/plots/tabla55.xlsx')


est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(
                def_1987_2002 = sum(def_1987_2002, na.rm=TRUE),
                def_2003_2019 = sum(def_2003_2019, na.rm=TRUE)
                ) %>%
        mutate(prop_def_1987_2002 = def_1987_2002/sum(def_1987_2002),
               prop_def_2003_2019 = def_2003_2019/sum(def_2003_2019)) %>%
        drop_na() %>%
        bind_rows(
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(
                                def_1987_2002 = sum(def_1987_2002, na.rm=TRUE),
                                def_2003_2019 = sum(def_2003_2019, na.rm=TRUE)
                        ) %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())
        ) %>%
        openxlsx::write.xlsx('./cap5/plots/tabla56.xlsx')






est_soc %>%
        st_set_geometry(NULL) %>%
        group_by(cluster2) %>%
        summarise(
                total_pluriact = sum(plur_total_2018, na.rm=TRUE),
                median_pluract_2018 = round(100*spatstat.geom::weighted.median(prop_pluriact_total, w=toteap_18, na.rm = TRUE), 2),
                median_pluriact_asal_agro_parte = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_agro_parte, w=toteap_18, na.rm = TRUE), 2),
                median_pluriact_asal_agro_todo = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_agro_todo, w=toteap_18, na.rm = TRUE), 2),
                median_pluriact_asal_noagro_parte = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_noagro_parte, w=toteap_18, na.rm = TRUE), 2),
                median_pluriact_asal_noagro_todo = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_noagro_todo, w=toteap_18, na.rm = TRUE), 2),
                median_pluriact_asal_todo = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_todo, w=toteap_18, na.rm = TRUE), 2),
                median_pluriact_asal = round(100*spatstat.geom::weighted.median(prop_pluriact_asal, w=toteap_18, na.rm = TRUE), 2),
                median_pluriact_noasal = round(100*spatstat.geom::weighted.median(prop_pluriact_noasal, w=toteap_18, na.rm = TRUE), 2),
        ) %>%
        bind_rows(
                est_soc %>%
                        st_set_geometry(NULL) %>%
                        summarise(#desoc_2001 = sum(desocup_rural_2001, na.rm=TRUE) / (sum(desocup_rural_2001, na.rm=TRUE) + sum(ocup_rural_2001, na.rm=TRUE)),
                                #desoc_2010 = sum(desocup_rural_2010, na.rm=TRUE) / (sum(desocup_rural_2010, na.rm=TRUE) + sum(ocup_rural_2010, na.rm=TRUE)),
                                total_pluriact = sum(plur_total_2018, na.rm=TRUE),
                                median_pluract_2018 = round(100*spatstat.geom::weighted.median(prop_pluriact_total, w=toteap_18, na.rm = TRUE), 2),
                                median_pluriact_asal_agro_parte = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_agro_parte, w=toteap_18, na.rm = TRUE), 2),
                                median_pluriact_asal_agro_todo = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_agro_todo, w=toteap_18, na.rm = TRUE), 2),
                                median_pluriact_asal_noagro_parte = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_noagro_parte, w=toteap_18, na.rm = TRUE), 2),
                                median_pluriact_asal_noagro_todo = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_noagro_todo, w=toteap_18, na.rm = TRUE), 2),
                                median_pluriact_asal_todo = round(100*spatstat.geom::weighted.median(prop_pluriact_asal_todo, w=toteap_18, na.rm = TRUE), 2),
                                median_pluriact_asal = round(100*spatstat.geom::weighted.median(prop_pluriact_asal, w=toteap_18, na.rm = TRUE), 2),
                                median_pluriact_noasal = round(100*spatstat.geom::weighted.median(prop_pluriact_noasal, w=toteap_18, na.rm = TRUE), 2)
                        ) %>%
                        mutate(cluster2="Total") %>%
                        select(cluster2, everything())
        ) %>%
        drop_na() %>%
        openxlsx::write.xlsx('./cap5/plots/tabla56.xlsx')

