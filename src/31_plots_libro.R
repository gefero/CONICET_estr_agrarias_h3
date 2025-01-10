library(tidyverse)
library(sf)
df_est <- read_sf('../CONICET_estr_agr/proc_data/CNA_CNPyV_estr_agrarias.geojson')

grayscale_map <- c("#3C3C3C","#6E6E6E","#C0C0C0","#DCDCDC")


df_est %>% 
        drop_na(cluster) %>%
        st_set_geometry(NULL) %>%
        group_by(cluster) %>%
        summarise(patr=median(patr),
                  pp1=median(P_PPT1EAP),
                  pba_asal=median(p_pba_ofic + p_pba_aux),
                  pp2=median(P_PPT2EAP),
                  pp3=median(P_PPT3EAP),
                  pbp=median(p_pbp_prop),
                  pys=median(p_pys),
                  nopp=median(P_NOPPEAP),
                  PC1=mean(PC1),
                  PC2=mean(PC2),
                  PC3=mean(PC3),
                  PC4=mean(PC4)) %>%
        mutate(cluster=factor(cluster)) %>%
        gather(., var, value, patr:nopp, factor_key = TRUE) %>%
        ggplot(aes(x=var, y=round(100*value,2), group=cluster, color=cluster)) +
        coord_polar(theta = 'x') +
        scale_color_manual(values=grayscale_map) +
        #scale_y_continuous(labels = scales::label_percent()) +
        #geom_polygon(fill=NA) + 
        ylim(0, NA) +
        geom_line() +
        facet_wrap(~cluster) +
        theme_minimal() + 
        labs(x="Variable",
             y="%",
             color="Cluster identificado") +
        theme(
              axis.text =  element_text(colour="black", size=16),
              axis.title = element_text(colour="black", size=20)
              )

ggsave('../factor_data_libro_unsam/cap5_ponencia_AASRU/plots/51_carac_clusters.svg',
       width = 12, height = 10,
       bg="white")

deptos <- read_sf('../CONICET_estr_agr/proc_data/deptos.geojson')
pp_gsf <- read_csv('../CONICET_estr_agr/proc_data/pp_gsf.csv')


df <- deptos %>%
        left_join(df_est)

df_est %>%
        left_join(pp_gsf %>% select(link, tot_gsf, rural_agrupada,rural_dispersa,TOTEAP, TOTSup)) %>%
        st_set_geometry(NULL) %>%
        drop_na() %>%
        group_by(cluster2) %>%
        summarise(
                `1 Viv. rural dispersa` = sum(rural_dispersa, na.rm=TRUE),
                `2 Viv. rural agrupada` = sum(rural_agrupada,na.rm=TRUE),
                `3 Pob. agropecuaria` = sum(tot_gsf,na.rm=TRUE),
                `4 Explot. agrop.` = sum(TOTEAP,na.rm=TRUE),
                `5 Superficie explot.` = sum(TOTSup,na.rm=TRUE)
        ) %>%
        mutate_at(vars(`1 Viv. rural dispersa`: `5 Superficie explot.`), funs(./ sum(.))) %>%
        pivot_longer(`1 Viv. rural dispersa`: `5 Superficie explot.`) %>%
        ungroup() %>%
        ggplot(aes(x=name, y=value*100, fill=cluster2)) + 
        geom_bar(stat='identity') +
        scale_fill_manual(values=grayscale_map) +
        geom_text(aes(label=round(value*100,2)), 
                  size = 8, position = position_stack(vjust = 0.5), 
                  color='white') +
        scale_x_discrete(labels = scales::label_wrap(10)) +
        theme_minimal() + 
        theme(legend.position = 'right', 
              legend.text = element_text(colour="black", size=10),
              axis.text =  element_text(colour="black", size=14),
              axis.title = element_text(colour="black", size=16)) +
        labs(fill='Macroestructura',
             x="Indicador",
             y="%")



ggsave('../factor_data_libro_unsam/cap5/plots/52_indic_clusters.svg',
       width = 14, height = 10,
       bg="white")


df_est_agg <- df_est %>%
        drop_na(cluster2) %>%
        st_make_valid() %>%
        group_by(cluster2) %>%
        summarise(n=n()) 


df_est_agg %>% write_sf('./cap5/data/est_aggs.geojson')
