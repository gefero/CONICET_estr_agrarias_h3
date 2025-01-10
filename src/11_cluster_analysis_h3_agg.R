library(tidymodels)
library(tidyverse)
library(sf)

trimean <- function(x){
        qs <- quantile(x, probs=c(0.25, 0.5, 0.75))
        tri <- as.numeric((qs[1] + 2*qs[2] + qs[3])/4)
        return(tri)
        }

exp_clst <- read_csv('./data/proc/v2_estab_to_h3_pca_clust.csv')
h3 <- read_sf('./data/raw/h3_geoms.geojson')

h3 <- h3 %>%
        left_join(exp_clst)


h3 %>%
        

h3 %>%
        drop_na() %>%
        filter(hc_clst_7 %in% c(4)) %>%
        ggplot() + 
        geom_sf(aes(fill=as.factor(hc_clst_7), 
                    color=as.factor(hc_clst_7))) +
        theme_minimal()

clst_val <- function(data_clst = exp_clst, var_clst="hc_clst_6",
                     indicat = "mean", return_plot=TRUE){
        
        clst_validacion <- data_clst %>%
                group_by(!!sym(var_clst)) %>%
                summarise(across(!starts_with("h"), 
                                 list(mean=mean,
                                      trimean=trimean,
                                      sd=sd, 
                                      sum=sum,
                                      median=median
                                      #   q1=quantile(.x, probs=0.25)
                                      #   q3=quantile(.x, probs=0.5),
                                      #   q4=quantile(.x, probs=0.75),
                                      #   mad = mad
                                 ))
                ) %>%
                pivot_longer(total_mean:prop_ganaderia_sum,
                             names_to = "indicator",
                             values_to = "value")
        
        ind_list <- clst_validacion$indicator %>% unique()
        
        if (return_plot==TRUE){
                patt <- paste0("prop_(.*)_", indicat)
                
                print(clst_validacion %>%
                              filter(indicator %in% c( 
                                      ind_list[ind_list %>% str_detect(pattern=patt)])
                              ) %>%
                              ggplot() +
                              geom_line(aes(x=indicator, y=value, 
                                            group=as.factor(!!sym(var_clst)), 
                                            color=as.factor(!!sym(var_clst))),
                                        show.legend=FALSE
                                        ) +
                              scale_color_viridis_d() +
                              coord_polar() +
                              facet_wrap(as.formula(paste("~", var_clst))) +
                              labs(x="Indicador",
                                   y="%") +
                              theme_minimal() +
                              theme(axis.text.x = element_text(size=7),
                                    axis.text.y = element_text(size=7),
                                    strip.text.x = element_text(size=7),
                                    axis.title = element_text(size=7))
                )
        }
        
        return(clst_validacion)
}
val_cst_7 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_7", indicat="mean")
#val_cst_4 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_4", indicat="mean")
#val_cst_5 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_5", indicat="mean")
#val_cst_6 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_6", indicat="mean")

clst_labels <- tibble(
        hc_clst_7 = 1:7,
        hc_clst_7_labels = c(
                "6. Pred. de peq. patr. (art. c/med., gr y fliares)- Diversif.",
                 "5. Pred. neto de peq. patr.- Peso ganad.",
                 "2. Pred. neto de prod. fliares.- Peso ganad.",
                 "7. Pred. de med. y gr. patr.- Diversif.",
                 "3. Pred. neto de prod. fliares.-Diversif.",
                 "4. Pred. neto de peq. patr.- Peso agric. ",
                 "1. Pred. neto de prod. fliares.- Peso agric.")
)

#write_csv(clst_labels, '../factor_data_libro_unsam/cap5/data/proc/clust_labels.csv')

micro_palette <- c('#762a83','#9970ab','#c2a5cf',
                   '#a6dba0','#5aae61','#1b7837','#00441b')

grey_scale <- c("#dddddd", "#c8c8c8", "#b4b4b4",
                "#797979", "#6b6b6b", "#5d5d5d","#4f4f4f")


ind_list <- val_cst_7$indicator %>% unique()
patt <- paste0("prop_(.*)_", "mean")

val_cst_7 %>%
        left_join(clst_labels) %>%
        filter(indicator %in% c( 
                ind_list[ind_list %>% str_detect(pattern=patt)])
        ) %>%
        mutate(indicator = str_replace(str_replace(indicator, "prop_", ""), "_mean","")) %>%
        ggplot() +
        geom_line(aes(x=indicator, y=value, 
                      group=hc_clst_7_labels, 
                      color=hc_clst_7_labels),
                  show.legend=FALSE
        ) +
        scale_color_manual(values=micro_palette) +
        coord_polar() +
        facet_wrap(~hc_clst_7_labels) +
        labs(x="Indicador",
             y="%") +
        theme_minimal() +
        theme(axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=7),
              strip.text.x = element_text(size=8),
              axis.title = element_text(size=7))

ggsave('./cap5/plots/54_caract_clst_mean.png', 
       width = 11, height = 13,
       bg="white")

exp_clst %>%
        left_join(clst_labels) %>%
        mutate(prop_total = total / sum(total)) %>%
        select(-c(hc_clst_4:hc_clst_6, total)) %>% 
        select(h3, prop_total, everything()) %>%
        pivot_longer(prop_total:prop_ganaderia) %>%
        #filter(!name %in% c("hc_clst_4", "hc_clst_5", "hc_clst_6")) %>%
        ggplot() + 
        geom_boxplot(aes(x=name, y=round(100*value,2), fill=hc_clst_7_labels),
                     show.legend = FALSE) + 
        scale_fill_manual(values=grey_scale) +
        coord_flip() +
        facet_wrap(~hc_clst_7_labels) + 
        labs(x="Indicador",
             y="%") +
        theme_minimal() +
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=7),
              strip.text.x = element_text(size=7),
              axis.title = element_text(size=7))


ggsave('./cap5/plots/55_caract_clst_boxplot.svg', 
       width = 10, height = 12,
       bg="white")

est_soc <- read_sf('../CONICET_estr_agr/proc_data/tablas_finales/frontera_depto.geojson')

tictoc::tic()
h3 <- h3 %>%
        drop_na() %>%
        st_join(
                est_soc %>% select(link, cluster2),
                largest = TRUE
        )
tictoc::toc()

#write_sf(h3, './cap5/data/proc/v2_estab_to_h3_pca_clust_depto.geojson')

pais_malv <- read_sf('./cap5/data/raw/pais_malvinas.geojson')
h3 <- read_sf('./cap5/data/proc/v2_estab_to_h3_pca_clust_depto.geojson')



h3 %>% 
        drop_na(total) %>% 
        st_set_geometry(NULL) %>% 
        left_join(clst_labels) %>%
        group_by(cluster2, hc_clst_7_labels) %>% 
        summarise(n=sum(total)) %>%
        mutate(prop = n/sum(n)) %>%
        ggplot() + 
                geom_col(aes(x=cluster2, y=round(100*prop,2), fill=hc_clst_7_labels)) +
                scale_fill_manual(values=micro_palette) +
                #scale_fill_grey(start=0.8, end=0.1) +
                scale_x_discrete(labels = label_wrap(10)) +
                theme_minimal() +
                labs(x="Macroestructuras",
                     y="%",
                     fill="Microestructuras")


ggsave('./cap5/plots/v256_clst_estr_agr.png', 
       width = 10, height = 6,
       bg="white")


ggplot() + 
        geom_sf(data=pais_malv, fill=NA) +
        geom_sf(data=est_soc %>%
                        group_by(provincia_cna18) %>%
                        summarise(), fill=NA
                ) +
                geom_sf(
                        data = h3 %>% 
                                drop_na(total) %>%
                                left_join(clst_labels),
                        aes(fill=hc_clst_7_labels), color=NA) +
                scale_fill_manual(values=micro_palette) +
        labs(fill="Microestructuras") +
        theme_minimal()


ggplot() + 
        geom_sf(data=pais_malv, fill=NA) +
        geom_sf(data=est_soc %>%
                        group_by(provincia_cna18) %>%
                        summarise(), fill=NA) +
        geom_sf(
                data = h3 %>% 
                        drop_na(total) %>%
                        left_join(clst_labels),
                aes(fill=hc_clst_7_labels), color=NA) +
        coord_sf(xlim = st_bbox(est_soc)[c(1,3)], # min & max of x values
                 ylim = st_bbox(est_soc)[c(2,4)]) + # min & max of y values
        scale_fill_manual(values=micro_palette) +
        labs(fill="Microestructuras") +
        theme_minimal()


ggsave('./cap5/plots/57_clst_estr_agr.png', width = 8, height = 10)

sta_fe_pol <- est_soc %>%
        group_by(provincia_cna18) %>%
        summarise() %>%
        filter(provincia_cna18=="santa fe")

sta_fe <- h3 %>% 
        drop_na(total) %>%
        left_join(clst_labels) %>%
        st_filter(sta_fe_pol)
                

ggplot() + 
        geom_sf(data=est_soc %>% filter(provincia_cna18=="santa fe"), fill=NA) + 
        geom_sf(data=sta_fe, aes(fill=hc_clst_7_labels), color=NA) +
        scale_fill_manual(values=micro_palette) +
        labs(fill="Microestructuras") +
        theme_minimal()

