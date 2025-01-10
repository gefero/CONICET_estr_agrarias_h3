## gen leaflet map

library(tidyverse)
library(sf)
library(leaflet)
library(leaflegend)

# Cargo datos
est_soc <- read_sf('../CONICET_estr_agr/proc_data/tablas_finales/frontera_depto.geojson') %>%
        mutate(cluster2 = case_when(
                cluster2 == "3-Alto desarrollo c/pequeña producción capitalista y mercantil simple"~ "3-Alto grado de desarrollo con pequeña producción capitalista y mercantil simple",
                TRUE ~ cluster2)
               ) %>%
        mutate(cluster2 = str_replace(cluster2, "-", ". "))
 

macro_estr <- est_soc %>%
        group_by(cluster2) %>%
        summarise()

h3 <- read_sf('./cap_5/data/proc/v2_estab_to_h3_pca_clust_depto.geojson') %>%
        select(h3, link, total:prop_ganaderia, hc_clst_7, cluster2)

h3 <- h3 %>%
        mutate(across(c(link,cluster2), ~if_else(is.na(.x), "S/D", .x)))


clst_labels <- tibble(
        hc_clst_7 = 1:7,
        hc_clst_7_labels = c(
                "6. Predominio de pequeños patrones (articulados con familiares, patrones medianos y grandes)-Diversif.",
                "5. Predominio neto de pequeños patrones - Peso ganadería",
                "2. Predominio neto de productores familiares - Peso ganadería",
                "7. Predominio de medianos y grandes patrones - Diversificados",
                "3. Predominio neto de productores familiares - Diversificados",
                "4. Predominio neto de pequeños patrones - Peso agricultura ",
                "1. Predominio neto de productores familiares - Peso agricultura")
)

h3 <- h3 %>%
        left_join(clst_labels) 

#labels <- sprintf(
#        "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
#        h3$link, 
#  ) %>% lapply(htmltools::HTML)

pal_h3 <- colorFactor(c('#762a83','#9970ab','#c2a5cf',
                        '#a6dba0','#5aae61','#1b7837','#00441b'),
                      levels = clst_labels$hc_clst_7_labels %>% sort())


pal_macro <- colorFactor("viridis", levels = macro_estr$cluster2 %>% unique() %>% sort())

h3_map <- h3 %>% select(h3, hc_clst_7_labels)        

mapa <- leaflet() %>% 
        addTiles('https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG%3A3857@png/{z}/{x}/{-y}.png'
                 , attribution = "Argenmap v2 - Instituto Geográfico Nacional",
                 group = "Argenmap v2") %>%
        setView(-63.32,-38.41, zoom = 4) %>%
        addPolygons(data = h3_map, 
                    group = "Micro",
                    #fill = pal_h3, 
                    color = ~pal_h3(hc_clst_7_labels),
                    stroke = 0.0001, 
                    fillOpacity = 0.8,
         #            label = ~paste0("Provincia: ", link, "<br>",
         #                          "Depto: ", link, "<br>",
         #                          "Total unid: ", total, "<br>",
         #                          "Prod. fliares: ", prop_sin_asal, "<br>",
         #                          "Peq. patr.: ", prop_peq_patr, "<br>",
         #                          "Med. y gr. patr: ", prop_myg_patr, "<br>",
         #                          "Unid. agríc.: ", prop_agric, "<br>",
         #                          "Unid. ganad: ", prop_ganaderia, "<br>",
         #                          "Macroestr: ", cluster2, "<br>"),
         #            labelOptions = labelOptions(noHide = FALSE,  # Controla cuándo se muestran las etiquetas
         #                                        direction = "auto",  # Posición automática de la etiqueta
         #                                        textOnly = TRUE),
         #            highlight = highlightOptions(
         #                    weight = 5,
         #                    color = "#666",
         #                    fillOpacity = 0.7,
         #                    bringToFront = TRUE)
          ) %>%
        addPolygons(data = macro_estr, 
                    group = "Macroestructuras",
                    #fill = pal_h3, 
                    color = ~pal_macro(cluster2),
                    stroke = TRUE,
                    weight = 2,
                    fillColor = "#00000",
                    fillOpacity = 0.0001
                    ) %>%
        addLegend(data = h3_map,
                  position = "bottomright",
                  pal = pal_h3, 
                  values = ~hc_clst_7_labels,
                  title = "Microestructuras",
                  opacity = 1) %>%
        addLegend(data = macro_estr,
                  position = "bottomright",
                  pal = pal_macro, 
                  values = ~cluster2,
                  title = "Macroestructuras",
                  opacity = 1) %>%
        addLayersControl(
                baseGroups = c(
                        "Argenmap v2 (default)"
                ),
                overlayGroups = c("Macroestructuras", "Microestructuras"),
                options = layersControlOptions(collapsed = TRUE)
        )


#mapa 

htmlwidgets::saveWidget(mapa, file="03_microestructuras.html")


