library(DBI)
library(RSQLite)
library(dbplyr)
library(tidyverse)
con <- dbConnect(SQLite(), "../censito/Ampliado/database/CNPyV2010_ampliado")


 
hog_tam <- tbl(con, sql("SELECT X.VIVIENDA_REF_ID, X.HOGAR_REF_ID, X.P01, X.ACTNUMERO, X.P32, X.EXPH, X.TOTPERS, K.DPTO FROM(
        SELECT h.VIVIENDA_REF_ID, p.HOGAR_REF_ID, p.P01, p.ACTNUMERO, p.P32, h.TOTPERS, h.EXPH
        FROM PERSONA p
        INNER JOIN HOGAR h
        ON h.HOGAR_REF_ID= p.HOGAR_REF_ID
        WHERE p.ACTNUMERO=1 AND (p.P32 = 3 OR p.P32=4) AND p.P01=1) AS X
INNER JOIN (SELECT* FROM (
        SELECT v.VIVIENDA_REF_ID, v.DPTO_REF_ID, d.DPTO
        FROM VIVIENDA v
        INNER JOIN DPTO d
        ON V.DPTO_REF_ID= d.DPTO_REF_ID
)) AS K 
ON K.VIVIENDA_REF_ID=X.VIVIENDA_REF_ID")) %>% collect() %>%
        janitor::clean_names() %>%
        mutate(tam_hogar = totpers*exph,
               dpto = as.character(dpto)) %>%
        mutate(dpto = case_when(
                       nchar(dpto) == 4 ~ paste0('0', dpto),
                       TRUE ~ dpto))

hog_tam_agg <-hog_tam %>%
        group_by(dpto) %>%
        summarise(
                n = n(),
                mean_tam = mean(tam_hogar),
                  sd_tam = sd(tam_hogar),
                  median_tam = median(tam_hogar),
                  mad_tam = mad(tam_hogar))


### JOINEAR ESTO CONTRA LAS DEMANDAS DE FdT