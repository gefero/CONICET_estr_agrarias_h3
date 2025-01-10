library(tidyverse)
library(sf)

deptos <- read_sf('./cap5/data/raw/pxdptodatosok.shp') %>% 
                        st_make_valid() 


deptos <- deptos %>%
        filter(st_is_valid(deptos)) %>%
        rename(departamento = departamen) %>%
        mutate(departamento = tolower(stringi::stri_trans_general(str = departamento, 
                                                                id = "Latin-ASCII")),
               provincia = tolower(stringi::stri_trans_general(str = provincia, 
                                                                  id = "Latin-ASCII")),
        )


deptos[!st_is_valid(deptos),]

naf <- read_sf('./cap5/data/raw/renaf_2.json') %>%
        rename(tam_superf = superf,
               provincia=pcia,
               departamento=dpto,
               clae_naf_desag=tipo_expl) %>%
        mutate(tipo='NAF',
               letra="A",
               clae2=1) %>%
        mutate(across(.cols=c(provincia:departamento, condicion), 
                      ~tolower(stringi::stri_trans_general(str = .x, 
                                                           id = "Latin-ASCII"))
               ))

naf <- naf %>%
        mutate(clae_naf_desag = case_when(
                clae_naf_desag == "A" ~ "Agricultura",
                clae_naf_desag == "M" ~ "Mixta",
                clae_naf_desag == "G" ~ "Ganaderia"),
                clae_naf_ag = clae_naf_desag)

## Estimacion superficie necesaria para trabajo familiar
topes <- tibble(
        provincia = c("corrientes", "misiones", 
                      "buenos aires", "cordoba", "entre rios", "la pampa", "santa fe",
                      "mendoza", "san juan", "san luis",
                      "chaco", "formosa", "santiago del estero", 
                      "jujuy", "salta", "catamarca", "tucuman", "la rioja",
                      "neuquen",
                      "rio negro", "chubut", "santa cruz", "tierra del fuego"),
        tope_has = c(
                rep(500, 2), 
                rep(1000, 5),
                rep(1000, 3),
                rep(1000, 3),
                rep(2500, 5),
                rep(2500, 1),
                rep(5000, 4)
        )
)

naf <- naf %>%
        left_join(topes)


#En 2002 las EAP's de PP contrataron aproximadamente 5.000.000 de jornadas laborales.
# Es decir, unos 15600 trabajadores equivalentes. Esto hace que en promedio, 
# cada EAP de PP contratara 0,071 trabajadores permanentes equivalentes en el año.

naf <- naf %>%
        mutate(tipo_fdt = case_when(
                tam_superf / tope_has <= 1.1 ~ 'Baja probabilidad de uso intesivo de fuerza de tabajo asalariada',
                tam_superf / tope_has >= 5 ~ 'Alta probabilidad de uso intensivo de fuerza de tabajo asalariada',
                TRUE ~ "Media probabilidad de uso intesivo de fuerza de tabajo asalariada"
        )) %>%
        mutate(tam_empleo = case_when(
                tipo_fdt == 'Baja probabilidad de uso intesivo de fuerza de tabajo asalariada' ~ 'a. 0',
                tipo_fdt == 'Media probabilidad de uso intesivo de fuerza de tabajo asalariada' ~ 'a. 1-9',
                tipo_fdt == 'Alta probabilidad de uso intensivo de fuerza de tabajo asalariada' ~ 'b. 10-49'
        ))

naf %>%
        st_set_geometry(NULL) %>%
        group_by(tam_empleo) %>%
        summarise(n=n()) %>%
        mutate(prop=n/sum(n))

naf <- naf %>%
        left_join(deptos %>%
                          st_set_geometry(NULL) %>%
                          select(provincia, departamento, link)
        )

fallos <- naf %>%
        filter(is.na(link)) %>%
        group_by(provincia, departamento) %>%
        summarise(n=n())

# ARREGLAR BIEN LOS LINKS

estab <- read_csv('./cap5/data/raw/distribucion_establecimientos_productivos.csv') %>%
        rename(id=ID,
               link=in_departamentos,
                tam_empleo= empleo) %>%
        mutate(tipo='Estab. OEDE',
               condicion='Persona jurídica (OEDE)',
               tipo_cond='Persona jurídica (OEDE)',
               tam_superf=NA) %>%
        mutate(across(.cols=c(provincia:departamento), 
                      ~tolower(stringi::stri_trans_general(str = .x, 
                                                           id = "Latin-ASCII"))
        ))

estab <- estab %>%
        mutate(link = as.character(link)) %>%
        mutate(link = case_when(
                nchar(link) == 4 ~ paste0('0', link),
                TRUE ~ link
        ))

estab <- estab %>% st_as_sf(coords=c("lon", "lat"))

estab <- estab %>% st_set_crs(st_crs(naf))

estab_agr <- estab %>%
                filter(letra=="A")

clanae <- read_csv('./cap5/data/raw/recod_actividades_establecimientos.csv') %>%
        select(-X9, -X10)

estab_agr <- estab_agr %>%
        left_join(clanae %>% select(clae6, clae_naf_desag, clae_naf_ag), by="clae6")


oede_naf <- estab_agr %>%
        select(id, provincia, departamento, link, tipo, condicion, tipo_cond, letra, clae2, clae_naf_desag,
               clae_naf_ag, tam_empleo) %>%
rbind(
naf %>%
        select(id, provincia, departamento, link, tipo, condicion, tipo_cond, letra, clae2, clae_naf_desag,
               clae_naf_ag, tam_empleo)
)



oede_naf %>%
        ggplot() + 
                geom_sf(data=deptos %>% 
                                filter(departamento != "islas del atlantico sur"), 
                        fill=NA, alpha=0.5) +
                geom_sf(aes(color=tam_empleo), size=0.4, alpha=0.5) +
                scale_color_viridis_d() +
                labs(color="Tipo de registro") +
                theme_minimal()

# id, pcia, depto, link, tipo, letra, clae2, oede_empleo, oede_clae6, oede_clae2, 
## naf_superf, naf_tipo_expl, naf_condicion, naf_tipo_cond

names(estab) 
names(naf)

write_sf(oede_naf, './cap5/data/proc/oede_naf.geojson')

