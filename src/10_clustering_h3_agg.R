library(tidymodels)
library(tidyverse)
library(sf)

exp_2_h3 <- read_csv('./data/proc/v2_estab_to_h3.csv')


exp_clst <- exp_2_h3 %>%
        select(h3,total, prop_sin_asal, prop_peq_patr,
               prop_myg_patr, prop_agric, prop_ganaderia)

recipe_pca <- exp_clst %>%
        recipe(~.) %>%
        update_role(h3, new_role = "id variable") %>%
        step_normalize(all_numeric()) %>%
        step_pca(all_numeric(), num_comp = 4)

recipe_pca
pca_estimates <- prep(recipe_pca, training = exp_clst)
pca_data <- bake(pca_estimates, exp_clst) %>% mutate(h3=as.character(h3))


tidy(pca_estimates, 2, type = "coef") %>%
        filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>%
        ggplot(aes(value, terms, fill = terms)) + 
        geom_col(show.legend = FALSE) + 
        geom_text(aes(label = round(value,2))) + 
        labs(title = "Cargas factoriales (comp. 1 y 2)", x = "Valor", y = "Variable") +
        facet_wrap(~component, nrow = 1) + theme_minimal()

tidy(pca_estimates, 2, type = "variance") %>%
        filter(terms == "percent variance") %>%
        mutate(component = paste0("PC", component)) %>%
        ggplot(aes(x = component, y = value, group = terms)) + 
        geom_col() + 
        ylim(0, 100) +
        labs(title = "% varianza", x = "Componente", y = "Valor") +
        theme_minimal() 


# Dissimilarity matrix
d <- dist(pca_data %>% select(-h3), method = "euclidian")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "ward.D")

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


exp_clst <- exp_clst %>%
        mutate(
                hc_clst_4 = cutree(hc1, k=4),
                hc_clst_5 = cutree(hc1, k=5),
                hc_clst_6 = cutree(hc1, k=6),
                hc_clst_7 = cutree(hc1, k=7)
        )

write_csv(exp_clst, './cap5/data/proc/v2_estab_to_h3_pca_clust.csv')
