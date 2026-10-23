########################################################################################
###################### Bird communities across restoration stages ######################
########################################################################################

####### This script allows for plotting species accumulation curves (sampling completeness)
### and analyzing differences in community richness and compositions across restoration stages.

####### For this you need a database with the year of monitoring, the age (# of years since restoration), 
## treatment (if there are different restoration techniques), plot id, bird scientific name and abundance.

library(tidyverse)
library(vegan)
library(iNEXT)
library(ggplot2)
library(gridExtra)
library(car)
library(dplyr)
library(patchwork) # Para combinar gráficos con leyenda compartida


Aves <- read_csv("avesv1.csv")
head(Aves)

# Clean database

Aves <- Aves %>%
  mutate(sc_name = case_when(
    str_detect(sc_name, "Amazilia sp.") ~ "Amazilia tzacatl",
    str_detect(sc_name, "Trochilidae") ~ "Amazilia tzacatl",
    str_detect(sc_name, "Amazona sp.") ~ "Amazona autumnalis",
    str_detect(sc_name, "Leptotila (llamado)") ~ "Leptotila verreauxi",
    str_detect(sc_name, "Dendrocolaptinae") ~ "Melanerpes rubricapillus",
    str_detect(sc_name, "Parkesia sp.") ~ "Parkesia noveboracensis",
    TRUE ~ sc_name  # mantener el valor original si no hay coincidencia
  ))

#Aves <- Aves %>%
 # mutate(plot_parcela = str_c(plot_id, parcela, sep = "_"))

# Species per site matrix
species_matrix <- Aves %>%
  group_by(plot_id, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  # pivot_wider(names_from = plot_parcela,  # now columns are sites
  pivot_wider(names_from = sc_name,  # now columns are sites
    values_from = total_abundance, values_fill = 0)

head(species_matrix)
species_matrix[is.na(species_matrix)] <- 0

# No. of species -- 73 - 85
ncol(species_matrix)

#################################################################
################ (1)  Species accumulation curve ################
# Has the project had enough sampling? 
#################################################################

head(species_matrix)
curva <- specaccum(species_matrix[, -1])
plot(curva)  #con todos los datos pero esperamos que haya una comunidad diferente en negraforra vs restauracion

# Non restoration
species_matrix_NF <- Aves %>%
  filter(treatment == "NF") %>%
  group_by(plot_id, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  pivot_wider(names_from = sc_name, values_from = total_abundance, values_fill = 0)

head(species_matrix_NF)
#curvaNF <- specaccum(species_matrix_NF[, -1], method="rarefaction")
curvaNF <- specaccum(species_matrix_NF[, -1]) #27 sp
plot(curvaNF, main="Curva de acumulaciC3n de especies multianual en negraforra")

# Restoration
species_matrix_RF <- Aves %>%
  filter(treatment == "RF") %>%
  group_by(plot_id, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  pivot_wider(names_from = sc_name, values_from = total_abundance, values_fill = 0)

head(species_matrix_RF)
species_matrix_RF[is.na(species_matrix_RF)] <- 0
curvaRF <- specaccum(species_matrix_RF[, -1])
plot(curvaRF, main="Curva de acumulaciC3n de especies multianual en parcelas de restauraciC3n")



### Another option.. with Hill numbers. 

species_matrixd <- Aves %>%
  group_by(plot_parcela, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  # pivot_wider(names_from = plot_parcela,  # now columns are sites
  pivot_wider(names_from = plot_parcela,  # now columns are sites
              values_from = total_abundance, values_fill = 0)

sp_matrix <- as.data.frame(species_matrixd)
sp_matrix[is.na(sp_matrix)] <- 0
# Remove species name column — only keep abundance data
sp_matrix <- sp_matrix[, -1]

D <- iNEXT(sp_matrix, datatype = "abundance")
plot(D)

## by treatment
species_matrixt <- Aves %>%
  group_by(treatment, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  # pivot_wider(names_from = plot_parcela,  # now columns are sites
  pivot_wider(names_from = treatment,  # now columns are sites
              values_from = total_abundance, values_fill = 0)

sp_matrixt <- as.data.frame(species_matrixt)
sp_matrixt[is.na(sp_matrixt)] <- 0
# Remove species name column — only keep abundance data
sp_matrixt <- sp_matrixt[, -1]

Dt <- iNEXT(sp_matrixt, datatype = "abundance", q=0)
plot(Dt)

##### Plot

Dt_q0 <- iNEXT(sp_matrixt, datatype = "abundance",  q=0 , endpoint=150)
Dt_q1 <- iNEXT(sp_matrixt, datatype = "abundance",  q=1 , endpoint=150)
Dt_q2 <- iNEXT(sp_matrixt, datatype = "abundance",  q=2 , endpoint=150)

# Plot Shannon diversity (q = 1) --
plot_dq0 <- ggiNEXT(Dt_q0, type = 1) +
  labs(    #title = "Riqueza de especies (q = 0)",
    x = "# individuos",
    y = "Riqueza de especies (q = 0)" #
  ) +
  theme_minimal()+
  theme(legend.position = "none")  

#  Shannon diversity (q = 1)
# # the more even the abundances, the higher the diversity value. 
#It gives more weight to common species compared to the \(q=0\) case.
plot_dq1 <- ggiNEXT(Dt_q1, type = 1) +
  labs(    #title = "Shannon Diversity (q = 1)",
    x = "# individuos",
    y = "Diversidad de Shannon (q = 1)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

#  It is a measure of dominance, meaning it is disproportionately sensitive
#to abundant species. The higher the value of \(q\), 
#the more sensitive the index is to the most dominant species in the communit
plot_dq2 <- ggiNEXT(Dt_q2, type = 1) +
  labs(    #title = "",
    x = "# individuos",
    y = "Diversidad de Simpson (q = 2)"
  ) +
  theme_minimal()+
  theme(legend.position = "none")  


# Display plots side by side
grid.arrange(plot_dq0, plot_dq1, plot_dq2, ncol = 3)


## For the above we used maximum abundance insted of summing abundances per plot to avoid double-counting. 
# but it is possible to do it using sum and the results would be roughly similar. 

#species_matrixc <- Aves %>%
#  group_by(treatment, sc_name) %>%
#  summarise(total_abundance = sum(abundance), .groups = 'drop') %>%
# pivot_wider(names_from = plot_parcela,  # now columns are sites
# pivot_wider(names_from = treatment,  # now columns are sites
#    values_from = total_abundance, values_fill = 0)

#sp_matrixc <- as.data.frame(species_matrixc)
#sp_matrixc[is.na(sp_matrixc)] <- 0
# Remove species name column — only keep abundance data
#sp_matrixc <- sp_matrixc[, -1]

#Dc <- iNEXT(sp_matrixc, datatype = "abundance")
#plot(Dc)

###### by age

species_matrixq <- Aves %>%
  group_by(age, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  # pivot_wider(names_from = plot_parcela,  # now columns are sites
  pivot_wider(names_from = age,  # now columns are sites
              values_from = total_abundance, values_fill = 0)

sp_matrixq <- as.data.frame(species_matrixq)
sp_matrixq[is.na(sp_matrixq)] <- 0
# Remove species name column — only keep abundance data
sp_matrixq <- sp_matrixq[, -1]

Dq <- iNEXT(sp_matrixq, datatype = "abundance")
plot(Dq)

## Same sampling units
DQD_q0 <- iNEXT(sp_matrixq, datatype = "abundance",  q=0 , endpoint=150)
DQD_q1 <- iNEXT(sp_matrixq, datatype = "abundance",  q=1 , endpoint=150)
DQD_q2 <- iNEXT(sp_matrixq, datatype = "abundance",  q=2 , endpoint=150)

# Plot Shannon diversity (q = 1) --
plot_q0 <- ggiNEXT(DQD_q0, type = 1) +
  labs(    #title = "Riqueza de especies (q = 0)",
    x = "# individuos",
    y = "Riqueza de especies (q = 0)" #
  ) +
  theme_minimal()+
  theme(legend.position = "none")  

#  Shannon diversity (q = 1)
# # the more even the abundances, the higher the diversity value. 
#It gives more weight to common species compared to the \(q=0\) case.
plot_q1 <- ggiNEXT(DQD_q1, type = 1) +
  labs(    #title = "Shannon Diversity (q = 1)",
    x = "# individuos",
    y = "Diversidad de Shannon (q = 1)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

#  It is a measure of dominance, meaning it is disproportionately sensitive
#to abundant species. The higher the value of \(q\), 
#the more sensitive the index is to the most dominant species in the communit
plot_q2 <- ggiNEXT(DQD_q2, type = 1) +
  labs(    #title = "",
    x = "# individuos",
    y = "Diversidad de Simpson (q = 2)"
  ) +
  theme_minimal()+
  theme(legend.position = "none")  


# Display plots side by side
grid.arrange(plot_q0, plot_q1, plot_q2, ncol = 3)

#######################################################33
## por edad y todos los individuos

species_matrixw <- Aves %>%
  group_by(age, sc_name) %>%
  summarise(total_abundance = sum(abundance), .groups = 'drop') %>%
  # pivot_wider(names_from = plot_parcela,  # now columns are sites
  pivot_wider(names_from = age,  # now columns are sites
              values_from = total_abundance, values_fill = 0)

sp_matrixw <- as.data.frame(species_matrixw)
sp_matrixw[is.na(sp_matrixw)] <- 0
# Remove species name column — only keep abundance data
sp_matrixw <- sp_matrixw[, -1]

Dw <- iNEXT(sp_matrixw, datatype = "abundance")
plot(Dw)

# species per cober
species_matrixw %>%
  summarise(
    especies_en_cob_0 = sum(`0` > 0, na.rm = TRUE),
    especies_en_cob_2 = sum(`2` > 0, na.rm = TRUE),
    especies_en_cob_4 = sum(`4` > 0, na.rm = TRUE)
  )

#################################################################
############# (2)  Differences in Species Richness ##############
# Do restoration stage vary in species richness? Do it vary in diversity?
#################################################################

ambiental <- Aves %>%
  #select(plot_parcela, treatment, age)
  select(plot_id, age)%>%
  distinct()

# Calculate species richness for each plot
sppr <- species_matrix %>%
  column_to_rownames(var = "plot_id") %>%
  specnumber()

sppr

# Convert species richness to a dataframe
sppr_df <- enframe(sppr, name = "plot_id", value = "sppr")

# Join with environmental data (age)
sppr_df <- sppr_df %>%
  left_join(ambiental, by = "plot_id")

sppr_df <- sppr_df %>%
  mutate(
    age = ifelse(age == "BM", "10", age),
    age = ifelse(age %in% c("6", "7"), "6.5", age)
  )

#Assumptions
#shapiro.test(residuals(sppr_aov)) # 0.03 no normal
leveneTest(sppr ~ as.factor(age), data = sppr_df) #homogenity of variances - 0.3

## since data is not normal use kruskal wallis
kruskal.test(sppr ~ age, data = sppr_df) # 0.07 We fail to reject the hypothesis that all groups have equal distribution of richness

#analysis of variance
#sppr_aov <- aov(sppr ~ age, data = sppr_df)
#summary(sppr_aov) # Pareciaria no haber dif sig. en sp richness por cobertura


# Plot

ggplot(sppr_df, aes(x = factor(age), y = sppr, fill = factor(age))) +
  geom_boxplot() +
  geom_jitter() +
  labs(
    x = "Edad",
    y = "Riqueza",
    #title = "Species richness by forest age"
  ) +
  scale_fill_manual(values = c("lightsalmon1", "gold1", "palegreen4")) +
  theme_minimal()+
  theme(legend.position = "none") 

sppr_df %>%
  group_by(age) %>%
  summarise(
    mean_richness = mean(sppr),
    median_richness = median (sppr),
    sd_sppr = sd(sppr),
    n = n(),
    max = max(sppr),
    min = min(sppr)
  )

## now with Shannon

diversity_indices <- data.frame(
  plot_parcela = rownames(sppr),
  sp_number = specnumber(sppr),
  shannon = diversity(sppr, index = "shannon"),
  simpson = diversity(sppr, index = "simpson")
)


ggplot(diversity_df, aes(x = factor(age), y = shannon, fill = factor(age))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "gray30") +
  labs(
    x = "Age",
    y = "Shannon diversity",
    title = "Shannon diversity by forest age"
  ) +
  scale_fill_manual(values = c("lightsalmon1", "gold1", "palegreen4")) +
  theme_minimal()

########## REstored vs non restored

ambientald <- Aves %>%
  #select(plot_parcela, treatment, age)
  select(plot_parcela, treatment)%>%
  distinct()

#analysis of variance
sppr_aovd <- aov(sppr ~ treatment, data = sppr_df)
summary(sppr_aovd) # Pareciaria no haber dif sig. en sp richness por cobertura

################ Assumptions

shapiro.test(residuals(sppr_aovd)) # 0.06 then normal
leveneTest(sppr_aovd) #homogenity of variances

ggplot(sppr_dfd, aes(x = factor(treatment.x), y = sppr, fill = factor(treatment.x))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "gray30") +
  labs(
    x = "Tratamiento",
    y = "Riqueza de especies",
    #title = "Shannon diversity by forest age"
  ) +
  scale_fill_manual(values = c("lightsalmon1", "lightgreen")) + #, "palegreen4")) +
  theme_minimal()+
  theme(legend.position = "none") 

#################################################################
########### (3)  Differences in Community Composition ############
# Do species composition vary across restoration stage?
#################################################################

################### TOP 5: the most abundant species per age

# Summarize total abundance per species per age
species_abundance <- Aves %>%
  group_by(age, sc_name) %>%
  summarise(total_abundance = sum(abundance), .groups = 'drop')

# Get top 5 species per age class
top_species <- species_abundance %>%
  group_by(age) %>%
  slice_max(total_abundance, n = 5, with_ties = FALSE) %>%
  ungroup()

# Mark species as top or others
species_abundance <- species_abundance %>%
  mutate(species_group = ifelse(
    paste(age, sc_name) %in% paste(top_species$age, top_species$sc_name),
    sc_name,
    "Others"
  ))

# Calculate total abundance per age including others
relative_abundance <- species_abundance %>%
  group_by(age, species_group) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop') %>%
  group_by(age) %>%
  mutate(rel_abundance = total_abundance / sum(total_abundance, na.rm = TRUE))

# Plot stacked bar plot of relative abundance per age
ggplot(relative_abundance, aes(x = factor(age), y = rel_abundance, fill = species_group)) +
  geom_col() +
  labs(
    x = "Edad",
    y = "Abundancia relativa",
    fill = "Especies",
    #title = "Relative Abundance of Top 5 Bird Species per Age Class"
  ) +
  theme_minimal()

###################### Now, analyze the whole community

# Summarize total abundance per species per age
species_abundance <- Aves %>%
  group_by(age, sc_name) %>%
  summarise(total_abundance = sum(abundance), .groups = 'drop')

# Calculate relative abundance per age class for all species (no grouping)
relative_abundance <- species_abundance %>%
  group_by(age) %>%
  mutate(rel_abundance = total_abundance / sum(total_abundance, na.rm = TRUE)) %>%
  ungroup()

# Plot stacked bar plot with all species individually
ggplot(relative_abundance, aes(x = factor(age), y = rel_abundance, fill = sc_name)) +
  geom_col() +
  labs(
    x = "Edad",
    y = "Abundancia relativa",
    fill = "Especies"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") 

top5_per_age <- relative_abundance %>%
  group_by(age) %>%
  slice_max(rel_abundance, n = 5, with_ties = FALSE) %>%
  ungroup()

top5_per_age

############# MArk the ones with 1 individual as others

# Summarize total abundance per species per age
species_abundance <- Aves %>%
  group_by(age, sc_name) %>%
  summarise(total_abundance = sum(abundance), .groups = 'drop')

# Mark species with total abundance = 1 as "Others"
species_abundance <- species_abundance %>%
  mutate(
    species_group = ifelse(total_abundance == 1, "Others", sc_name)
  )

# Recalculate total abundance per species_group per age (since multiple "Others" will group together)
relative_abundance <- species_abundance %>%
  group_by(age, species_group) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop') %>%
  group_by(age) %>%
  mutate(rel_abundance = total_abundance / sum(total_abundance, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(rel_abundance))

# Plot stacked bar plot with species groups
ggplot(relative_abundance, aes(x = factor(age), y = rel_abundance, fill = species_group)) +
  geom_col() +
  labs(
    x = "Edad",
    y = "Abundancia relativa",
    fill = "Especies"
  ) +
  theme_minimal() +
  #scale_fill_brewer(palette = "Paired") + 
  theme(legend.position = "right")

############ PERMANOVA -- Differences in community composition using bray-curtis distance

# Make sure Aves has only relevant columns
aves_clean <- Aves %>%
  select(plot_parcela, age, sc_name, abundance)

# Species per site 
abund_matrix <- aves_clean %>%
  group_by(plot_parcela, sc_name) %>%
  summarise(total_abundance = sum(abundance), .groups = "drop") %>%
  pivot_wider(names_from = sc_name, values_from = total_abundance, values_fill = 0)

# Extract metadata
meta <- aves_clean %>%
  select(plot_parcela, age) %>%
  distinct()

# Join metadata with abundance matrix
adonis_data <- abund_matrix %>%
  left_join(meta, by = "plot_parcela")

# Save age vector and abundance matrix
age_vector <- adonis_data$age
abund_only <- adonis_data %>%
  select(-plot_parcela, -age) %>%
  replace(is.na(.), 0)

## Bray-Curtis distance
dist_matrix <- vegdist(abund_only, method = "bray")

adonis_result <- adonis2(dist_matrix ~ age, data = adonis_data, permutations = 999)
print(adonis_result)


#################################333 Version 2026 #############33

# library(tidyverse)
library(iNEXT)
library(ggplot2)

# 1. Unir categorías 6 y 7 + Corregir suma de abundancia
species_matrixq <- Aves %>%
  mutate(age = case_when(
    age %in% c("6", "7", 6, 7) ~ "6-7",
    TRUE ~ as.character(age)
  )) %>%
  group_by(age, sc_name) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = age, 
    values_from = total_abundance, 
    values_fill = 0
  )

# 2. Formatear matriz (especies en filas, sitios/edades en columnas)
sp_matrixq <- as.data.frame(species_matrixq)
rownames(sp_matrixq) <- sp_matrixq$sc_name
sp_matrixq <- sp_matrixq[, -1]

# 3. Calcular iNEXT para q = 0, 1 y 2 simultáneamente
Dq <- iNEXT(sp_matrixq, q = c(0, 1, 2), datatype = "abundance", endpoint = 150)

q_labels <- c(
  "0" = "Riqueza de especies (q = 0)",
  "1" = "Diversidad de Shannon (q = 1)",
  "2" = "Diversidad de Simpson (q = 2)"
)

# -------------------------------------------------------------------------
# OPCIÓN A: Gráficos por número de individuos (type = 1) unificados en facetas
# -------------------------------------------------------------------------
plot_individuos <- ggiNEXT(Dq, type = 1, facet.var = "Order.q") +
  facet_wrap(~Order.q, labeller = as_labeller(q_labels)) +
  scale_shape_manual(values = 1:ncol(sp_matrixq)) +
  labs(
    x = "Número de individuos",
    y = "Diversidad de especies",
    color = "Edad", fill = "Edad", shape = "Edad"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

print(plot_individuos)

# -------------------------------------------------------------------------
# OPCIÓN B: Gráficos por Cobertura Muestral (type = 3) -- Corrige el desbalance
# -------------------------------------------------------------------------
plot_cobertura <- ggiNEXT(Dq, type = 3, facet.var = "Order.q") +
  facet_wrap(~Order.q, labeller = as_labeller(q_labels)) +
  scale_shape_manual(values = 1:ncol(sp_matrixq)) +
  labs(
    x = "Cobertura muestral (Sample Coverage)",
    y = "Diversidad de especies",
    color = "Edad", fill = "Edad", shape = "Edad"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

print(plot_cobertura)

# # 4. Unir ambos gráficos lado a lado con UNA sola leyenda abajo
grafico_final <- (plot_individuos + plot_cobertura) +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom")

print(grafico_final)

library(tidyverse)
library(vegan)
library(car)

# -------------------------------------------------------------------------
# 0. Preparación y armonización de datos
# -------------------------------------------------------------------------

# Niveles ordenados para la edad de sucesión
age_levels <- c("0", "2", "4", "6.5", "10")

# Armonizar categorías de edad y extraer tratamiento desde Aves
Aves <- Aves %>%
  mutate(age = case_when(
    age %in% c("BM", "10") ~ "10",
    age %in% c("6", "7", "6.5") ~ "6.5",
    TRUE ~ as.character(age)
  ),
  age = factor(age, levels = age_levels))

# Extraer metadata de tratamiento por parcela
meta_treatment <- Aves %>%
  select(plot_id, treatment) %>%
  distinct()

# Actualizar y enriquecer sppr_df
sppr_df <- sppr_df %>%
  mutate(age = case_when(
    age %in% c("BM", "10") ~ "10",
    age %in% c("6", "7", "6.5") ~ "6.5",
    TRUE ~ as.character(age)
  ),
  age = factor(age, levels = age_levels)) %>%
  left_join(meta_treatment, by = "plot_id")


# -------------------------------------------------------------------------
# 1. Riqueza de especies por edad
# -------------------------------------------------------------------------

# Supuestos estadísticos
sppr_aov <- aov(sppr ~ age, data = sppr_df)
shapiro.test(residuals(sppr_aov))
leveneTest(sppr ~ age, data = sppr_df)

# Prueba no paramétrica (Kruskal-Wallis)
kruskal.test(sppr ~ age, data = sppr_df)

# Gráfico de Riqueza por Edad
riq <- ggplot(sppr_df, aes(x = age, y = sppr, fill = age)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.8) +
  geom_jitter(width = 0.12, size = 2, alpha = 0.6, color = "gray30") +
  labs(x = "Edad (años)", y = "Riqueza de especies") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal() +
  theme(legend.position = "none")

# Resumen estadístico de riqueza
sppr_df %>%
  group_by(age) %>%
  summarise(
    mean_richness   = mean(sppr, na.rm = TRUE),
    median_richness = median(sppr, na.rm = TRUE),
    sd_sppr         = sd(sppr, na.rm = TRUE),
    n               = n(),
    min             = min(sppr, na.rm = TRUE),
    max             = max(sppr, na.rm = TRUE)
  )


# -------------------------------------------------------------------------
# 2. Matriz de abundancia e Índices de Shannon y Simpson
# -------------------------------------------------------------------------

# Crear matriz sitio x especie desde Aves
abund_matrix <- Aves %>%
  group_by(plot_id, sc_name) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sc_name, values_from = total_abundance, values_fill = 0)

meta_sites <- Aves %>%
  select(plot_id, age, treatment) %>%
  distinct()

abund_only <- abund_matrix %>%
  select(-plot_id)

# Calcular índices de diversidad con vegan
diversity_df <- data.frame(
  plot_id   = abund_matrix$plot_id,
  sp_number = specnumber(abund_only),
  shannon   = diversity(abund_only, index = "shannon"),
  simpson   = diversity(abund_only, index = "simpson")
) %>%
  left_join(meta_sites, by = "plot_id") %>%
  mutate(age = factor(age, levels = age_levels))

# Gráfico Diversidad de Shannon
div <- ggplot(diversity_df, aes(x = age, y = shannon, fill = age)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.8, color = "black") +
  geom_jitter(width = 0.12, size = 2, alpha = 0.6, color = "gray30") +
  labs(x = "Edad (años)", y = "Diversidad de Shannon") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal() +
  theme(legend.position = "none")

grafico_unificado <- riq + div

print(grafico_unificado)

#Resumen 

resumen_trat_edad <- diversity_df %>%
  group_by(treatment, age) %>%
  summarise(
    n = n(),
    # Riqueza de especies
    min_riqueza  = min(sp_number),
    mean_riqueza = round(mean(sp_number, na.rm = TRUE), 2),
    max_riqueza  = max(sp_number),
    # Diversidad de Shannon
    min_shannon  = round(min(shannon, na.rm = TRUE), 2),
    mean_shannon = round(mean(shannon, na.rm = TRUE), 2),
    max_shannon  = round(max(shannon, na.rm = TRUE), 2),
    # Diversidad de Simpson
    min_simpson  = round(min(simpson, na.rm = TRUE), 2),
    mean_simpson = round(mean(simpson, na.rm = TRUE), 2),
    max_simpson  = round(max(simpson, na.rm = TRUE), 2),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 3. Composición de la comunidad (Especies principales)
# -------------------------------------------------------------------------

# Resumen de abundancia total por especie y edad
species_abundance <- Aves %>%
  group_by(age, sc_name) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE), .groups = 'drop')

# Top 5 especies por edad
top_species <- species_abundance %>%
  group_by(age) %>%
  slice_max(total_abundance, n = 5, with_ties = FALSE) %>%
  ungroup()

# Agrupar las no principales como "Otras"
species_abundance_top <- species_abundance %>%
  mutate(species_group = ifelse(
    paste(age, sc_name) %in% paste(top_species$age, top_species$sc_name),
    sc_name,
    "Otras"
  ))

# Abundancia relativa del Top 5 + Otras
relative_abundance <- species_abundance_top %>%
  group_by(age, species_group) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop') %>%
  group_by(age) %>%
  mutate(rel_abundance = total_abundance / sum(total_abundance))

ggplot(relative_abundance, aes(x = age, y = rel_abundance, fill = species_group)) +
  geom_col() +
  labs(x = "Edad (años)", y = "Abundancia relativa", fill = "Especies") +
  theme_minimal()


# -------------------------------------------------------------------------
# 4. PERMANOVA (Composición de la comunidad con Bray-Curtis)
# -------------------------------------------------------------------------

adonis_data <- abund_matrix %>%
  left_join(meta_sites, by = "plot_id")

abund_perm <- adonis_data %>%
  select(-plot_id, -age, -treatment)

# Matriz de distancia Bray-Curtis
dist_matrix <- vegdist(abund_perm, method = "bray")

# PERMANOVA
adonis_result <- adonis2(dist_matrix ~ age, data = adonis_data, permutations = 999)
print(adonis_result)

library(pairwiseAdonis)

# Comparaciones pareadas post-hoc
pairwise_result <- pairwise.adonis(abund_perm, adonis_data$age, p.adjust.m = "bonferroni")
print(pairwise_result)

library(vegan)
library(ggplot2)

# 1. Calcular NMDS
set.seed(123)
nmds_res <- metaMDS(abund_perm, distance = "bray", k = 2, trymax = 100)

# 2. Extraer coordenadas de los sitios
nmds_scores <- as.data.frame(scores(nmds_res, display = "sites"))
nmds_scores$age <- adonis_data$age

# 3. Graficar NMDS
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = age, fill = age)) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) +
  geom_point(size = 3, alpha = 0.8) +
  annotate("text", x = min(nmds_scores$NMDS1), y = max(nmds_scores$NMDS2),
           label = paste("Stress =", round(nmds_res$stress, 3)), hjust = 0) +
  scale_color_brewer(palette = "YlGnBu") +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    color = "Edad (años)",
    fill = "Edad (años)"
  ) +
  theme_minimal()
