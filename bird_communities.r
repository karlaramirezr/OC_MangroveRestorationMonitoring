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

#setwd("C:/Users/YourPath")

Aves <- read_csv("aves_vff.csv")
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

Aves <- Aves %>%
  mutate(plot_parcela = str_c(plot_id, parcela, sep = "_"))

# Species per site matrix
species_matrix <- Aves %>%
  group_by(plot_parcela, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  # pivot_wider(names_from = plot_parcela,  # now columns are sites
  pivot_wider(names_from = sc_name,  # now columns are sites
    values_from = total_abundance, values_fill = 0)

head(species_matrix)
species_matrix[is.na(species_matrix)] <- 0

# No. of species -- 73
ncol(species_matrix)

################ (1)  Species accumulation curve ################
# Has the project had enough sampling? 

head(species_matrix)
curva <- specaccum(species_matrix[, -1])
plot(curva)  #con todos los datos pero esperamos que haya una comunidad diferente en negraforra vs restauracion

# Non restoration
species_matrix_NF <- Aves %>%
  filter(treatment == "NF") %>%
  group_by(plot_parcela, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  pivot_wider(names_from = sc_name, values_from = total_abundance, values_fill = 0)

head(species_matrix_NF)
#curvaNF <- specaccum(species_matrix_NF[, -1], method="rarefaction")
curvaNF <- specaccum(species_matrix_NF[, -1]) #27 sp
plot(curvaNF, main="Curva de acumulación de especies multianual en negraforra")

# Restoration
species_matrix_RF <- Aves %>%
  filter(treatment == "RF") %>%
  group_by(plot_parcela, sc_name) %>%
  summarise(total_abundance = max(abundance), .groups = 'drop') %>%
  pivot_wider(names_from = sc_name, values_from = total_abundance, values_fill = 0)

head(species_matrix_RF)
species_matrix_RF[is.na(species_matrix_RF)] <- 0
curvaRF <- specaccum(species_matrix_RF[, -1])
plot(curvaRF, main="Curva de acumulación de especies multianual en parcelas de restauración")


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

################## Analisis clasico

ambiental <- Aves %>%
  #select(plot_parcela, treatment, age)
  select(plot_parcela, age)%>%
  distinct()

# Calculate species richness for each plot
sppr <- species_matrix %>%
  column_to_rownames(var = "plot_parcela") %>%
  specnumber()

sppr

# Convert species richness to a dataframe
sppr_df <- enframe(sppr, name = "plot_parcela", value = "sppr")

# Join with environmental data (age)
sppr_df <- sppr_df %>%
  left_join(ambiental, by = "plot_parcela")

#supuestos
shapiro.test(residuals(sppr_aov)) # 0.03 no normal
leveneTest(sppr ~ as.factor(age), data = sppr_df) #homogenity of variances - 0.2

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

################ Assuptions

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


################## COMPOSITION

###################TOP 5

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

###################### TODOS

library(dplyr)
library(ggplot2)

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

############ PERMANOVA

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

