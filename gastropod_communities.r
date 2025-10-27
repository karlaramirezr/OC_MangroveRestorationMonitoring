library(tidyverse)
library(vegan)
library(iNEXT)
library(ggplot2) 
library(gridExtra)
library(car)

setwd("C:/Users/Karla R/Downloads")

caracoles <- read_csv("gastropods_2025.csv")
head(caracoles)

# Matriz de especies por sitio para el año 2025 -- # 5 generos
species_matrixg <- caracoles %>%
  filter(Monitoring_Date == 2025) %>%
  select(5, 10:14)

head(species_matrixg)
curvag <- specaccum(species_matrixg[, -1])
plot(curvag)  #con todos los datos -- EL muestreo esta completo 
### Opción de estimación con # de Hill 

species_matrixdg <- species_matrixg %>%
  pivot_longer(cols = starts_with("Ab_"),
               names_to = "Species",
               values_to = "Abundance") %>%
  pivot_wider(names_from = Plot_Standard, 
              values_from = Abundance,
              values_fill = 0)

# Remove species name column — only keep abundance data
sp_matrixg_clean <- species_matrixg %>%
  column_to_rownames("Plot_Standard") %>%      # make plots row names 
  select(where(~ sum(.) > 0)) %>%                  # keep only columns with abundance > 0
  t() %>%  # transpose so species are rows
  as.data.frame()

head(sp_matrixg_clean)

Dg <- iNEXT(sp_matrixg_clean, datatype = "abundance")
plot(Dg)

## por tratamiento
species_by_age <- caracoles %>%
  filter(Monitoring_Date == 2025) %>%
  group_by(Age) %>%
  summarise(across(starts_with("Ab_"), sum, .names = "{.col}")) %>%
  ungroup() %>%
  column_to_rownames("Age") %>%    
  select(-Ab_Gast) %>%
  t() 

species_by_age

Dt <- iNEXT(species_by_age, datatype = "abundance", q=0)
plot(Dt)

##### Image for report

Dt_q0 <- iNEXT(species_by_age, datatype = "abundance",  q=0 , endpoint=150)
Dt_q1 <- iNEXT(species_by_age, datatype = "abundance",  q=1 , endpoint=150)
Dt_q2 <- iNEXT(species_by_age, datatype = "abundance",  q=2 , endpoint=150)

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

### Bien muestreado a nivel de género 

################# Abundancia por edad de restauracion

# Calcular medias y desviaciones
resumen <- caracoles %>%
 filter(Monitoring_Date == 2025)  %>% 
  group_by(Age) %>%
  summarise(
    #Ab_Gast_mean = mean(Ab_Gast, na.rm = TRUE),
    #Ab_Gast_sd   = sd(Ab_Gast, na.rm = TRUE),
    Ab_Melampus_mean = mean(Ab_Melampus, na.rm = TRUE),
    Ab_Melampus_sd   = sd(Ab_Melampus, na.rm = TRUE),
    Ab_Littoraria_mean = mean(Ab_Littoraria, na.rm = TRUE),
    Ab_Littoraria_sd   = sd(Ab_Littoraria, na.rm = TRUE),
    Ab_Cerith_mean = mean(Ab_Cerith, na.rm = TRUE),
    Ab_Cerith_sd   = sd(Ab_Cerith, na.rm = TRUE),
    Ab_Thais_mean = mean(Ab_Thais, na.rm = TRUE),
    Ab_Thais_sd   = sd(Ab_Thais, na.rm = TRUE),
    Ab_Vitta_mean = mean(Ab_Vitta, na.rm = TRUE),
    Ab_Vitta_sd   = sd(Ab_Vitta, na.rm = TRUE),
    .groups = "drop"
  )

# Convertir a formato largo
resumen_long <- resumen %>%
  pivot_longer(
    cols = -Age,
    names_to = c(".value", "Grupo"),
    names_pattern = "(.*)_(.*)"
  )

# Dividimos en medias y desviaciones estándar
mean_data <- resumen_long %>% filter(Grupo == "mean")
sd_data   <- resumen_long %>% filter(Grupo == "sd")

# Combinamos en un solo data frame (formato largo)
data_long <- mean_data %>%
  pivot_longer(cols = starts_with("Ab_"), names_to = "Especie", values_to = "Media") %>%
  left_join(
    sd_data %>% 
      pivot_longer(cols = starts_with("Ab_"), names_to = "Especie", values_to = "SD"),
    by = c("Age", "Especie")
  )

# Graficamos con ggplot2
ggplot(data_long, aes(x = Age, y = Media, color = Especie)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin =  pmax(0, Media - SD), ymax = Media + SD))+
  #geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.3, alpha = 0.7) +
  theme_minimal(base_size = 13) +
  labs(
    x = "Edad",
    y = "Abundancia (media ± SD)"
    #title = "Abundancia media de especies por edad con desviación estándar"
  ) +
  scale_color_brewer(palette = "Set1")+
  coord_cartesian(ylim = c(0, 50))


################## Analisis clasico

# Convert species richness to a dataframe
caracoles_sp <- caracoles %>%
  filter(Monitoring_Date == 2025)%>%
  select(Rich_Gast, Age) #, Plot_Standard)
  
#analysis of variance
c_aov <- aov(Rich_Gast ~ Age, data = caracoles_sp)
summary(c_aov) # Pareciaria no haber dif sig. en sp richness por cobertura

#supuestos
shapiro.test(residuals(c_aov)) # 0.0006 no normal
leveneTest(Rich_Gast ~ as.factor(Age), data = caracoles_sp) #homogenity of variances - 0.36

## since data is not normal use kruskal wallis
kruskal.test(Rich_Gast ~ as.factor(Age), data = caracoles_sp) # 0.0163 We reject the hypothesis that all groups have equal distribution of richness


