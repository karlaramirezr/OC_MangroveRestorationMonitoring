library(tidyverse)

veg <- read_csv("vegetacion_2026.csv")
head(veg)
# Factor de expansión para parcelas de 10x10 m (100 m² -> 1 ha = 10,000 m² / 100 m² = 100)
factor_ha <- 100

# 1. Selección de columnas, limpieza y cálculo de Área Basal individual (g_m2)
df_proc <- veg %>% 
  select(Monitoring_Date, Age, Plot_Standard, Species, DAP, Height) %>% 
  mutate(
    DAP             = as.numeric(DAP),
    Height          = as.numeric(Height),
    Monitoring_Date = as.numeric(Monitoring_Date),
    Age             = as.numeric(Age),
    # Área basal individual en m²
    g_m2            = (pi / 40000) * (DAP^2)
  )

# 2. Resumen completo de variables dasonómicas por Parcela, Año y Edad
resumen_completo <- df_proc %>%
  group_by(Plot_Standard, Monitoring_Date, Age) %>%
  summarise(
    Densidad_arboles_plot = n(),
    Densidad_arboles_ha   = n() * factor_ha,
    DAP_promedio_cm       = mean(DAP, na.rm = TRUE),
    DAP_cuadratico_cm     = sqrt(mean(DAP^2, na.rm = TRUE)),
    Altura_promedio_m     = mean(Height, na.rm = TRUE) / 100, # cm a m
    Area_Basal_m2_plot    = sum(g_m2, na.rm = TRUE),
    Area_Basal_m2_ha      = sum(g_m2, na.rm = TRUE) * factor_ha,
    .groups = "drop"
  )

print("--- Resumen Completo por Parcela y Año ---")
print(resumen_completo)

# ----------------------------------------------------------------------
# MONITOREO 2026
# ----------------------------------------------------------------------

# 3. PASO 1: Suma de Área Basal y métricas por PARCELA y EDAD (Solo 2026)
resumen_por_parcela_2026 <- df_proc %>%
  filter(Monitoring_Date == 2026) %>%
  group_by(Plot_Standard, Age) %>%
  summarise(
    Area_Basal_parcela_m2     = sum(g_m2, na.rm = TRUE),       # m² por parcela
    Area_Basal_ha_m2          = sum(g_m2, na.rm = TRUE) * factor_ha, # m²/ha
    Densidad_parcela          = n(),                            # Árboles por parcela
    Densidad_ha               = n() * factor_ha,                # Árboles/ha
    DAP_promedio_plot         = mean(DAP, na.rm = TRUE),        # DAP promedio parcela (cm)
    Altura_promedio_plot      = mean(Height, na.rm = TRUE) / 100, # Altura promedio parcela (m)
    .groups = "drop"
  )

print("--- 1. Área Basal Total y Densidad por Parcela (2026) ---")
print(resumen_por_parcela_2026)


# 4. PASO 2: Promedio ENTRE PARCELAS por cada grupo de EDAD (Solo 2026)
resumen_por_edad_2026 <- resumen_por_parcela_2026 %>%
  group_by(Age) %>%
  summarise(
    n_parcelas                 = n_distinct(Plot_Standard),
    Area_Basal_m2_ha_promedio  = mean(Area_Basal_ha_m2, na.rm = TRUE),    # m²/ha promedio
    Area_Basal_m2_plot_promed  = mean(Area_Basal_parcela_m2, na.rm = TRUE),# m² parcela promedio
    Densidad_ha_promedio       = mean(Densidad_ha, na.rm = TRUE),         # Árboles/ha promedio
    Densidad_arboles_promedio  = mean(Densidad_parcela, na.rm = TRUE),    # Árboles/parcela promedio
    DAP_promedio_cm            = mean(DAP_promedio_plot, na.rm = TRUE),   # DAP promedio (cm)
    Altura_promedio_m          = mean(Altura_promedio_plot, na.rm = TRUE),# Altura promedio (m)
    .groups = "drop"
  )

print("--- 2. Resumen Promedio por Edad (2026) ---")
print(resumen_por_edad_2026)


# 5. Estadísticas generales del monitoreo 2026
stats_2026 <- df_proc %>%
  filter(Monitoring_Date == 2026) %>%
  summarise(
    total_registros_2026 = n(),
    n_parcelas_2026      = n_distinct(Plot_Standard)
  )

print("--- 3. Estadísticas Monitoreo 2026 ---")
print(stats_2026)

# ----------------------------------------------------------------------
# GRÁFICO 1: SOLO DATOS DE 2026
# ----------------------------------------------------------------------
grafico_2026 <- ggplot(df_largo_2026, aes(x = Age_factor, y = Valor)) +
  # bounds = c(0, Inf) evita la estimación de valores negativos en el violín
  geom_violin(bounds = c(0, Inf), trim = TRUE, alpha = 0.4, color = "darkblue") +
  geom_boxplot(width = 0.15, fill = "white", color = "black", outlier.shape = 16, alpha = 0.8) +
  geom_jitter(width = 0.05, alpha = 0.6, size = 1.8, color = "darkblue") +
  facet_wrap(~ Metrica, scales = "free_y", ncol = 2, strip.position = "left") +
  # Garantiza que el eje Y comience estrictamente en 0
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Edad",  y = NULL) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.position = "left",
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

print(grafico_2026)


# ----------------------------------------------------------------------
# GRÁFICO 2: TODOS LOS DATOS (TODOS LOS AÑOS)
# ----------------------------------------------------------------------
grafico_todos <- ggplot(df_largo_todos, aes(x = Age_factor, y = Valor)) +
  # bounds = c(0, Inf) evita la estimación de valores negativos en el violín
  geom_violin(bounds = c(0, Inf), trim = TRUE, alpha = 0.4, color = "darkgreen") +
  geom_boxplot(width = 0.15, fill = "white", color = "black", outlier.shape = 16, alpha = 0.8) +
  geom_jitter(width = 0.08, alpha = 0.6, size = 1.8, color = "darkgreen") +
  facet_wrap(~ Metrica, scales = "free_y", ncol = 2, strip.position = "left") +
  # Garantiza que el eje Y comience strictly en 0
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Distribución de Variables Dasonómicas por Edad (Todos los Datos)",
    x = "Edad (Age)",
    y = NULL,
    fill = "Edad"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.position = "left",
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

print(grafico_todos)
