library(terra)
library(sf)
library(ggplot2)
library(tidyr)
library(openxlsx)

# Cargar capas
r1 <- rast("LandCover_2019_HNTS_Sentinel.tif")
r2 <- rast("LandCover_2026_HNTS_Sentinel.tif")
HNTS <- st_read("HNTS_costero.gpkg")

# sistema de coordenadas
crs_crtm05 <- "EPSG:5367"
HNTS_m <- st_transform(HNTS, crs_crtm05)
r1_m <- project(r1, crs_crtm05, method = "near")
r2_m <- project(r2, crs_crtm05, method = "near")

#Cortar a HNTS
r1_mask <- crop(r1_m, HNTS_m, mask = TRUE)
r2_mask <- crop(r2_m, HNTS_m, mask = TRUE) 

plot(r1_mask)
plot(r2_mask)

# // class = 0 Mangrove
#// class = 1 Fern
#// class = 3 Water
#// class = 4 Artificial
#// class = 5 Bare soil
#// class = 6 Mixed or other vegetation

#Definir nombres de clase
codigos_clase <- c("0", "1", "3", "4", "5", "6")
nombres_clase <- c("Mangrove", "Fern", "Water", "Artificial", "Bare soil", "Mixed/Other Veg")

# Matriz de transicion
matriz_pixeles <- table(values(r1_mask), values(r2_mask))

# Asignar tus nombres correspondientes a las filas (2019) y columnas (2026)
rownames(matriz_pixeles) <- nombres_clase[match(rownames(matriz_pixeles), codigos_clase)]
colnames(matriz_pixeles) <- nombres_clase[match(colnames(matriz_pixeles), codigos_clase)]
matriz_pixeles

resolucion_m     <- res(r1_mask)
area_pixel_ha    <- (resolucion_m[1] * resolucion_m[2]) / 10000
matriz_hectareas <- matriz_pixeles * area_pixel_ha

## Grafico y exportar

#matriz_df <- as.data.frame.matrix(round(matriz_hectareas, 2))
# Calcular el área total por cobertura para cada año a partir de la matriz
total_2019 <- rowSums(matriz_hectareas)
total_2026 <- colSums(matriz_hectareas)

# Crear un dataframe estructurado para graficar con ggplot2
df_grafico <- data.frame(
  Cobertura = nombres_clase,
  Año_2019  = as.numeric(total_2019[nombres_clase]),
  Año_2026  = as.numeric(total_2026[nombres_clase])
)

# Transformar el dataframe de formato ancho a formato largo (long format)
df_largo <- df_grafico %>%
  pivot_longer(cols = c(Año_2019, Año_2026), 
               names_to = "Año", 
               values_to = "Hectareas") %>%
  mutate(Año = gsub("Año_", "", Año)) # Limpiar el texto para que solo diga 2019 y 2026

#Grafico

colores_cobertura <- c("Artificial"      = "red2", 
  "Bare soil"       = "chocolate3", 
  "Mangrove"        = "#006400", 
  "Fern"            = "#FFD700", 
  "Mixed/Other Veg" = "olivedrab", 
  "Water"           = "darkblue")

#grafico_barras <- 
ggplot(df_largo, aes(x = Cobertura, y = Hectareas, fill = Cobertura, pattern = Año)) +
  geom_bar_pattern(
    stat = "identity", position = "dodge", width = 0.7, 
    color = "black", pattern_color = "black", pattern_fill = "black", 
    pattern_angle = 45, pattern_density = 0.1, pattern_spacing = 0.03
  ) +
   scale_fill_manual(values = colores_cobertura, guide = "none") +
  scale_pattern_manual(values = c("2019" = "none", "2026" = "stripe")) +
    scale_x_discrete(labels = c(
    "Bare soil"       = "Suelo desnudo",
    "Mangrove"        = "Manglar",
    "Fern"            = "Helechal",
    "Mixed/Other Veg" = "Otra vegetación",
    "Water"           = "Agua",
    "Artificial"      = "Superficies artificiales"
  )) +
  labs(
    x = "Cobertura de la tierra", y = "Hectáreas",  pattern = "Año" ) +
  theme_minimal(base_size = 12) 
